library(ipumsr)
library(DBI)
library(RSQLite)
library(data.table)

source(here::here("_setup.R"))

# Downloads IPUMS USA extracts #4 and #5 and builds the project database.
# RUN FIRST. Requires IPUMS_API_KEY in .Renviron (gitignored).
# Output : data/interim/ipums_bkp.sqlite, with two tables:
#            ipums_table  <- extract #4 (the main person file)
#            wks_supp     <- extract #5 (weeks worked; see EXTRACT v5 below)

# =========================================================
# Build the BKP replication database from IPUMS extract v4
#
# Downloads IPUMS USA extract #4 and loads it into a SQLite database.
#
# WHY A SEPARATE DATABASE: this writes to ipums_bkp.sqlite, NOT the existing
# 42GB ipums_data.sqlite. The old database stays untouched so every existing
# script (ipums-county-household-analysis.R, t2/t2-rdd-breadwinner-norm.R,
# ipums-married-household-suite.R) keeps working unchanged
# while the BKP track moves to the new data. Once the new database is
# validated, the old one is redundant and can be deleted to reclaim ~39GB —
# but that is a manual decision, not something this script does.
#
# EXTRACT v4 CONTENTS (no overlapping samples — every person appears once):
#   - Decennial, highest-quality sample available per year:
#       1970: Form 1 State + Form 2 State (us1970a + us1970b). These are two
#             different questionnaire forms, so they do NOT overlap and combine
#             to ~2%. (The Metro/Neighborhood variants are the SAME records
#             recoded to different geography — including them would duplicate.)
#       1980, 1990, 2000: the 5% samples, the largest IPUMS offers.
#     These are the only decennials with income data: the 2010 and 2020
#     censuses were short-form only (no income, employment, or education), so
#     the ACS carries the series from 2001 on.
#   - ACS 1-year 2001-2024, every year.
#   - Deliberately EXCLUDES the ACS 3-year and 5-year products. BKP's Figure 1
#     used the 2008-2010 ACS 3-year aggregate, but that file contains the same
#     respondents as the 2008/2009/2010 1-year files, so including both would
#     double-count. We stack the 1-year files instead (documented deviation —
#     BKP's use of the 3-year file reflected the data vintage available to
#     them, not a property we need to reproduce).
#
# KEY VARIABLES ADDED vs. the old extract:
#   INCBUS / INCFARM (pre-2000) and INCBUS00 (2000+) -> lets us build BKP's
#     actual "labor income" = wage + self-employment, instead of wage-only.
#   HISPAN -> lets us match BKP's white/Black/Hispanic marriage-market split.
#   MARST, MARRNO, YRMARR, DIVINYR -> marriage timing; YRMARR in particular
#     supports BKP's "relative income at marriage" controls (Table 2 cols 11-12).
#   INCRETIR, plus allocation flags on the income variables (for the
#     Murray-Close & Heggeness misreporting diagnostic).
#   YNGCH, ELDCH, GQ, METRO, MULTGEN, PERWT -> NOT needed by the BKP scripts,
#     but required by the existing pipeline (ipums-county-household-analysis.R
#     reads YNGCH for its child-status LFPR panel and filters on GQ). They are
#     included so this database is a COMPLETE replacement for the old one and
#     the old 42GB file can be retired without breaking anything.
#
# EXTRACT v5 CONTENTS (the weeks-worked supplement):
#   WKSWORK1 (weeks worked last year, continuous) and WKSWORK2 (the same in
#   intervals) for the SAME 29 samples as extract v4 — 1970 Forms 1 and 2,
#   1980, 1990, 2000, and ACS 2001-2024.
#
#   WHY A SECOND EXTRACT: annual hours = UHRSWORK x weeks worked, and v4
#   carries WKSWORK1 but NOT WKSWORK2. That matters because WKSWORK1 is not
#   coded in every sample. Verified against the v5 file: 1970 has WKSWORK1
#   entirely missing and only WKSWORK2 populated. ipums-model-data.R
#   additionally documents that the ACS asked weeks in brackets over 2008-2018,
#   so WKSWORK2 carries the measure there too — eleven of the model's 27 years.
#   Without WKSWORK2 those years would have NA weeks, hence NA annual hours,
#   and would silently drop out of the model panel. weeks_worked() in
#   functions.R prefers WKSWORK1 and falls back to WKSWORK2; the validation in
#   section 5b prints which variable actually carries each year, so the splice
#   stays evidence-based rather than assumed.
#
#   v5 re-pulls WKSWORK1 as well, so the join carries both variables and
#   weeks_worked() reads a single source rather than straddling two tables.
#
#   Only 6 of v5's 12 variables are kept (YEAR, SAMPLE, SERIAL, PERNUM,
#   WKSWORK1, WKSWORK2). The rest — CBSERIAL, CLUSTER, GQ, HHWT, PERWT,
#   STRATA — are IPUMS defaults that duplicate columns already in ipums_table,
#   and dropping them roughly halves the size of wks_supp.
#
# DISK: the downloads are ~4 GB and the resulting database ~15 GB.
# The script refuses to start if free space looks insufficient.
# =========================================================

collection    <- "usa"
main_extract  <- 4L      # person file          -> ipums_table
wks_extract   <- 5L      # weeks-worked supplement -> wks_supp
chunk_size    <- 100000L
min_free_gb   <- 18      # refuse to start below this (both tables, no indexes)

interim_dir  <- data_path("interim")
db_path      <- file.path(interim_dir, "ipums_bkp.sqlite")

# Each extract downloads to its own directory so a partial file for one can
# never be mistaken for the other's.
extract_dir <- function(n) file.path(interim_dir, paste0("ipums_extract_v", n))
invisible(lapply(c(main_extract, wks_extract), function(n) ensure_dir(extract_dir(n))))

# ── 0) Preflight: disk space ──────────────────────────────────────────────
free_gb <- tryCatch({
  df <- system2("df", c("-g", shQuote(interim_dir)), stdout = TRUE)
  as.numeric(strsplit(trimws(df[2]), "\\s+")[[1]][4])
}, error = function(e) NA_real_)

message("Free space on data volume: ",
        if (is.na(free_gb)) "unknown" else paste0(free_gb, " GB"))
if (!is.na(free_gb) && free_gb < min_free_gb) {
  stop("Only ", free_gb, " GB free; need ~", min_free_gb, " GB for the two ",
       "downloads plus the database build. Free up space first, then re-run — ",
       "already-downloaded extracts are reused, so a re-run does not repeat ",
       "any download that completed.")
}

# ── 1) API key ─────────────────────────────────────────────────────────────
# Read from .Renviron (gitignored). Never hard-code the key in this script.
api_key <- Sys.getenv("IPUMS_API_KEY")
if (!nzchar(api_key)) {
  stop("IPUMS_API_KEY not set. Add it to .Renviron (which is gitignored) as:\n",
       "  IPUMS_API_KEY=your_key_here\n",
       "then restart R so it is picked up.")
}
set_ipums_api_key(api_key)

# ── 2) Fetch both extracts ────────────────────────────────────────────────
# One function for both, so the weeks supplement gets exactly the same
# integrity checking as the main person file.
#
# Reuse an existing download ONLY if the gzip is intact. A partial .dat.gz is
# the dangerous case: it looks like a valid file to list.files(), and loading it
# would silently produce a table missing an arbitrary tail of the data.
# (This happened on the first attempt: the IPUMS download died on an HTTP/2
# PROTOCOL_ERROR at 2.66GB, leaving a truncated file behind.)
gz_is_intact <- function(path) {
  isTRUE(tryCatch(
    system2("gzip", c("-t", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0L,
    error = function(e) FALSE
  ))
}

fetch_extract <- function(extract_num) {
  dir <- extract_dir(extract_num)
  message("Checking IPUMS extract ", collection, ":", extract_num, " ...")

  dat_files <- list.files(dir, pattern = "\\.dat\\.gz$", full.names = TRUE)
  xml_files <- list.files(dir, pattern = "\\.xml$",     full.names = TRUE)

  if (length(dat_files) >= 1 && length(xml_files) >= 1 && gz_is_intact(dat_files[1])) {
    message("  Files already present and gzip verified — skipping download.")
  } else {
    if (length(dat_files) >= 1) {
      message("  Found a .dat.gz that fails its gzip integrity check (truncated ",
              "download). Resume it, or delete it and re-run:")
      message("    ", dat_files[1])
      stop("Refusing to load a truncated extract file.")
    }
    info <- get_extract_info(c(collection, extract_num))
    message("  status: ", info$status)
    if (!identical(info$status, "completed")) {
      message("  Extract not ready; waiting (checks every 60s, up to 2 hours) ...")
      info <- wait_for_extract(c(collection, extract_num),
                               initial_delay_seconds = 30,
                               max_delay_seconds     = 60,
                               timeout_seconds       = 7200)
    }
    if (!identical(info$status, "completed")) {
      stop("Extract ", extract_num, " did not complete (status: ", info$status, ").")
    }
    message("  Downloading extract to ", dir, " ...")
    download_extract(info, download_dir = dir, overwrite = FALSE)
    dat_files <- list.files(dir, pattern = "\\.dat\\.gz$", full.names = TRUE)
    xml_files <- list.files(dir, pattern = "\\.xml$",     full.names = TRUE)
    if (length(dat_files) < 1 || !gz_is_intact(dat_files[1])) {
      stop("Download completed but the gzip is not intact — retry (the IPUMS ",
           "endpoint can drop HTTP/2 connections on large files; resuming with ",
           "`curl --http1.1 -C -` is reliable).")
    }
    message("  Download verified.")
  }

  if (length(xml_files) < 1) stop("No DDI (.xml) file found in ", dir)
  message("  DDI: ", basename(xml_files[1]))
  xml_files[1]
}

ddi_main <- fetch_extract(main_extract)
ddi_wks  <- fetch_extract(wks_extract)

# ── 3) Load into SQLite, chunked ──────────────────────────────────────────
# Chunked so memory stays bounded regardless of extract size — either extract
# is far too large to hold in R at once.
if (file.exists(db_path)) {
  stop("Database already exists: ", db_path, "\n",
       "Delete it first if you intend to rebuild from scratch.")
}

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA journal_mode = OFF")   # no rollback journal: faster, less disk
dbExecute(con, "PRAGMA synchronous = OFF")

# One loader for both extracts.
#
# ROW ORDER MATTERS. Rows land in rowid order = file order, and IPUMS writes
# records grouped by sample, so each YEAR occupies a contiguous rowid block.
# Every reader exploits this: it looks up MIN/MAX rowid per year once, then
# scans that range with NOT INDEXED (measured ~290x faster than an index seek
# on this database — see the note in t1/t1-replication.R). Correctness does not
# depend on the ordering, because those queries also filter on YEAR; only speed
# does. Do not sort or re-insert rows here.
#
# keep_cols subsets each chunk before it is written. Passing it also to
# read_ipums_micro_chunked() avoids parsing the unwanted fixed-width columns in
# the first place; the callback subset is the belt-and-braces guarantee, since
# a column that slipped through would silently bloat the table.
load_extract <- function(ddi_path, table, keep_cols = NULL) {
  ddi <- read_ipums_ddi(ddi_path)
  n_written <- 0L

  cb <- function(chunk_df, pos) {
    chunk_df <- as.data.frame(chunk_df)
    if (!is.null(keep_cols)) {
      missing <- setdiff(keep_cols, names(chunk_df))
      if (length(missing)) {
        stop("Extract is missing expected column(s): ", paste(missing, collapse = ", "))
      }
      chunk_df <- chunk_df[, keep_cols, drop = FALSE]
    }
    # Strip IPUMS labelled-vector attributes; store plain values for SQLite.
    chunk_df[] <- lapply(chunk_df, function(x) {
      if (inherits(x, "haven_labelled")) as.numeric(x) else x
    })
    dbWriteTable(con, table, chunk_df, append = TRUE, row.names = FALSE)
    n_written <<- n_written + nrow(chunk_df)
    if (pos %% 2000000 < chunk_size) {
      message("    ... ", format(n_written, big.mark = ","), " rows")
    }
    NULL
  }

  message("Loading ", basename(ddi_path), " -> ", table, " (chunked) ...")
  # var_attrs = NULL: don't attach IPUMS value labels. We store plain numeric
  # codes in SQLite, so building labelled vectors would only cost time/memory.
  args <- list(ddi, callback = IpumsSideEffectCallback$new(cb),
               chunk_size = chunk_size, verbose = FALSE, var_attrs = NULL)
  # tidyselect::all_of() — passing a bare character vector is deprecated and
  # warns on every chunk.
  if (!is.null(keep_cols)) args$vars <- tidyselect::all_of(keep_cols)
  do.call(read_ipums_micro_chunked, args)
  message("  Wrote ", format(n_written, big.mark = ","), " records to ", table, ".")
  n_written
}

n_main <- load_extract(ddi_main, "ipums_table")

# ── 3b) Weeks worked ──────────────────────────────────────────────────────
# Extract v5, same 29 samples as v4. Six columns only — see EXTRACT v5 in the
# header for why the other six IPUMS defaults are dropped, and why WKSWORK2
# is the variable that makes this extract necessary.
#
# This table is a hard dependency of ipums-model-data.R, which builds
# model_input_households.csv (the input to the whole T3 model track). It joins
# on SAMPLE/SERIAL/PERNUM to get annual hours = UHRSWORK x weeks worked.
n_wks <- load_extract(ddi_wks, "wks_supp",
                      keep_cols = c("YEAR", "SAMPLE", "SERIAL", "PERNUM",
                                    "WKSWORK1", "WKSWORK2"))

# ── 4) Indexes: deliberately none ─────────────────────────────────────────
# This database carries no indexes, and that is a measured decision rather than
# an omission.
#
# Every reader pulls whole year-blocks, not individual rows. For that access
# pattern a sequential rowid-range scan beats an index seek by ~290x on this
# database, so all of T1, T2 and T3 pass NOT INDEXED to force the planner off
# any index that exists. An earlier build created an index on (YEAR, AGE, SEX)
# and another on the wks_supp key; between them they cost 3.8 GB and were used
# by nothing except the one-off MIN/MAX rowid setup query at the top of each
# script, which is a full scan either way and runs once per script.
#
# DO NOT add an index on the spouse-join keys expecting it to help: with an
# age index present, SQLite's planner will use it for BOTH sides of a spouse
# self-join and nested-loop the match (measured: hours per year). The scripts
# deliberately pull each side separately and join in R instead.
message("\nNo indexes created (by design — see comment above).")

# ── 5) Validate ───────────────────────────────────────────────────────────
message("\nRecords by year:")
yr <- setDT(dbGetQuery(con, "SELECT YEAR, COUNT(*) AS n FROM ipums_table GROUP BY YEAR ORDER BY YEAR"))
print(yr)

message("\nChecking the new income variables are populated:")
for (v in c("INCWAGE", "INCBUS", "INCFARM", "INCBUS00", "HISPAN", "MARST")) {
  ok <- tryCatch({
    q <- dbGetQuery(con, paste0(
      "SELECT COUNT(*) AS n_nonmissing FROM ipums_table WHERE ", v, " IS NOT NULL"))
    paste0(format(q$n_nonmissing, big.mark = ","), " non-null")
  }, error = function(e) paste0("COLUMN MISSING (", conditionMessage(e), ")"))
  message("  ", formatC(v, width = 10), ": ", ok)
}

# Is the 1970 Form 2 sample actually usable, or does it lack income variables?
# The analysis defaults to Form 1 only (see pull_person_side()); this reports
# whether pooling would be safe, so that stays an evidence-based choice rather
# than an assumption. If Form 2 shows income coverage comparable to Form 1,
# pooling is available as a one-line change (plus halving 1970 weights).
message("\n1970 form comparison — income coverage by sample:")
print(dbGetQuery(con, "
  SELECT SAMPLE,
         COUNT(*)                                                   AS n_records,
         SUM(CASE WHEN INCWAGE IS NOT NULL THEN 1 ELSE 0 END)       AS incwage_nonnull,
         SUM(CASE WHEN INCBUS  IS NOT NULL THEN 1 ELSE 0 END)       AS incbus_nonnull,
         SUM(CASE WHEN INCFARM IS NOT NULL THEN 1 ELSE 0 END)       AS incfarm_nonnull,
         SUM(CASE WHEN EDUC    IS NOT NULL THEN 1 ELSE 0 END)       AS educ_nonnull
  FROM ipums_table WHERE YEAR = 1970 GROUP BY SAMPLE ORDER BY SAMPLE"))
message("  (Analysis uses Form 1 = SAMPLE 197001. If Form 2 coverage is")
message("   comparable, pooling to ~2% is available — see pull_person_side().)")

message("\nSanity check — 2000 decennial present? (was missing from the old extract)")
print(dbGetQuery(con, "SELECT COUNT(*) AS n_2000 FROM ipums_table WHERE YEAR = 2000"))
message("Sanity check — 2024 present? (extends the panel past the old 2023 end)")
print(dbGetQuery(con, "SELECT COUNT(*) AS n_2024 FROM ipums_table WHERE YEAR = 2024"))

# ── 5b) Validate wks_supp ─────────────────────────────────────────────────
# The weeks table is joined on SAMPLE/SERIAL/PERNUM. A wrong or partial key
# would not error — it would silently produce NA weeks, and therefore NA annual
# hours, for the affected rows. So check the join explicitly rather than
# assuming it, and check it on the sample the model actually uses.

message("\nwks_supp: record counts against ipums_table, by year")
cmp <- setDT(dbGetQuery(con, "
  SELECT y.YEAR, y.n_main, COALESCE(w.n_wks, 0) AS n_wks
  FROM      (SELECT YEAR, COUNT(*) n_main FROM ipums_table GROUP BY YEAR) y
  LEFT JOIN (SELECT YEAR, COUNT(*) n_wks  FROM wks_supp    GROUP BY YEAR) w
         ON y.YEAR = w.YEAR
  ORDER BY y.YEAR"))
cmp[, diff := n_wks - n_main]
print(cmp)
if (nrow(cmp[n_wks == 0])) {
  stop("wks_supp is missing these years entirely: ",
       paste(cmp[n_wks == 0, YEAR], collapse = ", "),
       "\nipums-model-data.R will fail on them. Check the v5 sample selection.")
}
if (nrow(cmp[diff != 0])) {
  warning("wks_supp and ipums_table disagree on record counts in: ",
          paste(cmp[diff != 0, YEAR], collapse = ", "),
          ". The two extracts should cover identical samples — investigate ",
          "before trusting annual hours in those years.")
}

# Verify the join key itself. A full SQL join is not an option here: the
# database has no indexes, so SQLite would nested-loop 109M rows. Two linear
# checks instead.
#
# (a) Per-year checksums of the key columns. If the two extracts really cover
#     identical samples, these agree exactly; a mismatch means the sample
#     selections diverged. One sequential pass per table, no join.
message("\nwks_supp: key checksums against ipums_table")
ck <- setDT(dbGetQuery(con, "
  SELECT y.YEAR,
         y.s_serial = w.s_serial AND y.s_pernum = w.s_pernum AS keys_agree
  FROM (SELECT YEAR, SUM(SERIAL) s_serial, SUM(PERNUM) s_pernum
          FROM ipums_table NOT INDEXED GROUP BY YEAR) y
  JOIN (SELECT YEAR, SUM(SERIAL) s_serial, SUM(PERNUM) s_pernum
          FROM wks_supp    NOT INDEXED GROUP BY YEAR) w
    ON y.YEAR = w.YEAR
  ORDER BY y.YEAR"))
bad <- ck[keys_agree != 1L, YEAR]
if (length(bad)) {
  stop("SERIAL/PERNUM checksums differ between the extracts in: ",
       paste(bad, collapse = ", "),
       "\nThe two extracts do not cover the same records. Do not build the ",
       "model panel until this is resolved.")
}
message("  all ", nrow(ck), " years agree on SUM(SERIAL) and SUM(PERNUM).")

# (b) An actual row-level join, done in R on one bounded slice per era so the
#     cost stays trivial. This is what catches a key that is well-formed but
#     mismatched — checksums alone could in principle agree by coincidence.
for (yr in c(1980L, 2019L)) {
  rm_ <- dbGetQuery(con, paste0(
    "SELECT MIN(rowid) lo FROM ipums_table NOT INDEXED WHERE YEAR = ", yr))$lo
  rw_ <- dbGetQuery(con, paste0(
    "SELECT MIN(rowid) lo FROM wks_supp    NOT INDEXED WHERE YEAR = ", yr))$lo
  slice <- 200000L
  a <- setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE, SERIAL, PERNUM FROM ipums_table NOT INDEXED
      WHERE rowid BETWEEN ", rm_, " AND ", rm_ + slice, " AND YEAR = ", yr)))
  b <- setDT(dbGetQuery(con, paste0(
    "SELECT SAMPLE, SERIAL, PERNUM, WKSWORK1, WKSWORK2 FROM wks_supp NOT INDEXED
      WHERE rowid BETWEEN ", rw_, " AND ", rw_ + slice, " AND YEAR = ", yr)))
  setkey(a, SAMPLE, SERIAL, PERNUM); setkey(b, SAMPLE, SERIAL, PERNUM)
  matched <- sum(!is.na(b[a, which = TRUE]))
  pct <- 100 * matched / nrow(a)
  message("  ", yr, ": ", format(matched, big.mark = ","), " of ",
          format(nrow(a), big.mark = ","), " sampled rows matched (",
          sprintf("%.2f%%", pct), ")")
  if (is.finite(pct) && pct < 99) {
    stop("Join coverage below 99% in ", yr, ". SAMPLE/SERIAL/PERNUM is not ",
         "matching between the two extracts.")
  }
}

# WKSWORK1 is the continuous measure; WKSWORK2 is intervalled and is all that
# is coded in the earlier samples. weeks_worked() in functions.R prefers
# WKSWORK1 and falls back to WKSWORK2, with weeks_is_imputed() flagging the
# fallback. This reports where the splice actually bites.
message("\nWeeks coverage by year (which variable carries the measure):")
print(setDT(dbGetQuery(con, "
  SELECT YEAR,
         SUM(CASE WHEN WKSWORK1 BETWEEN 1 AND 52 THEN 1 ELSE 0 END) AS has_wkswork1,
         SUM(CASE WHEN WKSWORK2 > 0              THEN 1 ELSE 0 END) AS has_wkswork2
  FROM wks_supp GROUP BY YEAR ORDER BY YEAR")))

# Rowid blocks must stay one-per-year for the range-scan reads to be fast.
# Correctness does not depend on this (the readers also filter on YEAR), so
# report rather than stop.
for (tb in c("ipums_table", "wks_supp")) {
  rr <- setDT(dbGetQuery(con, paste0(
    "SELECT YEAR, MIN(rowid) lo, MAX(rowid) hi FROM ", tb, " GROUP BY YEAR ORDER BY lo")))
  overlap <- if (nrow(rr) > 1) sum(rr$lo[-1] <= rr$hi[-nrow(rr)]) else 0L
  message("  ", formatC(tb, width = 12), ": ", nrow(rr), " year-blocks, ",
          overlap, " overlapping",
          if (overlap > 0) "  <- reads will be slower than expected" else "")
}

message("\nDatabase built: ", db_path)
message("Size: ", round(file.size(db_path) / 1024^3, 2), " GB")
message("Tables: ipums_table (", format(n_main, big.mark = ","), " records), ",
        "wks_supp (", format(n_wks, big.mark = ","), " records)")
message("\nNext: ipums-model-data.R joins the two tables to build")
message("data/processed/panel/model_input_households.csv, the input to T3.")
