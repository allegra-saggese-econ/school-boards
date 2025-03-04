# purpose: script for cleaning IPMUS data
# author : allegra saggese

library(tidyr)
library(tidyverse)

zip_dir  <- "/Users/allegrasaggese/Desktop/research/school-boards/data-zipped" # Where GRF (spatial) data is 
data_dir <- "/Users/allegrasaggese/Desktop/research/school-boards/grf-unzipped"  # Destination folder
census_zip <- "/Users/allegrasaggese/Desktop/research/school-boards/usa_00001.dat.gz" #raw IPUMS data download 
data_dir2 <-"/Users/allegrasaggese/Desktop/research/school-boards/ipums-unzipped"

# make directory if not already set in script
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# List all ZIP files
zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

# Unzip them to the chosen directory
for (zfile in zip_files) {
  unzip(zfile, exdir = data_dir)
}

# List all files directly in 'data_dir' that start with "grf", exclude subfolders
files_loose_lower <- list.files(
  path = data_dir, 
  pattern = "^grf", 
  full.names = TRUE, 
  recursive = FALSE
)

# Iterate over these files
for (file_path in files_loose_lower) {
  # Get just the filename, e.g. "GRF81_something.dat"
  fname <- basename(file_path)
  
  # Extract the first 5 characters: "GRFXX"
  prefix <- substr(fname, 1, 5)
  
  # Create the destination folder (e.g., "data_dir/GRF81")
  dest_dir <- file.path(data_dir, prefix)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
  
  # Build the new file path in the subfolder
  new_file_path <- file.path(dest_dir, fname)
  
  # Move the file into the new subfolder
  file.rename(from = file_path, to = new_file_path)
}

# change all subfolders to be lowercase 
dirs <- list.dirs(path = data_dir, full.names = TRUE, recursive = FALSE)

for (dir_path in dirs) {
  dir_name <- basename(dir_path)  # e.g. "GRF81"
    if (grepl("(?i)^GRF", dir_name, perl = TRUE)) {
    # Convert the directory name to all lower case (e.g., "grf81")
    dir_name_lower <- tolower(dir_name)
    # Build the new, lower-cased directory path
    new_dir_path <- file.path(dirname(dir_path), dir_name_lower)
        if (dir_path != new_dir_path) {
      file.rename(from = dir_path, to = new_dir_path)
    }
  }
}

# clear enviro, make list of grf dirs for looping
rm(list = setdiff(ls(), c("zip_dir", "data_dir")))
grf_dirs <- list.dirs(path = data_dir, full.names = TRUE, recursive = FALSE)

# next step - manually review files so I understand what is in each one 
