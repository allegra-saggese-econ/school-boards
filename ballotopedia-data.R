# script for creating and cleaning the school boards data 

# for school boards data - it is complicated, we have the following pieces of information:
# IN SOME STATES there is explicit DEM/REP endorsement (most states do not have this)
# other states are all nonpartisan, and inference can be made
  ## through reported endorsements (by users) on ballotopedia
  ## through self reported endorsements (by candidates) on ballotopedia survey
  ## through key-word selection in candidate profiles / platforms 
    ##(i.e. parent choice, mental health, racial justice, gender-based student advocacy)
  ## finally, if not possible to scrape this information - we may have to manually identify ideology from web search 

# we basically need to either use (1) historical data on elections and determine districts with
  ## certain values - then pace those moms into an ideological category, and then compare the back end -- 
  ## i.e. their cultural upbringing with those of very similar HH levels in diff districts


# OR we need to take the conflict data from Ballotopedia, found here: 
  ## https://ballotpedia.org/Conflicts_in_school_board_elections,_2021-2023
  ## and use it as an event study - a time when beliefs were truly realized, and subsequently
  ## this expression allows us to instrument over the variation, with thinking this variation is not temporary (time fixed) 