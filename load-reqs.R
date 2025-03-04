# required packages 

library(utils) # base
library(R.utils) # for unzipping
library(ipumsr) # for reading in IPUMS data

library(ff) # for loading IPMUS bc storage capacity is reached
library(bigmemory)
library(DBI) # for making IPUMS data into easily accesible database
library(RSQLite) # for making IPUMS data lightweight database 

library(tidyr)
library(tidyverse)