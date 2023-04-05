library(readr)
library(DBI)
library(dplyr)
library(googlesheets4)

## Loading necessary W1 question Metadata
meta <- read_sheet('https://docs.google.com/spreadsheets/d/1KGqV2M0Ud5raOeJaVz2aoarkcsXSvp6Kk42SGxoGjAw/edit?usp=sharing')
## May need to set up authorisation at this step


## Loading Cleaning function, from Poltrack github
source("https://raw.githubusercontent.com/lisamerten/poltrack/main/preprocessing/multiple_survey_entry_cleaner.R?token=GHSAT0AAAAAAB7X522DOL66XOZROTBPUJY4ZBNMEBQ")

## Connecting to server
con_tracking <- dbConnect(RPostgres::Postgres(),dbname = 'poltrack', host = '134.102.58.175', port = 5432, user = 'poltrack_read', password = Sys.getenv("poltrack_pwd"))
participants <- dbGetQuery(con_tracking, 'SELECT * FROM "survey" WHERE LENGTH(p_0002) = 16 AND  v_2122 = 1')



## Applying Function to filter entries by multiple participants
survey_clean <- survey_cleaner(participants, 'p_0002', 'dateteime', meta)