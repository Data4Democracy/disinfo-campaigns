# Quick R script to parse through the facebook ads released in May 2018 by the US House of Representatives Permanent Select Committee on Intelligence
# Just source this script into an R session.  On a 2017-vintage MacBook Pro it took about 20 minutes to run through all the files.
# The script uses `tabulizer` (the R binding of the Java tabula pdf library) which in turn requires rJava.

library(tabulizer)
library(tidyverse)
library(utils)

# Download the zips from https://democrats-intelligence.house.gov/facebook-ads/social-media-advertisements.htm into this directory
INPUT_DIR = '/opt/data/house-psci-facebook-ads/'

makeInteger <- function(s) {
  as.integer(gsub(x=s, pattern=',', replacement=''))
}

blankToNA <- function(s) {
  case_when(s=='' ~ NA_character_, TRUE ~ s)
}

fdf <- map_df(list.files(INPUT_DIR, pattern="*.zip", full.names=TRUE), function(zipfile) {
  zipFileName <- basename(zipfile)
  td <- file.path(tempdir(), zipFileName)
  writeLines(paste0('Using temporary directory: ', td))
  if(dir.exists(td)) {
    unlink(td)
  }
  dir.create(td)
  unzip(zipfile, exdir=td)
  rawFiles <- list.files(td, recursive=TRUE, full.names=TRUE)
  writeLines(paste0('Processing ', length(rawFiles), ' files in zipfile ', zipfile))
  imap_dfr(rawFiles, function(pdf, idx) {
    if (!grepl(x=pdf, pattern='empty')) {
      #writeLines(paste0('Processing pdf ', pdf))
      if (idx %% 10 == 0) writeLines(paste0('Processed pdf # ', idx))
      fileName <- gsub(x=pdf, pattern=paste0(td, '(.+)'), replacement='\\1')
      ret <- tibble()
      tryCatch({
      tt <- extract_text(pdf, pages=1) %>% gsub(x=., pattern='\n', replacement=' ')
      ret <- tibble(tt=tt) %>%
        mutate(
          AdID=gsub(x=tt, pattern='^[^0-9]+([0-9]+).+', replacement='\\1'),
          AdText=gsub(x=tt, pattern='^.*Ad Text(.+)http.+', replacement='\\1'),
          Impressions=case_when(grepl(x=tt, pattern='.+[^0-9] ([0-9,]+) ([0-9,]+).+') ~ gsub(x=tt, pattern='.+[^0-9] ([0-9,]+) ([0-9,]+).+', replacement='\\1'), TRUE ~ NA_character_),
          Clicks=case_when(grepl(x=tt, pattern='.+[^0-9] ([0-9,]+) ([0-9,]+).+') ~ gsub(x=tt, pattern='.+[^0-9] ([0-9,]+) ([0-9,]+).+', replacement='\\2'), TRUE ~ NA_character_),
          Location=gsub(perl=TRUE, x=tt, pattern='^.+Location[ ]?[\\-:] (.+)(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend).+', replacement='\\1'),
          Location=str_split(Location, '(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language)') %>% map_chr(function(i) { i[1] }),
          Age=gsub(perl=TRUE, x=tt, pattern='^.+Age: ([0-9 \\-+]+).+', replacement='\\1'),
          Placements=gsub(perl=TRUE, x=tt, pattern='^.+Placements[:]? ([^:]+).+', replacement='\\1'),
          Placements=str_split(Placements, '(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          PeopleWhoMatch=case_when(grepl(x=tt, pattern='People Who Match') ~ gsub(perl=TRUE, x=tt, pattern='^.+People Who Match: ([^:]+).+', replacement='\\1'), TRUE ~ NA_character_),
          PeopleWhoMatch=str_split(PeopleWhoMatch, '(?:Friends of connections|Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          Interests=case_when(grepl(x=tt, pattern='Interests') ~ gsub(perl=TRUE, x=tt, pattern='^.+Interests: ([^:]+).+', replacement='\\1'), TRUE ~ NA_character_),
          Interests=str_split(Interests, '(?:Friends of connections|Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          Language=case_when(grepl(x=tt, pattern='Language') ~ gsub(perl=TRUE, x=tt, pattern='^.+Language: ([^:]+).+', replacement='\\1'), TRUE ~ NA_character_),
          Language=str_split(Language, '(?:Friends of connections|Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          FriendsOfConnections=case_when(grepl(x=tt, pattern='Friends of connections') ~ gsub(perl=TRUE, x=tt, pattern='^.+Friends of connections: ([^:]+).+', replacement='\\1'), TRUE ~ NA_character_),
          FriendsOfConnections=str_split(FriendsOfConnections, '(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          ExcludedConnections=case_when(grepl(x=tt, pattern='Excluded Connections') ~ gsub(perl=TRUE, x=tt, pattern='^.+Excluded Connections: ([^:]+).+', replacement='\\1'), TRUE ~ NA_character_),
          ExcludedConnections=str_split(ExcludedConnections, '(?:Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Ad Creation Date|[0-9]+)') %>% map_chr(function(i) { i[1] }),
          AdSpend=gsub(x=tt, pattern='.+ ([0-9.,]+ RUB).+', replacement='\\1'),
          CreationDate=gsub(perl=TRUE, x=tt, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} (?:AM|PM) (?:[PDST]+)).+([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} (?:AM|PM) (?:[PDST]+)).+', replacement='\\1'),
          CreationDate=case_when(grepl(x=CreationDate, pattern='^[0-9]{2}/.+') ~ CreationDate, TRUE ~ gsub(perl=TRUE, x=tt, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} (?:AM|PM) (?:[PDST]+)).+', replacement='\\1')),
          EndDate=gsub(perl=TRUE, x=tt, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} (?:AM|PM) (?:[PDST]+)).+([0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2} (?:AM|PM) (?:[PDST]+)).+', replacement='\\2'),
          EndDate=case_when(grepl(x=EndDate, pattern='^[0-9]{2}/.+') ~ EndDate, TRUE ~ NA_character_),
        ) %>%
        mutate(
          AdText=gsub(perl=TRUE, x=AdText, pattern='(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Ad Landing Page|Ad Targeting|Ad Creation Date|Ad End Date)', replacement=''),
          Clicks=case_when(is.na(Clicks) ~ case_when(grepl(x=tt, pattern='Ad Clicks\\s+([0-9,]+)') ~ gsub(x=tt, pattern='.+Ad Clicks\\s+([0-9,]+).+', replacement='\\1'), TRUE ~ NA_character_), TRUE ~ Clicks),
          Impressions=case_when(is.na(Impressions) ~ case_when(grepl(x=tt, pattern='Ad Impressions\\s+([0-9,]+)') ~ gsub(x=tt, pattern='.+Ad Impressions\\s+([0-9,]+).+', replacement='\\1'), TRUE ~ NA_character_), TRUE ~ Impressions)
        ) %>%
        mutate_if(is.character, trimws) %>%
        mutate_if(is.character, blankToNA) %>%
        mutate_at(vars(Clicks, Impressions), makeInteger) %>%
        mutate(AdSpend=case_when(grepl(x=AdSpend, pattern='RUB') ~ suppressWarnings(as.numeric(gsub(x=gsub(x=AdSpend, pattern='(.+) RUB', replacement='\\1'), pattern=',', replacement=''))),
                                 TRUE ~ NA_real_),
               AdSpendCurrency='RUB') %>%
        mutate(AdText=gsub(x=AdText, pattern='^[0-9]+[ ]+(.+)', replacement='\\1')) %>%
        mutate(SourceFile=fileName, SourceZip=zipFileName)
      }, error = function(e) {
        writeLines(paste0('Error occurred processing pdf ', pdf, '...moving on!'))
      })
      ret
    }
  })
}) %>% select(-tt)

write_csv(fdf %>% select(-tt), 'FacebookAds.csv', na='')




