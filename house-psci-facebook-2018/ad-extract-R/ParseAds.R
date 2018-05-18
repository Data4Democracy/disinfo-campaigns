# Quick R script to parse through the facebook ads released in May 2018 by the US House of Representatives Permanent Select Committee on Intelligence
# Just source this script into an R session.  On a 2017-vintage MacBook Pro it took about 20 minutes to run through all the files.
# The script uses `tabulizer` (the R binding of the Java tabula pdf library) which in turn requires rJava.

library(tabulizer)
library(tidyverse)
library(utils)
library(stringi)

# Download the zips from https://democrats-intelligence.house.gov/facebook-ads/social-media-advertisements.htm into this directory
INPUT_DIR = '/opt/data/house-psci-facebook-ads/'

makeInteger <- function(s) {
  as.integer(gsub(x=s, pattern=',|\\.| ', replacement=''))
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
    fileName <- gsub(x=pdf, pattern=paste0(td, '(.+)'), replacement='\\1')
    # if (fileName %in% c("/2017-03/P(1)0005551.pdf", "/2017-02/P(1)0005603.pdf", "/2017-03/P(1)0005553.pdf")) {
    if (!grepl(x=pdf, pattern='empty')) {
      #writeLines(paste0('Processing pdf ', pdf))
      if (idx %% 10 == 0) writeLines(paste0('Processed pdf # ', idx))
      ret <- tibble()
      
      tryCatch({
        
        pages <- get_n_pages(pdf)
        tt <- paste(extract_text(pdf, pages=seq(max(1, pages-1))), collapse=' ') %>% gsub(x=., pattern='\n', replacement=' ')
        
        sectionTitles <- 'Connections|Behaviors|Friends of connections|Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Language|Location|Generation|Politics|Ad Creation Date|Gender|Custom Audience'
        
        parseSection <- function(rawText, sectionTitle, forceColonAfterTitle=FALSE, useDigitsAsStopper=TRUE) {
          ret <- NA_character_
          if (grepl(x=rawText, pattern=sectionTitle)) {
            colonPart <- '[:]'
            if (!forceColonAfterTitle) {
              colonPart <- '[:]?'
            }
            ret <- gsub(perl=TRUE, x=tt, pattern=paste0('^.+', sectionTitle, colonPart, ' ([^:]+).+'), replacement='\\1')
            fullRegex <- sectionTitles
            if (useDigitsAsStopper) {
              fullRegex <- paste0(sectionTitles, '|[0-9]+')
            }
            ret <- str_split(ret, paste0('(?:', fullRegex, ')')) %>% map_chr(function(i) { i[1] })
          }
          ret <- case_when(grepl(x=ret, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ ret)
          ret
        }
        
        ret <- tibble(tt=tt) %>%
          mutate(
            AdID=gsub(x=tt, pattern='^[^0-9]+([0-9]+).+', replacement='\\1'),
            AdText=case_when(grepl(x=tt, pattern='Ad Text') ~ gsub(x=tt, pattern='^.*Ad Text(.+)http.+', replacement='\\1'), TRUE ~ NA_character_),
            Clicks=case_when(grepl(x=tt, pattern='Ad Clicks\\s+([0-9,\\. ]+)A') ~ gsub(x=tt, pattern='.+Ad Clicks\\s+([0-9,\\. ]+)A.+', replacement='\\1'), TRUE ~ NA_character_),
            Impressions=case_when(grepl(x=tt, pattern='Ad Impressions\\s+([0-9,\\. ]+)A') ~ gsub(x=tt, pattern='.+Ad Impressions\\s+([0-9,\\. ]+)A.+', replacement='\\1'), TRUE ~ NA_character_),
            Clicks=case_when(is.na(Clicks) ~ case_when(grepl(x=tt, pattern='Ad Clicks\\s+([0-9,\\.]+)') ~ gsub(x=tt, pattern='.+Ad Clicks\\s+([0-9,\\.]+).+', replacement='\\1'), TRUE ~ NA_character_), TRUE ~ Clicks),
            Impressions=case_when(is.na(Impressions) ~ case_when(grepl(x=tt, pattern='Ad Impressions\\s+([0-9,\\.]+)') ~ gsub(x=tt, pattern='.+Ad Impressions\\s+([0-9,\\.]+).+', replacement='\\1'), TRUE ~ NA_character_), TRUE ~ Impressions),
            Clicks=case_when(is.na(Clicks) & grepl(x=tt, pattern='.+Clicks.+[^0-9] ([0-9,\\.]+) ([0-9,\\.]+).+') ~ gsub(x=tt, pattern='.+Clicks.+[^0-9] ([0-9,\\.]+) ([0-9,\\.]+).+', replacement='\\2'), TRUE ~ Clicks),
            Impressions=case_when(is.na(Impressions) & grepl(x=tt, pattern='.+Impressions.+[^0-9] ([0-9,\\.]+) ([0-9,\\.]+).+') ~ gsub(x=tt, pattern='.+Impressions.+[^0-9] ([0-9,\\.]+) ([0-9,\\.]+).+', replacement='\\1'), TRUE ~ Impressions),
            Clicks=case_when(is.na(Clicks) & grepl(x=tt, pattern=' ([0-9,\\.]+) ([0-9,\\.]+) ([0-9,\\.]+) (RUB|USD).+') ~ gsub(x=tt, pattern='.+ ([0-9,\\.]+) ([0-9,\\.]+) ([0-9,\\.]+) (RUB|USD).+', replacement='\\2'), TRUE ~ Clicks),
            Impressions=case_when(is.na(Impressions) & grepl(x=tt, pattern=' ([0-9,\\.]+) ([0-9,\\.]+) ([0-9,\\.]+) (RUB|USD).+') ~ gsub(x=tt, pattern='.+ ([0-9,\\.]+) ([0-9,\\.]+) ([0-9,\\.]+) (RUB|USD).+', replacement='\\1'), TRUE ~ Impressions),
            Age=gsub(perl=TRUE, x=tt, pattern='^.+Age: ([0-9 \\-+]+).+', replacement='\\1'),
            Age=case_when(grepl(x=Age, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ Age),
            AdSpend=gsub(x=tt, pattern='.+ ([0-9.,]+ RUB).+', replacement='\\1'),
            ttd=gsub(x=tt, pattern='[ ]*/[ ]*', replacement='/'),
            ttd=gsub(x=ttd, pattern='[ ]*:[ ]*', replacement=':'),
            ttd=gsub(x=ttd, pattern='0&', replacement=''),
            CreationDate=gsub(perl=TRUE, x=ttd, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+)).+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+)).+', replacement='\\1'),
            CreationDate=case_when(grepl(x=CreationDate, pattern='^[0-9]{2}/.+') ~ CreationDate, TRUE ~ gsub(perl=TRUE, x=tt, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+)).+', replacement='\\1')),
            CreationDate=case_when(grepl(x=CreationDate, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ CreationDate),
            CreationDate=case_when(is.na(CreationDate) & !grepl(perl=TRUE, pattern='(?:P[DS]T).+(?:P[DS]T)', x=ttd) ~ gsub(x=ttd, perl=TRUE, pattern='.+(?!P[DS]T)+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+))[ ]+P.+', replacement='\\1'),
                                   TRUE ~ CreationDate),
            CreationDate=case_when(grepl(x=CreationDate, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ CreationDate),
            EndDate=gsub(perl=TRUE, x=ttd, pattern='.+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+)).+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+)).+', replacement='\\2'),
            EndDate=case_when(grepl(x=EndDate, pattern='^[0-9]{2}/.+') ~ EndDate, TRUE ~ NA_character_),
            EndDate=case_when(grepl(x=EndDate, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ EndDate),
            EndDate=case_when(is.na(EndDate) & !grepl(perl=TRUE, pattern='(?:P[DS]T).+(?:P[DS]T)', x=ttd) ~ gsub(x=ttd, perl=TRUE, pattern='.+(?!P[DS]T)+([0-9]{2}/[0-9]{2}/[0-9]{2}[ ]?[0-9]{2}[:\\-][0-9]{2}[:\\-][0-9]{2}[ ]?(?:AM|PM) (?:[PDST]+))[ ]+P.+', replacement='\\2'),
                                   TRUE ~ EndDate),
            EndDate=case_when(grepl(x=EndDate, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ EndDate),
          ) %>% mutate(
            Location=gsub(perl=TRUE, x=tt, pattern=paste0('^.+Location[ ]?[\\-:] (.+)(?:', sectionTitles, ').+'), replacement='\\1'),
            Location=str_split(Location, paste0('(?:', sectionTitles, ')')) %>% map_chr(function(i) { i[1] }),
            Location=case_when(grepl(x=Location, pattern='Ad ID|Ad Landing Page') ~ NA_character_, TRUE ~ Location)
          ) %>% mutate(
            Behaviors=parseSection(tt, 'Behaviors'),
            Placements=parseSection(tt, 'Placements'),
            PeopleWhoMatch=parseSection(tt, 'People Who Match'),
            Interests=parseSection(tt, 'Interests', useDigitsAsStopper=FALSE),
            Language=parseSection(tt, 'Language'),
            FriendsOfConnections=parseSection(tt, 'Friends of connections'),
            ExcludedConnections=parseSection(tt, 'Excluded Connections'),
            Gender=parseSection(tt, 'Gender', forceColonAfterTitle = TRUE),
            Generation=parseSection(tt, 'Generation', forceColonAfterTitle = TRUE),
            Politics=parseSection(tt, 'Politics', forceColonAfterTitle = TRUE),
            CustomAudience=parseSection(tt, 'Custom Audience', forceColonAfterTitle = TRUE, useDigitsAsStopper=FALSE),
          ) %>% mutate(
            Interests=gsub(x=Interests, pattern='P\\(1\\).+$', replacement=''),
            Interests=gsub(x=Interests, pattern='P\\(1\\).+$', replacement=''),
            Interests=gsub(x=Interests, pattern='P\\(1\\).+$', replacement=''),
            Interests=stri_reverse(gsub(x=stri_reverse(gsub(x=Interests, pattern='RUB|USD|R U B|None[0-9 /]+$', replacement='')), pattern="^([0-9 \\.,/]+)(.+)", replacement = '\\2')),
          ) %>% mutate(
            AdText=gsub(perl=TRUE, x=AdText, pattern='(?:Excluded Connections|Placements|Age|Interests|People Who Match|Ad Impressions|Ad Clicks|Ad Spend|Ad Landing Page|Ad Targeting|Ad Creation Date|Ad End Date)', replacement=''),
          ) %>%
          mutate_if(is.character, trimws) %>%
          mutate_if(is.character, blankToNA) %>%
          mutate_at(vars(Clicks, Impressions), makeInteger) %>%
          mutate(AdSpendp=case_when(grepl(x=AdSpend, pattern='RUB|USD') ~ suppressWarnings(as.numeric(gsub(x=gsub(x=AdSpend, pattern='(.+) (RUB|USD)', replacement='\\1'), pattern=',', replacement=''))),
                                    TRUE ~ NA_real_),
                 AdSpendCurrency=case_when(grepl(x=AdSpend, pattern='RUB') ~ 'RUB', grepl(x=AdSpend, pattern='USD') ~ 'USD', TRUE ~ NA_character_)) %>%
          select(-AdSpend) %>% rename(AdSpend=AdSpendp) %>%
          mutate(AdText=gsub(x=AdText, pattern='^[0-9]+[ ]+(.+)', replacement='\\1')) %>%
          mutate(SourceFile=fileName, SourceZip=zipFileName, pages=pages)
      }, error = function(e) {
        writeLines(paste0('Error occurred processing pdf ', pdf, '...moving on!'))
        print(e)
      })
      ret
    }
  })
}) # %>% select(-tt)

#write_csv(fdf %>% select(-tt), 'FacebookAds.csv', na='')
#dwapi::configure(Sys.getenv("DATA_WORLD_RW_API_KEY"))
#dwapi::upload_data_frame("scottcame/us-house-psci-social-media-ads", fdf %>% select(-tt, -ttd), 'FacebookAds.csv')




