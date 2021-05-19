library(tidyr)
library(data.table)
library(rvest)
library(xml2)
library(jsonlite)
library(dplyr)
# install.packages("tidytext")
library(tidytext)
# install.packages("rlang")
library(rlang)
library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(scales)
# install.packages("textdata")
library(textdata)
library(forcats)


# getting The Guardian articles

# post IPCC report

require(httr)
get_guardian_page <- function(page_id) {
  
  
  cookies = c(
    'AWSELB' = '75B9BD811C5C032EDEF76366759629DCCB8726D7A351BD742ADC0F6765DC9B9779ABBA9CF9E4519DDF3CD336789F71716B110728D8113016EF6A68C0624AF42D85A5DDB971'
  )
  
  headers = c(
    `Connection` = 'keep-alive',
    `Upgrade-Insecure-Requests` = '1',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
    `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    `Accept-Language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7,de;q=0.6'
  )
  
  params = list(
    `order-by` = 'newest',
    `show-fields` = 'bodyText',
    `q` = 'climate change',
    `page-size` = '200',
    `page` = page_id,
    `api-key` = '857ce172-919e-4e61-a6d7-ef90264efa54',
    `from-date` = '2018-10-01',
    `to-date` = '2019-01-01',
    )
  
  res <- httr::GET(url = 'http://content.guardianapis.com/search', httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies), config = httr::config(ssl_verifypeer = FALSE))
  
  page_list <- fromJSON(content(res, 'text'))
  
  saveRDS(page_list, paste0("C:/Users/Fandurka/Google Drive/CEU/DS3/Project/post_IPCC/", "guardian_", page_id, ".rds"))
  
}

# get all content from relevant pages and save separate .rds files
lapply(1:8, get_guardian_page)

# save .rds files into a list
df <- rbindlist(lapply(list.files(path="C:/Users/Fandurka/Google Drive/CEU/DS3/Project/post_IPCC", pattern = "rds", full.names = TRUE), function(x){
  flatten(as.data.frame(readRDS(x)$response$results))
}), fill = TRUE)


df_post <- df %>% unnest(tags, names_repair = "unique")

# View(head(df_post))

df_post <- df_post %>% mutate(time_group = "post_IPCC")

# pre IPCC report

require(httr)
get_guardian_page2 <- function(page_id) {
  
  
  cookies = c(
    'AWSELB' = '75B9BD811C5C032EDEF76366759629DCCB8726D7A351BD742ADC0F6765DC9B9779ABBA9CF9E4519DDF3CD336789F71716B110728D8113016EF6A68C0624AF42D85A5DDB971'
  )
  
  headers = c(
    `Connection` = 'keep-alive',
    `Upgrade-Insecure-Requests` = '1',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36',
    `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    `Accept-Language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7,de;q=0.6'
  )
  
  params = list(
    `order-by` = 'newest',
    `show-fields` = 'bodyText',
    `q` = 'climate change',
    `page-size` = '200',
    `page` = page_id,
    `api-key` = '857ce172-919e-4e61-a6d7-ef90264efa54',
    `from-date` = '2017-10-01',
    `to-date` = '2018-01-01',
    )
  
  res <- httr::GET(url = 'http://content.guardianapis.com/search', httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies), config = httr::config(ssl_verifypeer = FALSE))
  
  page_list <- fromJSON(content(res, 'text'))
  
  saveRDS(page_list, paste0("C:/Users/Fandurka/Google Drive/CEU/DS3/Project/pre_IPCC/", "guardian_", page_id, ".rds"))
  
}



# get all content from relevant pages and save separate .rds files
lapply(1:9, get_guardian_page2)

# save .rds files into a list
df2 <- rbindlist(lapply(list.files(path="C:/Users/Fandurka/Google Drive/CEU/DS3/Project/pre_IPCC/", pattern = "rds", full.names = TRUE), function(x){
  flatten(as.data.frame(readRDS(x)$response$results))
}), fill = TRUE)


df_pre <- df2 %>% unnest(tags, names_repair = "unique")

df_pre <- df_pre %>% mutate(time_group = "pre_IPCC")

# View(head(df_pre))

# concatenate df_pre and df_post
df <- bind_rows(df_pre, df_post)


# drop variables
## api URL, contributor's profile ID & URL & api URL, references, bio, bylineimageURL, bylinelargeimageURL, first name, last name,
# twitterHandle, email, section ID, section name, isHosted, pillar ID, 

df <- df[, -c(8, 9, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)]

# rename columns

old_names <- colnames(df)
new_names <- c("id", "type", "section_id", "section_name", "publication_date", "title", "url", "contributor_type", "contributor_name", "pillar_name", "body_text", "time_group")

setnames(df, old_names, new_names)

write.csv(df, "C:/Users/fkiss/Documents/Unstructured_text_analysis/Guardian_articles_text_analysis/guardian_climate_change5.csv", row.names = FALSE)

