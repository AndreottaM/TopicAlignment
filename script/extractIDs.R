#Script for randomly sampling tweetIDs from df.ID
#df.ID: ea. row contains an ID from the corpus
#Column contains information on group allocation, extracted group, topic, etc

library(tidyverse)
library(xml2)

dat.group <- read.delim('../out/output_k5.csv', sep = ',', header = T, colClasses = c("keywords" = "character"))
dat.ID <- read.delim('../data/data-IDs.csv', header = T, sep = ',', colClasses = c("id" = "character", "keywords" = "character"))

df.ID <- dat.ID %>%
  as.tibble %>%
  select(-endbreak) %>%
  left_join(dat.group, by = c("batch", "topic", "topicsperbatch", "keywords", "vol"))

#From df.ID, extract IDs to check with Twitter
set.seed(107409)
df.ext <- df.ID %>% 
  filter(extract >0) %>% 
  filter(!is.na(topic)) %>% 
  group_by(extract) %>% 
  sample_n(30, replace = F) %>%
  ungroup
#Write to file
df.ext %>%
  select(id) %>%
  write_delim('../out/checkid.csv', col_names = F)


checked.file <- '../data/Oct2018.csv' #location of checked IDs
tweetspergroup <- 10 #number of tweets required per group
qualname <- '../out/qualdata.csv' #file name for subset of data to be used in qual. analysis
qualname.short <- '../out/qualdata-short.csv' #short data containing only tweet information

#function for html stuff
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}


if(file.exists(checked.file)){
  dat.checked <- read_delim('../data/Oct2018.csv', delim = ',', col_names = F, col_types = list(col_character())) %>%
    pull(X1)
  dat.tweet <- read_delim('../data/tweetClimate.csv', delim = ',', col_names = F, col_types = list(col_character(), col_character(), col_character()))
  colnames(dat.tweet) <- c('id', 'tweet', 'time')
  
  df.checked <- df.ext %>%
    rowwise %>%
    mutate(exist = id %in% dat.checked) %>%
    ungroup %>%
    filter(exist == T) 
  needsmoretweets <- df.checked %>%
    group_by(extract) %>%
    summarise(n = n()) %>%
    filter(n < tweetspergroup) %>%
    nrow()
  if(needsmoretweets > 1){
    print('Not enough tweets returned from ID check. Extract & check more IDs')
  }
  else{
    df.qualset <- df.checked %>%
      group_by(extract) %>%
      sample_n(tweetspergroup, replace = F) %>%
      select(-exist) %>%
      arrange(topic, .by_group = T) %>%
      ungroup %>%
      left_join(dat.tweet, by = 'id') %>%
      rowwise() %>%
      mutate(tweet = enc2native(unescape_html(tweet))) %>%
      ungroup
    df.qualset <- df.qualset %>%
      add_column(internalid = 1:nrow(df.qualset))
    write_excel_csv(df.qualset, qualname, col_names = T)
    df.qualset %>%
      select(tweet, internalid) %>%
      sample_n(nrow(df.qualset), replace = F) %>%
      write_excel_csv(qualname.short, col_names = T)
    print(sprintf('Qualitative data set has been saved with %d tweets extracted per group', tweetspergroup))
  }
}