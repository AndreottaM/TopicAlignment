#Script for randomly sampling tweetIDs from df.ID
#df.ID: ea. row contains an ID from the corpus
#Column contains information on group allocation, extracted group, topic, etc

library(tidyverse)

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