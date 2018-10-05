library(tidyverse)

datapath <- "../data/" #path to data files
datafiles <- list.files(path = datapath) %>%
  subset(grepl("rawtweet_5000_K", .)) #data files

lapply(datafiles, function(x){
  y <- paste0(datapath, x) %>%
    read.csv(header = F, fill = T, comment.char = "", stringsAsFactors = F) %>%
    as.tibble() #tibble of data from NMijF algorithm
  
  k <- pull(y[1,1]) %>%
    strsplit(., split = "[;]") %>%
    unlist() %>%
    .[6] %>%
    as.integer()
  #topics per batch
  
  batchnum <- dim(y)[1]
  tibble(
    topic = 1:(k*batchnum), #total number of topics = topics per batch * number of batches
    topicinbatch = rep(1:k, batchnum), #represents unique topic in batch
    batch = rep(1:batchnum, each = k),
    topicsperbatch = k
  ) %>%
    group_by(batch) %>%
    mutate(endbreak = which(grepl("]", y[batch, ]))[topicinbatch]) %>% #element where last tweetID is located for that topic
    mutate(keywords = y[batch, max(endbreak)]  %>%
             pull  %>% 
             strsplit("[;]") %>% 
             unlist %>% 
             .[2] %>% 
             strsplit("[&]") %>% 
             unlist #pulls apart keywords
    ) %>%
    ungroup() %>%
    mutate(vol = if_else(topicinbatch == 1, endbreak, endbreak - lag(endbreak))) %>% #volume of tweets in each topic
    select(-topicinbatch) %>%
    return()
}) %>%
  {do.call(rbind, .)} %>% #bind all dataframes for all k together
  write_delim(path = paste0(datapath, "data.csv"), delim = ",")






