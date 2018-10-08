#Packages
library(tidyverse)

#Code is based on Chuang's algorithm, but instead of cosine similarity of topic words, we use keywords.
#First calculate similarity between topics
#Second group similar topics
#Third display this similarity


###
#Interim ouput commands
###
#Code takes a while to run, interim output is saved to improve rerun speeds
clear.sim <- F #change to T to delete saved similarity calculations
clear.group <- F #change to T to delete saved grouping calculations
datapath <- "../out/" #path to saved calculations
name.sim <- "similarity_k"
name.group <- "group_k"

#Function for clearing files
clear_files <- function(p, n){
  #Checks path p for files containing name n. If these exists, clears these
  datafiles <- list.files(path = p) %>%
    subset(grepl(n, .)) #vector of relevant file names
  if(length(datafiles) > 0){
    lapply(datafiles, function(x){
      file.remove(paste0(p, x))
    })
  }
}
if(clear.sim){clear_files(datapath, name.sim)}
if(clear.group){clear_files(datapath, name.group)}
##########

#General functions
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) #rounds decimal number x to k decimal places

#Data
df.t <- read.delim("../data/rawdata.csv", sep = ",", stringsAsFactors = F) %>%
  as.tibble()

####
#Similarity calculations
###


####Functions for similarity calculation

#Compute similarity between each topic
#To do so, we use number of keywords shared between topics
calculate_sharedkeywords <- function(keywords){
  #Takes vector (length = 2) of keywords, seperated by '|' and calculates number of words in common divided by (max) keywords in topic
  #Extract keywords
  kw <- lapply(keywords, function(x){
    strsplit(x, '|', fixed = T) %>%
      unlist %>%
      return()
  })
  #Calculate shared keyword ratio
  c(kw[[1]], kw[[2]]) %>%
    table() %>% 
    subset(. > 1) %>%
    length()/max(length(kw[[1]]), length(kw[[2]])) %>%
    return()
}

retrieve_interimdata <- function(n, k, hasheader){
  #Retrieves relevant data (where topicsinbatch == k) from file names paste0(n,k), hasheader is T/F value for datafile
  #g is threshold index, relevant for grouping files
  name <- paste0(n, k, '.csv')
  
  datafiles <- list.files(path = datapath) %>%
    subset(grepl(name, .)) #vector of relevant file names
  if(length(datafiles) == 0){
    return(NA) #no such files exist
  }
  else{
    read.delim(paste0(datapath, name), sep = ",", stringsAsFactors = F, header = hasheader) %>%
      as.tibble() %>%
      return()    
  }
}

calculate_similaritymatrix <- function(k){
  #For a given k (topicsperbatch), creates a N x N matrix of topic similarities, where N is the number of topics across all batches
  #Initialise df of interest (with ascending topics)
  df.tk <- df.t %>%
    filter(topicsperbatch == k) %>%
    arrange(topic)
  totaltopic <- dim(df.tk)[1]
  #Return NA if no modelling was run for this k
  if(totaltopic == 0){return(NA)}
  #Search for existing output
  dat.s <- retrieve_interimdata(name.sim, k, T)
  if(!any(is.na(dat.s))){
    #Then computation is already completed and stored
    df.s <- dat.s
  }
  else{
    #At least one value is not calculated
    #Re-run entire analysis
    df.s <- matrix(nrow = totaltopic, ncol = totaltopic)
    #Calculate non-diagonal similarity scores (0 to 1)
    for (i in 1:(totaltopic-1)){
      for (j in (i+1):totaltopic){
        df.s[i,j] <- df.tk %>%
          slice(c(i,j)) %>%
          pull(keywords) %>%
          calculate_sharedkeywords()
        df.s[j,i] <- df.s[i,j]
      }
    }
    diag(df.s) <- 1 #topics are identical to themselves
    #Write df.s to file
    write_delim(as.data.frame(df.s), path = paste0(datapath, name.sim, as.character(k), '.csv'), delim = ",")
  }
  return(df.s)
}

####Calculation of similarity

#1. Initialise list (length corresponds to topicsperbatch) to hold each similarity matrix
l.sim <- df.t %>% 
  summarise(max(topicsperbatch)) %>% 
  pull %>% 
  {vector("list", .)}

#2. Calculate similarity for each topicsperbatch in df.t
#Assign result to element corresponding to the number of topicsperbatch
df.t %>% 
  pull(topicsperbatch) %>% 
  unique %>%
  lapply(function(x){l.sim[[x]] <- calculate_similaritymatrix(x)}) %>%
  invisible() #hides output

####Functions for aligning each topic
#To align topic, threshold needs to be specified (ascending order only)
thresholds = seq(0.1, 1, by = 0.1) #above (or equal to) which topics are similar, below which are too dissimilar to be grouped

#Detect minimum between two columns of different groups
detect_mostsimilar <- function(g, s, t){
  #Function takes dataframe of groupings (see calculate_grouping) and corresponding similarity matrix to detect most similar topic to t
  #In cases with ties, selects the lowest value of t
  #add relevant similarity scores to g
  g.classed <- g %>%
    filter(topic == t) %>%
    pull(group) #grouping of classified topic
  #Most similar topic cannot belong to a group from the same batch as the topic
  bnum <- g %>%
          filter(topic == t | group == g.classed) %>%
          pull(batch) #batch for topic t
  gnum <- g %>%
    filter(batch %in% bnum) %>%
    pull(group) #topic cannot be grouped with other topics from the same batch
  
  #tibble of valid topics to compare
  g.valid <- g %>%
    mutate(sim = s[ , t]) %>%
    filter(!(group %in% gnum))
  #maximum similarity
  s.max <- g.valid %>%
    summarise(max(sim)) %>%
    pull
  g.valid %>%
    filter(sim == s.max) %>%
    slice(1) %>%
    pull(topic) %>%
    return()
}

detect_incompatgrouping <- function(tg, b, df){
  #takes grouping of topics, tg, and assesses whether it is incompatible (T) for a batch b, indicated by df
  df %>%
    filter(topic %in% tg) %>%
    pull(batch) %>%
    {(b %in% .)} %>%
    return
}


calculate_grouping <- function(k){
  #calculates alignment of N topics from topicsperbatch k, using a threshold t
  #at most, there exists N distinct groups of topics, at minimum, there exists k groups of topics
  #using SLINK algorithm of single-link clustering
  #Check if data already exists
  dat.res <- retrieve_interimdata(name.group, k, T)
  if(is.tibble(dat.res)){
    #Then computation has already been completed
    return(dat.res)
  }
  #1. initialise data
  #similarity matrix, each element represents sim(topic(i), topic(j)) from modelling of k
  df.s <- read.delim(paste0(datapath, name.sim, as.character(k), '.csv'), sep = ",", stringsAsFactors = F) %>%
    as.matrix()
  #results and pointers of towards nearest neighbours (nearest topic (location) at similarity of (closeness))
  df.r <- df.t %>%
    filter(topicsperbatch == k) %>%
    arrange(topic) %>%
    mutate(group = topic)
  df.r <- df.r %>%
    rowwise %>%
    mutate(location = detect_mostsimilar(df.r, df.s, topic)) %>%
    mutate(closeness = df.s[topic, location]) %>%
    ungroup
  
  #2. Begin merging loops until no valid groupings remain
  maxsim <- df.r %>%
    summarise(max(closeness)) %>%
    pull
  tol <- 1e-10 #tolerance for comparison of doubles
  next.logging <- length(thresholds) #next index of thresholds to be logged in results tibble
  i<-0
  while(maxsim >= (thresholds[1] - tol)){
    i<-i+1
    print(i)
    while(maxsim < (thresholds[next.logging] - tol)){
      #write results to df.r
      groupname <- paste0('G', as.character(next.logging))
      df.r <- df.r %>%
        mutate(!!groupname := group)
      next.logging <- next.logging - 1
    }
    #whilst largest similarity exceeds threshold, there are topics that may be aligned
    #find information of most similar pairing (if ties, selected the topic corresponding with the smallest number)
    detail <- df.r %>%
      filter(closeness == maxsim) %>%
      slice(1)
    #group remaining
    g.remain <- pull(detail, group)
    #Merge both groups
    g.merging <- df.r %>%
      filter(topic == pull(detail, location)) %>%
      pull(group)
    #swap if g.remain > g.merging
    if(g.remain > g.merging){
      temp <- g.remain
      g.remain <- g.merging
      g.merging <- temp
    }
    browser(expr = (filter(df.r, group %in% c(g.remain, g.merging)) %>% pull(batch) %>% {any(duplicated(.))}))
    df.r <- df.r %>%
      mutate(group = if_else(group == g.merging, g.remain, group))
    #Update similarity matrix by selecting highest similarity of all topics
    t.grouped <- df.r %>%
                  filter(group == g.remain) %>%
                  pull(topic)
    #Calculate maximum for each element for entire group
    sim.update <- df.s[ , t.grouped] %>%
      apply(1, max)
    df.s[ , g.remain] <- sim.update
    df.s[g.remain, ] <- sim.update
    #Remove unneeded rows
    df.s[ , t.grouped[-1]] <- 0
    df.s[t.grouped[-1], ] <- 0
    #Update locations
    df.r <- df.r %>%
       rowwise %>%
       mutate(location = if_else(topic == t.grouped[1], detect_mostsimilar(df.r, df.s, topic),
                                if_else((location %in% t.grouped)||(t.grouped %in% location), 
                                        if_else(detect_incompatgrouping(t.grouped, batch, df.r), detect_mostsimilar(df.r, df.s, topic), t.grouped[1]),
                                        location))) %>%
       mutate(closeness = if_else(group == g.remain, 
                                  if_else(topic == t.grouped[1], df.s[topic, location], 0), 
                                  if_else(detect_incompatgrouping(t.grouped, batch, df.r), df.s[topic, location], closeness))) %>%
       ungroup
    #Recalculate maxsim
    maxsim <- df.r %>%
      summarise(max(closeness)) %>%
      pull
    if(is.na(maxsim)){break}
  }
  groupname <- paste0('G', as.character(next.logging))
  df.r <- df.r %>%
    mutate(!!groupname := group) %>%
    select(-c(location, closeness))
  write_delim(as.data.frame(df.r), path = paste0(datapath, name.group, as.character(k), '.csv'), delim = ",")
  df.r %>% return()
}

dft2<- df.t %>% 
  pull(topicsperbatch) %>% 
  unique %>%
  sort %>%
  lapply(function(x){
    calculate_grouping(x) %>%
      return()}) %>%
  {do.call(rbind, .)} %>%
  invisible()

#error @ i = 133, batch = 23, topics 111, 115; groups 109 and 35 about to be merged


