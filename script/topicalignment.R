#Packages
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)


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
specify_decimal <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=k))) #rounds decimal number x to k decimal places

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
  lapply(function(x){l.sim[[x]] <<- calculate_similaritymatrix(x)}) %>%
  invisible() #hides output

####Functions for aligning each topic
#To align topic, threshold needs to be specified (ascending order only)
thresholds = seq(0.1, 1, by = 0.1) #above (or equal to) which topics are similar, below which are too dissimilar to be grouped

#Detect minimum between two columns of different groups
detect_mostsimilar <- function(g, s, t, func.me, th.me){
  #Function takes dataframe of groupings (see calculate_grouping) and corresponding similarity matrix to detect most similar topic to t
  #also input var: func & th, see calculate_grouping
  #In cases with ties, selects the lowest value of t
  #Firstly, if all similarity scores are 0, return NA
  tol <- 1e-10
  if(!any(s[ , t] > 0 + tol)){
    return(NA)
  }
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
  
  #if no valid comparisons, return NA
  if(nrow(g.valid) == 0){return(NA)}
  #if func.me == T, then only consider groups where mean > threshold
  if(func.me){
    if(th.me >= 0 - tol){
    #topics in group to-be-merged
    compar_t <- g %>%
      filter(group == g.classed) %>%
      pull(topic)
    #calculate if median similarity between each topic of to-be-merged group with all topics from potential group > threshold
    k <- g.valid %>%
      pull(topicsperbatch) %>%
      unique() #number of topics per batch
    g.valid <- g.valid %>% 
      group_by(group) %>% 
      mutate(me = 
               l.sim[[k]][compar_t, topic] %>% 
               unlist %>%
               #median) %>%
               mean) %>%
      filter(me >= th.me-tol) %>%
      ungroup %>%
      select(-me) #calculates mean
    #Check if any valid groups remain
    if(nrow(g.valid) == 0){return(NA)}
    }
  }
  #maximum similarity
  s.max <- g.valid %>%
    summarise(max(sim, na.rm = T)) %>%
    pull
  g.valid %>%
    filter(sim == s.max) %>%
    slice(1) %>%
    pull(topic) %>%
    return()
}

detect_incompatgrouping <- function(g1, g2, df){
  #takes two groups (g1, g2) from df and checks for incompatability (return T if yes)
  df %>%  
    filter(group %in% c(g1,g2)) %>%
    pull(batch) %>%
    anyDuplicated() > 0 %>%
    return()
}

calculate_grouping <- function(k, func.me, th.me){
  #calculates alignment of N topics from topicsperbatch k, using a threshold t
  #at most, there exists N distinct groups of topics, at minimum, there exists k groups of topics
  #using SLINK algorithm of single-link clustering
  #func is optional output to add further constraints to grouping"
  #if missing, no constraints
  #if T, ensures topic is only grouped if the median similarity between topic and grouped topics >= threshold
  #Check if data already exists
  dat.res <- retrieve_interimdata(name.group, k, T)
  if(is.tibble(dat.res)){
    #Then computation has already been completed
    return(dat.res)
  }
  if(missing(func.me)){func.me <- F}
  if(missing(th.me)){th.me <- 0}
  
  #1. initialise data
  #similarity matrix, each element represents sim(topic(i), topic(j)) from modelling of k
  df.s <- l.sim[[k]] %>%
    as.matrix
  #results and pointers of towards nearest neighbours (nearest topic (location) at similarity of (closeness))
  df.r <- df.t %>%
    filter(topicsperbatch == k) %>%
    arrange(topic) %>%
    mutate(group = topic)
  df.r <- df.r %>%
    rowwise %>%
    mutate(location = detect_mostsimilar(df.r, df.s, topic, func.me, th.me)) %>%
    mutate(closeness = df.s[topic, location]) %>%
    ungroup
  
  #2. Begin merging loops until no valid groupings remain
  maxsim <- df.r %>%
    summarise(max(closeness, na.rm = T)) %>%
    pull
  tol <- 1e-10 #tolerance for comparison of doubles
  next.logging <- length(thresholds) #next index of thresholds to be logged in results tibble
  i<-0
  while(maxsim >= (thresholds[1] - tol)){

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
    
    i <- i + 1
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
    #Update locations + respective closeness
    df.r <- df.r %>%
      rowwise %>%
      mutate(location = ifelse(group == g.remain,
                                ifelse(topic == t.grouped[1], as.integer(detect_mostsimilar(df.r,df.s,topic,func.me, th.me)), as.integer(NA)),
                                ifelse(detect_incompatgrouping(g.remain, group, df.r), as.integer(detect_mostsimilar(df.r,df.s,topic,func.me,th.me)),
                                        ifelse(location %in% t.grouped, t.grouped[1], location))
            )) %>%      
      mutate(closeness = if_else(group == g.remain, 
                                  if_else(topic == t.grouped[1], df.s[topic, location], as.double(0)), 
                                  if_else(detect_incompatgrouping(g.remain, group, df.r), df.s[topic, location], closeness))) %>%
      ungroup
    #Recalculate maxsim
    maxsim <- df.r %>%
      summarise(max(closeness, na.rm=T)) %>%
      pull
    if(is.na(maxsim)){break}
  }
  groupname <- paste0('G', as.character(next.logging))
  df.r <- df.r %>%
    mutate(!!groupname := group) %>%
    select(-c(location, closeness, group))
  #for remaining thresholds, there are no solutions - allocate NA to each group to indicate process has terminated
  if(next.logging > 1){
    newcol <- rep(as.integer(NA_character_), next.logging - 1)
    names(newcol) <- paste0('G', 1:(next.logging - 1))
    df.r <- mutate(df.r, !!! newcol)
  }
  write_delim(as.data.frame(df.r), path = paste0(datapath, name.group, as.character(k), '.csv'), delim = ",")
  df.r %>% return()
}

df.g <- df.t %>%
  pull(topicsperbatch) %>%
  unique %>%
  lapply(function(x){
    calculate_grouping(x, T, 0.3) %>%
      return()}) %>%
      {do.call(rbind, .)}

###Shiny App to display groupings

# Define UI
ui <- fluidPage(
  titlePanel("Topic alignment using single-linkage clustering"),

  #Sidebar layout with input and output definitions ----
  sidebarLayout(

    #Sidebar panel for inputs ----
    sidebarPanel(

      #Input: Which modelling approach?
      radioButtons(
        inputId = "k.select", label = "Select modelling approach:",
        c("5 Topics/Batch" = "5",
          "10 Topics/Batch" = "10",
          "20 Topics/Batch" = "20")
      ),

      #Input: Numeric for the threshold of classifying two topics as similar
      sliderTextInput("threshold",
                      label = "Minimum criteria for matching topics :",
                      grid = TRUE,
                      choices = as.character(thresholds),
                      selected = as.character(thresholds[3])
      ),

      #Input: Slider for the number of groups to consider
      sliderInput("group",
                  label = "Number of groups:",
                  min = 1,
                  max = 40,
                  step = 1,
                  value = 10),

      #Saving external file options
      #Input: Save groupings
      actionButton("saveButton", "Save output"),

      ###Display options
      #Input: Slider for the number of keywords to display
      sliderInput("keywordsNum",
                  label = "Number of keywords for groups:",
                  min = 1,
                  max = 10,
                  step = 1,
                  value = 10),

      #Input: Display option
      sliderTextInput("propthresh",
                      label = "Proportion of times a keyword must occur in aligned topics to be displayed in summary",
                      grid = TRUE,
                      choices = as.character(seq(0, 1, by = 0.05)),
                      selected = 0.5
      ),

      #Input: Button for sdisplay of columns in summary
      materialSwitch(
        inputId = "show.kw",
        label = "Show keywords in summary",
        status = "danger",
        value = T
      ),

      #Input: Button for display of columns in summary
      materialSwitch(
        inputId = "show.topic",
        label = "Show topics in summary",
        status = "danger",
        value = FALSE
      ),

      #Input: Button for display of columns in summary
      materialSwitch(
        inputId = "show.batch",
        label = "Show batches in summary",
        status = "danger",
        value = FALSE
      ),

      #Input: Button for display of columns in summary
      materialSwitch(
        inputId = "show.vol",
        label = "Show volume in summary",
        status = "danger",
        value = T
      ),

      #Input: Button for display of rows in summary
      materialSwitch(
        inputId = "show.unextracted",
        label = "Show unextracted in summary",
        status = "danger",
        value = F
      )

      ),

    mainPanel(
      tabsetPanel(type = "tabs",
                  #tabPanel("Debug", textOutput(outputId = "textTest"), dataTableOutput(outputId = "tableTest")),
                  tabPanel("Topics", DT::dataTableOutput(outputId = "topicTable")),
                  tabPanel("Alignment of topics across batches", plotOutput(outputId = "topicImage")),
                  tabPanel("Summary of aligned topics", textOutput(outputId = "groupText"), DT::dataTableOutput(outputId = "groupTable"))
      )
    )
  )
)
server <- function(input, output) {
  gvals <- reactiveValues() #dataframes of group data (topics has rows = topics; groups has rows = groups)
  gpara <- reactiveValues(k = 0, g = 0, t = 0) #holds parameters for grouping, includes topicsperbatch (k), number of groups (g), and threshold (th)
  res <- reactiveValues() #results: table of grouped topics & summary of extracted groups & summary of keywords in each group
  rpara <- reactiveValues() #parameters of results summary: kw is the proportional threshold above which keywords are shown
  #show indicates the display of topics/models in summary table

  observe({
    #Register parameters
    gpara$k <- as.numeric(input$k.select)
    gpara$g <- as.numeric(input$group)
    gpara$t <- as.numeric(input$threshold)
    rpara$kw <- as.numeric(input$propthresh)
    rpara$showbatch <- input$show.batch
    rpara$showkw <- input$show.kw
    rpara$showvol <- input$show.vol
    rpara$showtopic <- input$show.topic
    rpara$showunext <- input$show.unextracted
  })

  observe({
    #Watch for changes in grouping criteria & update dataframe of topics
    #Similar structure to df.t, but contains relevant group
    tol <- 1e-10 #tolerance for comparisons
    groupname <- paste0('G', which(abs(thresholds - gpara$t) < tol))
    gvals$topics <- df.g %>%
      filter(topicsperbatch == gpara$k) %>%
      mutate(group = !!as.name(groupname)) %>%
      select(-matches('^G', ignore.case = F))
    #Vars = group, number of topics in group, vol of tweets
    #Arranged by descending number of topics in each group.
    #Groups tied are then arranged by descending volume of tweets
    gvals$groups <- gvals$topics %>%
      group_by(group) %>%
      summarise_at(vars(group,vol), funs(length, sum)) %>%
      ungroup %>%
      select(-c(vol_length,group_sum)) %>%
      rename(totaltopic = group_length, totalvol = vol_sum) %>%
      arrange(desc(totaltopic), by_group = desc(totalvol))
    #From gvals$groups, select top gpara$g groups
    #0 denotes grouping will not be extracted
    gvals$groups <- isolate(gvals$groups) %>%
      mutate(extract = c(seq(1:gpara$g), rep(0, nrow(isolate(gvals$groups)) - gpara$g)))
  })

  observe({
    tol <- 1e-10
    res$topics <- gvals$topics %>%
      left_join(gvals$groups, by = "group")
    res$keywords <- res$topics %>%
      rowwise %>%
      mutate(keyword.num = length(unlist(strsplit(keywords,"|", fixed = T)))) %>%
      ungroup %>%
      separate(keywords, into = paste0("kw", 1:max(.$keyword.num)), sep =  "[|]") %>%
      group_by(group) %>%
      gather(key, keywords, starts_with("kw")) %>%
      select(-key) %>%
      group_by(keywords,group) %>%
      summarise(sum_keywords = table(keywords, useNA = "ifany")) %>%
      ungroup %>%
      left_join(select(res$topics, c(group, totaltopic)), by = "group") %>%
      distinct %>%
      group_by(group) %>%
      mutate(prop_keywords = sum_keywords/totaltopic)
    #Filter out keywords where prop < prop_threshold
    df.filterkw <- res$keywords %>%
      filter(prop_keywords >= (rpara$kw - tol)) %>%
      group_by(group) %>%
      select(group,keywords) %>%
      mutate(keywords = paste0(keywords, collapse = " ")) %>%
      distinct() %>%
      ungroup()
    res$groups <- res$topics %>%
       # select(-extract) %>%
       group_by(group) %>%
       mutate(aligned_topic = paste0(topic, collapse = " ")) %>%
       mutate(aligned_batch = paste0(batch, collapse = " ")) %>%
      select(-c(topic, batch, keywords, vol)) %>%
      distinct() %>%
      rowwise %>%
      mutate(vol_prop = totalvol/sum(.$totalvol, na.rm = T)) %>%
      ungroup %>%
      left_join(df.filterkw, by = "group")
  })

  observeEvent(input$saveButton, {
    isolate(res$topics) %>%
      as.data.frame() %>%
      write_delim(path = paste0(datapath, "output_k", as.character(gpara$k), '.csv'), delim = ",")
  })

#These were for debugging purposes
#  output$textTest <- renderText({as.character(rpara$showunext)})
#  output$tableTest <- renderDataTable({res$topics})

  output$topicTable <- DT::renderDataTable({
    #Renders a datatable of topics and keywords
    gvals$topics %>%
      rowwise() %>%
      mutate(keywords = paste(strsplit(keywords, '|', fixed = T)[[1]], collapse = " ", sep = " ")) %>%
      ungroup %>%
      select(-topicsperbatch) %>%
      DT::datatable(rowname = F, options = list(pageLength = gpara$k))
  })

  output$topicImage <- renderPlot({
    #initialise disp, where each row is an extracted grouping and each column is a batch
    #elements code presence of group in batch (1 if present, 0 if not)
    disp <- sapply(sort(unique(gvals$topics$batch)), function(x){
        gvals$groups %>%
          filter(extract > 0) %>%
          pull(group) %>%
          sapply(function(y){
            gvals$topics %>%
              filter(topicsperbatch == gpara$k) %>%
              filter(batch == x) %>%
              filter(group == y) %>%
              nrow() %>%
              return()
          }) %>%
          return()
      })
    #to each column, add blank space, and indicate the presence of 'ungrouped' topics (i.e., grouped topics not extracted)
    disp <- disp %>%
      apply(2, function(x){c(rep(-1, gpara$k - sum(x, na.rm = T)), rep(0, sum(x, na.rm = T)))}) %>%
      {rbind(disp, .)}
    #Use image function to draw grid of disp
    #Data wrangle with disp
    disp <- apply(disp, 2, rev)
    #Set coordinates for axis labels
    x1 <- 0.5;
    x2 <- dim(disp)[2] + 0.5;
    y1 <- 0.5;
    y2 <- dim(disp)[1] + 0.5;
    #Set colours
    if(any(gvals$groups$extract > 0)){colscheme <- c(326, 1, 509)}
    else(colscheme <- c(1, 509))
    image(1:dim(disp)[2], 1:dim(disp)[1],
          t(disp),
          xlab = 'Batch', xlim = c(x1,x2), xaxp = c(x1 + 0.5 , x2 - 0.5 , x2 - x1 - 1),
          ylab = 'Aligned topics', yaxt = 'n',
          col = colors()[colscheme])
    axis(2, at = (gpara$k + 1):(gpara$g + gpara$k), labels = (gpara$g):1)
    abline(h=y1:y2, v=x1:x2, col='gray', lty=3)
    })

  output$groupTable <- DT::renderDataTable({
    #Renders data table summarising groups, which of these are extracted, and the alignment of topics and batches
    #display parameters
    disp.cols <- c(T, T, rpara$showvol, rpara$showvol, rpara$showunext, rpara$showtopic, rpara$showbatch, rpara$showkw)
    res$groups %>%
      arrange(desc(totaltopic), by_group = desc(totalvol)) %>%
      rowwise %>%
      mutate(vol_prop = specify_decimal(vol_prop,4)*100) %>%
      ungroup %>%
      filter(extract > as.integer(-1*as.integer(rpara$showunext))) %>% #workaround, couldnt get ifelse to combine with filters
      select(-topicsperbatch) %>%
      select(group, totaltopic, totalvol, vol_prop, extract, aligned_topic, aligned_batch, keywords) %>%
      .[ , disp.cols] %>%
      datatable(rowname = F, colnames = subset(c("Grouping ID", "Topics in Group", "Volume of tweets", "Volume of Tweets (%)", "Extraction ID", "Aligned Topics", "Batches with a topic in this grouping", "Prominent Keywords"), subset = disp.cols),
                options = list(pageLength = gpara$g))
  })
  
  output$groupText <- renderText({
    sumtab <- res$groups %>%
      group_by(ex = extract > 0) %>%
      summarise_at(vars(totalvol, vol_prop), funs(sum)) %>%
      ungroup
    sprintf('Extracted groupings of topics account for %d (%.2f%%) tweets in the corpus, whereas %d (%.2f%%) are not regarded as aligned with other topics.',
            pull(filter(sumtab, ex==T), totalvol),
            pull(filter(sumtab, ex==T), vol_prop) %>%
              specify_decimal(4)*100,
            pull(filter(sumtab, ex==F), totalvol),
            pull(filter(sumtab, ex==F), vol_prop) %>%
              specify_decimal(4)*100
            )
      })
}
# Run the application
shinyApp(ui = ui, server = server)





