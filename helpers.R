library(tidyverse)

if (file.exists(f <- file.path(params$rootdir,'NodeKeys.csv')))   {
  allnodes <- read_csv(f,col_types = cols())
} else {
  allnodes <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=0') %>%
    # rename columns of interest with simple names
    rename(order = 1,
           UniqueName = 4) %>%
    # discard the comment line at the beginning of the table
    filter(order != 'Comment') %>%
    write_csv(f)
}


if (file.exists(f <- file.path(params$rootdir,'ExperimentIDs.csv')))   {
  experimentIDs <- read_csv(f,col_types = cols())
} else {
  experimentIDs <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
    write_csv(f)
}

if (!is_null(params$experimentID)) {
  ExperimentName <- dplyr::filter(experimentIDs,`Experiment ID` %in% params$experimentID)$ExperimentName
}else{ ExperimentName <- NULL
}


Q_cols <- cols(`Participant Private ID` = col_factor())


Q_read <- function(datafiles) {
  orig <- tibble()
  for (f in datafiles) {
    orig <- read_csv(f,col_types = cols(.default = col_character())) %>%
      mutate(File = basename(f))%>%
      bind_rows(orig)
  }
  
  orig %>%
    mutate(`Participant Private ID` = as.factor(`Participant Private ID`))
}


Q_Nsuj <- function(orig){
  
  N <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    group_by(File,`Experiment ID`) %>%
    summarize(N = n())
}

Q_Complete <- function(orig) {
  d <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    group_by(File) %>%
    mutate(Response = as.numeric(Response)/1000)
  p <- ggplot(d,aes(x = `Participant Private ID`, y=Response,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    scale_y_log10() +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_wrap(~File) +
    ylab('Time to complete (s, log scale)')
  print(p)
  
}


T_read <- function(datafiles) {
  orig <- tibble()
  for (f in datafiles) {
    orig <- read_csv(f,col_types = cols(.default = col_character())) %>%
      mutate(File = basename(f))%>%
      bind_rows(orig)
  }
  
  orig %>%
    mutate(`Participant Private ID` = as.factor(`Participant Private ID`))
}

T_Nsuj <- function(orig){
  
  N <- orig %>% filter(`Trial Number` %in% 'END TASK') %>%
    group_by(File,`Experiment ID`) %>%
    summarize(N = n())
}

T_Complete <- function(orig) {
  d <- orig %>% filter(`Trial Number` %in% 'END TASK') %>%
    group_by(File) %>%
    mutate(`Reaction Time` = as.numeric(`Reaction Time`)/1000)
  p <- ggplot(d,aes(x = `Participant Private ID`, y=`Reaction Time`,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_wrap(~File) +
    ylab('Time to complete (s, log scale)') +
    scale_y_log10()
  print(d %>% summarize(n=n()))
  print(p)
  
}

