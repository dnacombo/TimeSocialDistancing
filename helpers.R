library(tidyverse)
library(lubridate)

source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  
  envir <- globalenv()
  source(tempR, local = envir, ...)
}
f <- file.path(params$rootdir,'NodeKeys.csv')
  allnodes.S1 <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=0') %>%
    filter(prefix != 'Comment')
  
  allnodes.S2 <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=1606433485') %>%
    filter(prefix != 'Comment')
  
  allnodes.S3 <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=1814296211') %>%
    filter(prefix != 'Comment')

  (allnodes <- bind_rows(allnodes.S1,
                        allnodes.S2,
                        allnodes.S3,.id = "Session")%>%
    mutate_all(.funs = ~ na_if(.,'N/A'))) %>%
    write_csv(f)
rm(allnodes.S1,allnodes.S2,allnodes.S3)

f <- file.path(params$rootdir,'ExperimentIDs.csv')
  ExperimentIDs <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
    pivot_longer(cols=starts_with('Session'),names_to = 'Session', values_to = 'Experiment ID',names_prefix = 'Session',values_drop_na = T) %>%
    write_csv(f)

if (!is_null(params$ExperimentID)) {
  ExperimentID <- params$ExperimentID
  ExperimentName <- dplyr::filter(ExperimentIDs,`Experiment ID` %in% params$ExperimentID)$ExperimentName
}else{
  if (!is_null(params$ExperimentName)) {
    ExperimentName <- params$ExperimentName
    ExperimentID <- dplyr::filter(ExperimentIDs,ExperimentName == params$ExperimentName)$`Experiment ID`
  }else {stop('Give either ExperimentName or Experiment ID')}
  
}
Session <- dplyr::filter(ExperimentIDs,`Experiment ID` %in% ExperimentID)$Session

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
    select(`Participant Private ID`,File,`Experiment ID`) %>%
    group_by(File,`Experiment ID`) %>%
    distinct() %>%
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
    select(`Participant Private ID`,File,`Experiment ID`) %>%
    group_by(File,`Experiment ID`) %>%
    distinct() %>%
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

gimmedata <- function(DataDir = getwd(), ExperimentID = '[0-9]{5}', ExperimentName = '.*', UniqueName = '.*', Session = '.*', Run = '.*', clean = T, verbose = T) {
  
  p <- paste0('data_exp_([0-9]{5}-)*', ExperimentID, '(-[0-9]{5})*', '_', ExperimentName, '_Session', Session)
  d <- list.files(path = DataDir, pattern = p,full.names = T)
  if (verbose) {
    cat(paste0('Loading data from ', str_replace(d,DataDir,'')),sep = '\n')
  }
  
  p <- paste0('S',Session,'_', UniqueName, '_r',Run,'.csv')
  fs <- list.files(path = file.path(d),pattern = p,full.names = T)
  if (length(fs) == 0) { stop(paste0('Could not find data (', p, ')'))}
  
  d <- tibble()
  for (f in fs) {
    FF <- str_match_all(basename(f),'(S[^_]*)_([^_]*)_r([^\\.]*)')
    Session <- FF[[1]][2]
    UniqueName <- FF[[1]][3]
    Run <- FF[[1]][4]
    
    if (verbose) {cat(paste0('Loading ',str_replace(f,DataDir,'')),sep = '\n')}
    
    d <- read_csv(f,col_types = cols(.default = col_character())) %>%
      mutate(Session = as.character(Session),
             UniqueName = as.character(UniqueName),
             Run = as.character(Run)) %>%
      bind_rows(d,.)
  }
  if (clean){
    d <- d %>% select(-starts_with('order-'),-starts_with('checkpoint-'),-starts_with('branch-'),-`Schedule ID`,-starts_with('Participant'),`Participant Private ID`)
  }
  d %>% select(Session,UniqueName,Run,matches('PID'),everything()) %>%
    mutate(`UTC Date` = dmy_hms(`UTC Date`),
           `Local Date` = dmy_hms(`Local Date`))
}