library(tidyverse)
if (!exists('params')) {
  params = list(rootdir = '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA')
}

gimmedata <- function(DataDir = getwd(), ExperimentID = '[0-9]{5}', ExperimentName = '.*', UniqueName = '.*', Session = '.*', Run = '.*', file = '',  clean = T, verbose = T) {
  
  if (file != '') {
    if (! file.exists(file)) {stop('File provided does not exist')}
    fs <- file
  } else {
    p <- paste0('data_exp_([0-9]{5}-)*', ExperimentID, '(-[0-9]{5})*', '_', ExperimentName, '(_Session', Session, ')?')
    d <- list.files(path = DataDir, pattern = p,full.names = T)
    if (verbose) {
      cat(paste0('Loading data from ', str_replace(d,DataDir,'')),sep = '\n')
    }
    
    p <- paste0('S',Session,'_', UniqueName, '(_r',Run,')?.csv')
    fs <- list.files(path = file.path(d),pattern = p,full.names = T)
  }
  if (length(fs) == 0) { stop(paste0('Could not find data (', p, ')'))}
  
  d <- tibble()
  for (f in fs) {
    FF <- str_match_all(basename(f),'(S[^_]*)_([^_]*)_?r?([^\\.]*)?.csv')
    Session <- FF[[1]][2]
    UniqueName <- FF[[1]][3]
    Run <- FF[[1]][4]
    
    if (verbose) {cat(paste0('Loading ',str_replace(f,DataDir,'')),sep = '\n')}
    
    tmp <- read_csv(f,col_types = cols(.default = col_character())) %>%
      mutate(Session = as.character(Session),
             UniqueName = as.character(UniqueName),
             Run = as.character(Run))
    if (clean){
      tmp <- tmp %>% select(-starts_with('order-'),-starts_with('checkpoint-'),-starts_with('branch-'),-matches('Schedule ID'),-starts_with('Participant'),`Participant Private ID`)
    }
    d <- bind_rows(d,tmp)
  }
  d %>% select(Session,UniqueName,Run,matches('PID'),everything()) %>%
    mutate(`UTC Date` = lubridate::dmy_hms(`UTC Date`),
           `Local Date` = lubridate::dmy_hms(`Local Date`))
}

source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  
  envir <- globalenv()
  source(tempR, local = envir, ...)
}


Q_cols <- cols(`Participant Private ID` = col_factor())


Q_read <- function(datafiles) {
  orig <- tibble()
  for (f in datafiles) {
    orig <- read_csv(f,col_types = cols(.default = col_character())) %>%
      bind_rows(orig)
  }
  
  orig %>%
    mutate(`Participant Private ID` = as.factor(`Participant Private ID`))
}


Q_Nsuj <- function(orig){
  N <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    select(`Participant Private ID`,Session,Run,`Experiment ID`,UniqueName) %>%
    group_by(Session,Run,`Experiment ID`,UniqueName) %>%
    distinct() %>%
    summarize(N = n())
}

Q_Complete <- function(orig) {
  d <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    group_by(Session,Run,`Experiment ID`) %>%
    mutate(Response = as.numeric(Response)/1000)
  p <- ggplot(d,aes(x = `Participant Private ID`, y=Response,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    scale_y_log10() +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_grid(Session~Run) +
    ylab('Time to complete (s, log scale)')
  print(p)
  
}


T_read <- function(datafiles) {
  orig <- tibble()
  for (f in datafiles) {
    orig <- read_csv(f,col_types = cols(.default = col_character())) %>%
      bind_rows(orig)
  }
  
  orig %>%
    mutate(`Participant Private ID` = as.factor(`Participant Private ID`))
}

T_Nsuj <- function(orig){
  N <- orig %>% filter(`Trial Number` %in% 'END TASK') %>%
    select(`Participant Private ID`,Session,Run,`Experiment ID`,UniqueName) %>%
    group_by(Session,Run,`Experiment ID`,UniqueName) %>%
    distinct() %>%
    summarize(N = n())
}

T_Complete <- function(orig) {
  d <- orig %>% filter(`Trial Number` %in% 'END TASK') %>%
    group_by(Session,Run, UniqueName,`Experiment ID`,`Repeat Key`) %>%
    mutate(Duration = as.numeric(`Reaction Time`)/1000)
  
  p <- ggplot(d,aes(x = `Participant Private ID`, y = Duration,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_grid(Session~Run) +
    ylab('Time to complete (s, log scale)') +
    scale_y_log10()
  
  print(d %>% group_by(Session,Run, UniqueName,`Experiment ID`) %>% summarize(n=n()))
  print(p)
  
  return(d %>% select(Session, Run, UniqueName, `Participant Private ID`,`Repeat Key`, Duration))
  
}

f <- file.path(params$rootdir,'NodeKeys.csv')

lastmod <- googledrive::drive_get('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=0') %>%
  hoist(drive_resource,modified_on = 'modifiedTime') %>%
  mutate(modified_on = lubridate::ymd_hms(modified_on))

loc <- file.info(f)
if (loc$mtime < lastmod$modified_on) {
  
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
} else {
  allnodes <- read_csv(f, col_types = cols())
}

f <- file.path(params$rootdir,'ExperimentIDs.csv')

lastmod <- googledrive::drive_get('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
  hoist(drive_resource,modified_on = 'modifiedTime') %>%
  mutate(modified_on = lubridate::ymd_hms(modified_on))

loc <- file.info(f)
if (loc$mtime < lastmod$modified_on) {
  ExperimentIDs <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
    pivot_longer(cols=starts_with('Session'),names_to = 'Session', values_to = 'Experiment ID',names_prefix = 'Session',values_drop_na = T) %>%
    write_csv(f)
} else {
  ExperimentIDs <- read_csv(f,col_types = cols())
}

if (!is_null(params$ExperimentID)) {
  ExperimentID <- params$ExperimentID
  ExperimentName <- dplyr::filter(ExperimentIDs,`Experiment ID` %in% params$ExperimentID)$ExperimentName
}else{
  if (!is_null(params$ExperimentName)) {
    ExperimentName <- params$ExperimentName
    ExperimentID <- dplyr::filter(ExperimentIDs,ExperimentName == params$ExperimentName)$`Experiment ID`
  }else {
    ExperimentID <- ExperimentIDs$`Experiment ID`
    ExperimentName <- dplyr::filter(ExperimentIDs,`Experiment ID` %in% ExperimentID)$ExperimentName
  }
}

Session <- dplyr::filter(ExperimentIDs,`Experiment ID` %in% ExperimentID)$Session
