library(tidyverse)
options(dplyr.summarise.inform=F)
options(gargle_oauth_email = 'maximilien.chaumon@gmail.com')


if (!exists('params')) {
  datadir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'
} else {
  if (is.null(params$datadir)) {
    datadir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'
  } else {
    datadir <- params$datadir
  }
}


source('gimmedata.R')

UpdateTables <- function(datadir = '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA', wherefrom='online') {
  
  f <- file.path(datadir,'NodeKeys.csv')
  
  test <- T
  if (wherefrom == 'online'){
    tryCatch( {
      if (! file.exists(f)) stop(paste0(f, 'does not exist'))
      loc <- file.info(f)
      googledrive::drive_deauth()
      lastmod <- googledrive::drive_get('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=0') %>%
        hoist(drive_resource,modified_on = 'modifiedTime') %>%
        mutate(modified_on = lubridate::ymd_hms(modified_on))
      test <- loc$mtime < lastmod$modified_on
    },error = function(e){test <- T})
  } else { test <- F}
  
  if (test) {
    cat('Updating NodeKeys...\n')
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
  
  f <- file.path(datadir,'ExperimentIDs.csv')
  
  test <- T
  if (wherefrom == 'online'){
    tryCatch( {
      if (! file.exists(f)) stop(paste0(f, 'does not exist'))
      loc <- file.info(f)
      lastmod <- googledrive::drive_get('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
        hoist(drive_resource,modified_on = 'modifiedTime') %>%
        mutate(modified_on = lubridate::ymd_hms(modified_on))
      
      test <- loc$mtime < lastmod$modified_on
    }, error = function(e){test <- T})
  } else { test <- F}
  
  if (test) {
    cat('Updating ExperimentIDs...\n')
    ExperimentIDs <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
      pivot_longer(cols=starts_with('Session'),names_to = 'Session', values_to = 'ExperimentID',names_prefix = 'Session',values_drop_na = T) %>%
      write_csv(f)
  } else {
    ExperimentIDs <- read_csv(f,col_types = cols())
  }
  
  return(list(allnodes = allnodes, ExperimentIDs = ExperimentIDs))
  
}
list2env(UpdateTables(datadir), envir = globalenv())

Name2ID <- function(ExperimentName, ExperimentIDs) {
  EN <- ExperimentName
  return(dplyr::filter(ExperimentIDs,ExperimentName == EN)$ExperimentID)
}
ID2Name <- function(ExperimentID, ExperimentIDs) {
  ID <- ExperimentID
  return(dplyr::filter(ExperimentIDs,ExperimentID %in% ID)$ExperimentName)
}
Name2Session <- function(ExperimentName, ExperimentIDs) {
  EN <- ExperimentName
  return(dplyr::filter(ExperimentIDs,ExperimentName %in% EN)$Session)
}
ID2Session <- function(ExperimentID, ExperimentIDs) {
  ID <- ExperimentID
  return(dplyr::filter(ExperimentIDs,ExperimentID %in% ID)$Session)
}
Session2Name <- function(Session,ExperimentIDs){
  S <- Session
  return(dplyr::filter(ExperimentIDs,Session %in% S)$`ExperimentName`)
}
Session2ID <- function(Session,ExperimentIDs){
  S <- Session
  return(dplyr::filter(ExperimentIDs,Session %in% S)$ExperimentID)
}
Country2ID <- function(Country, ExperimentIDs) {
  C <- Country
  return(dplyr::filter(ExperimentIDs,Country == C)$ExperimentID)
}
ID2Country <- function(ExperimentID, ExperimentIDs) {
  ID <- ExperimentID
  return(dplyr::filter(ExperimentIDs,ExperimentID %in% ID)$Country)
}
Country2Name <- function(Country, ExperimentIDs) {
  C <- Country
  return(dplyr::filter(ExperimentIDs,Country %in% C)$ExperimentName)
}
Name2Country <- function(ExperimentName, ExperimentIDs) {
  EN <- ExperimentName
  return(dplyr::filter(ExperimentIDs,ExperimentName == EN)$Country)
}

paramsMatch <- function(params = NULL, ExperimentName = NULL, ExperimentID = NULL, Session = NULL, Country = NULL, experimentIDs) {
  
  if (!is.null(params)) {
    dum <- list2env(params, envir = environment())
  }
  if (any(attr(experimentIDs,'class') %in% 'knit_param_list')) {
    attr(experimentIDs,'class') <- NULL
    experimentIDs <- as_tibble(experimentIDs)
  }
  # if (all(is.null(c(ExperimentName, ExperimentID, Session, Country)))){
  #   stop('Specify at least one data selection method: ExperimentName, ExperimentID, Session, or Country')
  # }
  EN <- if (is.null(ExperimentName)) {experimentIDs$ExperimentName} else {ExperimentName}
  EID <-  if (is.null(ExperimentID)) {experimentIDs$`ExperimentID`} else {ExperimentID}
  S <-  if (is.null(Session)) {experimentIDs$Session} else {Session}
  C <-  if (is.null(Country)) {experimentIDs$Country} else {Country}
  eid <- dplyr::filter(experimentIDs, 
                       ExperimentName %in% EN,
                       `ExperimentID` %in% EID,
                       Session %in% S,
                       Country %in% C)
  names(eid$Country) <- eid$`ExperimentID`
  return(list(ExperimentName = eid$ExperimentName, 
              ExperimentID = eid$`ExperimentID`, 
              Session = eid$Session,
              Country = eid$Country))
}

params2dir <- function(params) {
  ps <- params %>% as.data.frame() %>%
    group_by(Session) %>%
    group_split()
  d <- character()
  for (p in ps) {
    d <- c(d,file.path(params$datadir,paste0('data_exp_', paste0(unique(p$ExperimentID),collapse = '-'),'_',p$ExperimentName[1],'_Session',unique(p$Session))))
  }
  return(d)
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
  p <- ggplot(d,aes(x = `PID`, y=Response,fill= `Participant Private ID`)) +
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
  
  p <- ggplot(d,aes(x = PID, y = Duration,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_grid(Session~Run) +
    ylab('Time to complete (s, log scale)') +
    scale_y_log10()
  
  print(p)
  
  return(d %>% select(Session, Run, UniqueName, `Participant Private ID`,`Repeat Key`, Duration))
  
}


