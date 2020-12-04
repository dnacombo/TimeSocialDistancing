library(tidyverse)

source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  
  envir <- globalenv()
  source(tempR, local = envir, ...)
}

if (file.exists(f <- file.path(params$rootdir,'NodeKeys.csv')))   {
  allnodes <- read_csv(f,col_types = cols())
} else {
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
}


if (file.exists(f <- file.path(params$rootdir,'ExperimentIDs.csv')))   {
  ExperimentIDs <- read_csv(f,col_types = cols())
} else {
  ExperimentIDs <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1p6_WHQXNGFw2EJGny1jb5qivMy2pJ_VRRYoDGRLxgbY/edit#gid=0') %>%
    pivot_longer(cols=starts_with('Session'),names_to = 'Session', values_to = 'Experiment ID',names_prefix = 'Session',values_drop_na = T) %>%
    write_csv(f)
}

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

