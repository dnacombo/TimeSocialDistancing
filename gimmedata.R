gimmedata <- function(DataDir = getwd(), ExperimentID = '[0-9]{5}', ExperimentName = '.*', UniqueName = '.*', Session = '.*', Run = '.*', file = '',  clean = T, verbose = T, progressbar = T) {
  
  if (file != '') {
    if (! file.exists(file)) {stop('File provided does not exist')}
    fs <- file
  } else {
    p <- paste0('data_exp_([0-9]{5}-)*(', ExperimentID, ')(-[0-9]{5})*', '_(', ExperimentName, ')(_Session', Session, ')?')
    d <- list.files(path = DataDir, pattern = p,full.names = T)
    if (verbose) {
      cat(paste0('Loading data from ', str_replace(d,DataDir,'')),sep = '\n')
    }
    
    p <- paste0('S',Session,'_', UniqueName, '(_r',Run,')?.csv')
    fs <- list.files(path = file.path(d),pattern = p,full.names = T)
  }
  if (length(fs) == 0) { stop(paste0('Could not find data (', p, ')'))}
  
  d <- tibble()
  if (progressbar) {
    pb <- txtProgressBar(min = 0, max = length(fs), initial = 0, char = "=",
                         width = 40, style = 3)
  }
  i <- 0
  for (f in fs) {
    if (progressbar) {
      i <- i + 1
      setTxtProgressBar(pb,i)
    }
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
