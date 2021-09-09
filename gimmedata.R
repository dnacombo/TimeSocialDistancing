library(tidyverse)
gimmedata <- function(DataDir = getwd(), ExperimentID = '[0-9]{5}', ExperimentName = '.*', UniqueName = '.*', Session = '.*', Run = '.*', file = '',  clean = T, verbose = F, progress = T, clap = F)
  {
  
  if (file != '') {
    if (! file.exists(file)) {stop('File provided does not exist')}
    fs <- file
  } else {
    p <- paste0('data_exp_([0-9]{5}-)*(', ExperimentID, ')(-[0-9]{5})*', '_(', ExperimentName, ')(_Session', Session, ')')
    d <- list.files(path = DataDir, pattern = p,full.names = T)
    if (verbose) {
      cat(paste0('Loading data from ', str_replace(d,DataDir,'')),sep = '\n')
    }
    
    p <- paste0('S',Session,'_', UniqueName, '(_r',Run,')?.csv')
    fs <- list.files(path = file.path(d),pattern = p,full.names = T)
  }
  if (length(fs) == 0) { stop(paste0('Could not find data (', p, ')'))}
  
  d <- tibble()
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(fs), initial = 0, char = "=",
                         width = 40, style = 3)
  }
  i <- 0
  for (f in fs) {
    if (progress) {
      i <- i + 1
      setTxtProgressBar(pb,i)
    }
    FF <- str_match_all(basename(f),'(S[^_]*)_([^_]*)_?r?([^\\.]*)?.csv')
    Session <- FF[[1]][2]
    UniqueName <- FF[[1]][3]
    Run <- FF[[1]][4]
    
    if (verbose) {cat(paste0('Loading ',str_replace(f,DataDir,'')),sep = '\n')}
    
    tmp <- read_csv(f,col_types = cols(.default = col_character()), n_max = ifelse(clap,1,Inf), progress = F) %>%
      mutate(Session = as.character(Session),
             UniqueName = as.character(UniqueName),
             Run = as.character(Run))
    if (clap) {
      tmp <- tmp %>%
        mutate(filename = str_replace(f, DataDir,''))
    }
    if (clean){
      tmp <- tmp %>% select(-starts_with('order-'),
                            -starts_with('checkpoint-'),
                            -starts_with('branch-'),
                            -matches('Schedule ID'),
                            -ends_with('Timestamp'),
                            -starts_with('Participant'),
                            `Participant Private ID`,
                            -`Tree Node Key`,
                            -starts_with('Spreadsheet'),
                            matches('Spreadsheet Name'))
    }
    d <- bind_rows(d,tmp)
  }
  if (progress) cat('\n')
  d %>% select(Session,UniqueName,Run,matches('PID'),everything()) %>%
    mutate(`UTC Date` = lubridate::dmy_hms(`UTC Date`),
           `Local Date` = lubridate::dmy_hms(`Local Date`))
}

gimmeRdata <- function(DataDir = getwd(), UniqueName = '[^_]+', 
                       ExperimentID = '[0-9]{5}', Country = '[^_]+', 
                       Session = 'S[^_]+', Run = '[^_]*', ...,
                       progress = F,
                       fast = T,
                       verbose = T)
  {
  
  humanReadable <- function (x, units = "auto", standard = c("IEC", "SI", "Unix"), 
                             digits = 1, width = NULL, sep = " ", justify = c("right", 
                                                                              "left")) 
  {
    suffix.SI <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", 
                   "ZB", "YB")
    suffix.IEC <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", 
                    "EiB", "ZiB", "YiB")
    suffix.Unix <- c("B", "K", "M", "G", "T", "P", "E", "Z", 
                     "Y")
    standard <- match.arg(standard)
    if (length(justify) == 1) 
      justify <- c(justify, justify)
    .applyHuman <- function(x, base, suffix, digits, width, 
                            sep) {
      n <- length(suffix)
      i <- pmax(pmin(floor(log(x, base)), n - 1), 0)
      if (!is.finite(i)) 
        i <- 0
      x <- x/base^i
      if (is.null(width)) 
        x <- format(round(x = x, digits = digits), nsmall = digits)
      else {
        lenX <- nchar(x)
        if (lenX > width) {
          digits <- pmax(width - nchar(round(x)) - 1, 
                         0)
        }
        if (i == 0) 
          digits <- 0
        x <- round(x, digits = digits)
      }
      c(x, suffix[i + 1])
    }
    if (any(x < 0)) 
      stop("'x' must be positive")
    if (standard == "SI") {
      suffix <- suffix.SI
      base <- 10^3
    }
    else if (standard == "IEC") {
      suffix <- suffix.IEC
      base <- 2^10
    }
    else {
      suffix <- suffix.Unix
      base <- 2^10
    }
    if (!missing(units) && units == "bytes") {
      retval <- rbind(x, "bytes")
    }
    else if (!missing(units) && units != "auto") {
      units <- suffix[match(toupper(units), toupper(suffix))]
      power <- match(units, suffix) - 1
      X <- x/(base^power)
      X <- format.default(x = X, digits = digits, nsmall = digits)
      retval <- rbind(X, rep(units, length(X)))
    }
    else retval <- sapply(X = x, FUN = ".applyHuman", base = base, 
                          suffix = suffix, digits = digits, width = width, sep = sep)
    if (all(justify == "none")) 
      paste(trimws(retval[1, ]), trimws(retval[2, ]), sep = sep)
    else paste(format(trimws(retval[1, ]), justify = justify[1]), 
               format(trimws(retval[2, ]), justify = justify[2]), sep = sep)
  }
  
  if (length(UniqueName) > 1){
    UniqueName <- paste0('(',paste(UniqueName,collapse = '|'),')')
  }
  if (length(Country) > 1) {
    Country <- paste0('(',paste(Country,collapse = '|'),')')
  }
  if (length(Session) > 1) {
    Session <- paste0('(',paste(Session,collapse = '|'),')')
  }
  
  # if (file.exists(file.path(DataDir,'alldata.db'))) {
  #   con <- dbConnect(SQLite(), file.path(DataDir,'alldata.db'))
  #   allUniqueNames <- dbListTables(con)[!grepl('^sqlite_',allUniqueNames)]
  #   
  #   UniqueNames = allUniqueNames[str_detect(allUniqueNames,UniqueName)]
  #   dd <- tibble()
  #   for (UniqueName in UniqueNames) {
  #     d <- tbl(con, UniqueName)
  # 
  #     dd <- bind_rows(dd,
  #                     d %>%
  #                       filter(stringr::str_detect(Country, !!Country),
  #                              stringr::str_detect(Session, !!Session),
  #                              # stringr::str_detect(Experiment_ID, !!ExperimentID),
  #                              stringr::str_detect(Run, !!Run),
  #                              ...) %>%
  #                       collect()
  #     )
  #   }
  #   return(dd)
  # }
  
  fs <- list.files(path = DataDir, pattern = paste0('^TSD_',Country,'_',Session,'_',UniqueName,'.RData'), full.names = T, recursive = F)
  if (verbose) {
    cat(paste0('Loading ', humanReadable(sum(file.info(fs)$size)), ' from ', length(fs), ' files.\n'))
  }
  if (length(fs) == 0) return(tibble())
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(fs), initial = 0, char = "=",
                         width = 40, style = 3)
    i <- 0
  }
  if (fast) {
    d <- sapply(fs,
                function(x){
                  if (progress) {
                    i <<- i + 1
                    setTxtProgressBar(pb,i)
                  }
                  mget(load(x))
                })
    D <- bind_rows(d)%>%
      filter(grepl(!!ExperimentID,Experiment_ID),
             grepl(!!Run,Run))
    
  } else {
    D <- tibble()
    for (f in fs) {
      load(f)
      D <- d %>%
        filter(grepl(!!ExperimentID,Experiment_ID),
               grepl(!!Run,Run)) %>%
        bind_rows(D)
      if (progress) {
        i <- i + 1
        setTxtProgressBar(pb,i)
      }
    }
  }
  if (verbose) {
    cat(paste0('Done.\n'))
  }
  return(D)
}


add_StringencyIndex <- function(d)
  {
  
  countryMapping <- c(FR = 'France',
                      DE = 'Germany',
                      IT = 'Italy',
                      TR = 'Turkey',
                      AR = 'Argentina',
                      UK = 'United Kingdom',
                      CA = 'Canada',
                      CO = 'Colombia',
                      GR = 'Greece', 
                      IN = 'India',
                      JP = 'Japan'
  )
  
  stringencyIndex <- read_csv('TSDshiny/data/covid-stringency-index.csv',
                              col_types = cols(
                                Entity = col_character(),
                                Code = col_character(),
                                Day = col_date(format = ""),
                                stringency_index = col_double())) %>%
    filter(Entity %in% countryMapping) %>%
    rename(Stringency_Index = stringency_index)
  
  d <- d %>% 
    mutate(Day = lubridate::date(Local_Date)) %>%
    mutate(Country2 = recode(Country,!!!countryMapping),.before = 1) %>%
    left_join(stringencyIndex, by = c('Day', Country2 = 'Entity')) %>%
    select(-Country2, -Code, -Day)
  
}

add_demographics <- function(d) {
  load(file = file.path('TSDshiny/data','Demographics.RData'))
  
  d %>% 
    left_join(Demographics, by = 'PID')
}

add_SubjectiveConfinementIndices <- function(d){
  load(file=file.path('TSDshiny/data',"SubjectiveConfinementIndices.RData"))
  d %>% left_join(SubjectiveConfinementIndices, by= c('Country','PID', 'Session'))
}

add_SubjectiveConfinementDuration <- function(d){
  if (! 'Local_Date' %in% colnames(d)) {
    stop('Must have Local_Date to add ConfinementIndices')
  }
  load(file=file.path('TSDshiny/data',"SubjectiveConfinementDuration.RData"))
  d %>% left_join(SubjectiveConfinementDuration, by= "PID") %>%
    pivot_longer(cols = starts_with("Local_Date_CT"),names_to = 'CT',values_to = )
    mutate(across(starts_with("Local_Date_CT"), ~ lubridate::ddays(lubridate::as.duration(lubridate::interval(end=Local_Date, start = .x))), .names = 'diff_{.col}'),
           min_diff_Local_Date = min(c(diff_Local_Date_CT1, diff_Local_Date_CT2, diff_Local_Date_CT3), na.rm = T )) %>%
    select(everything(), Local_Date, ends_with('CT1'), ends_with('CT2'), ends_with('CT3'))
}


add_TimeOfDay <- function(d){
  d %>% mutate(Hour_Of_Day = lubridate::hour(Local_Date),
               Time_Of_Day = cut(Hour_Of_Day, breaks = c(0,6,12,18,24), labels = c('night', 'morning', 'afternoon', 'evening')))
}

