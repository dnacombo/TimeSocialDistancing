# This is the Blursday database paper source code.
# This code was used to prepare the following article:
# XXX Ref. to add XXX
#
# The live application of the server can be accessed at 
# https://dnacombo.shinyapps.io/Blursday/
# This code is publicly available at
# https://github.com/dnacombo/TSDshiny

#     Copyright (C) 2021  Maximilien Chaumon
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)
library(emmeans)
theme_set(theme_minimal() +
            theme(axis.text = element_text(size = 7),
                  title = element_text(size = 7),
                  legend.text = element_text(size = 7),
                  legend.key.size = unit(4,'mm')))
options(dplyr.summarise.inform=F)
options(gargle_oauth_email = 'maximilien.chaumon@gmail.com')

dirData <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'
dirBlursday <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/TSDshiny/data'

Palette_Session <- c(S1 = '#FF73AD',
                         S2 = '#6BD685',
                         S3 = '#BCE4C6',
                         S4 = '#FCC0D9',
                         SC = '#8E8C8C')
Palette_SessionS1SC <- Palette_Session[names(Palette_Session) %in% c('S1','SC')]
Palette_Mobility_Transit <- c(high = '#9D83EF', low = '#CFC0FF')
Palette_Stringency_Index <- c(high = '#FFD45B', low = '#F7ECCD')


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

source('gimmedata.R')
gsheet2tbl <- function (url) {
  suppressMessages(read_csv(file=gsheet::gsheet2text(url, format='csv'), col_types = cols(.default = col_character())))
}

file.hide <- function(f) {
  d <- dirname(f)
  f <- basename(f)
  hiddenf = file.path(d,paste0('.',f))
  f <- file.path(d,f)
  file.rename(f,hiddenf)
}
file.unhide <- function(f){
  d <- dirname(f)
  f <- basename(f)
  originalf <- file.path(d,str_replace(f,'^\\.',''))
  f <- file.path(d,f)
  if (file.exists(originalf)) {
    warning(paste0('Cannot unhide ', f, ', regular file with same name exists'))
  }
  else
  {
    file.rename(f,originalf)
  }
}

QTranslate <- function(orig) {
  
  QTranslateOrMap <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1bwKj-ngDrHFVXpSD13l183FHsXoZu1HqQmz9ZtYROYM/edit#gid=1012544807') %>%
    select(-Comment)
  
  # The tables of previously translated materials (those we recompute now will be merged with these)
  QToTranslate <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1YOZ_3MMdo7ghgdIyhxgWqE8WYsB_wFihlEAVhODkz7Q/edit#gid=1845970270') %>%
    select(-Comment) %>%
    left_join(QTranslateOrMap, by = c('UniqueName', Question = 'Standard'), suffix = c('','_Key')) %>%
    select(-Question_Key) %>%
    distinct()
  
  QToMap <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1GKqRiMRrqHv2BT524nFqOlHYuSrfAuTi5ypLmBKiyEw/edit#gid=1785683302') %>%
    pivot_longer(cols = 4:last_col(), names_to = 'Country', values_to = 'Response') %>%
    left_join(QTranslateOrMap, by = c('UniqueName', Question = 'Standard'), suffix = c('','_Key')) %>%
    distinct()
  
  Question_Key_recoder <- QTranslateOrMap$Standard
  names(Question_Key_recoder) <- QTranslateOrMap$Question
    
  toto <- orig %>%
    mutate(Question_Key = recode(Question_Key,!!!Question_Key_recoder)) %>%
    left_join(QToTranslate, by = c('Country', 'Unique_Name' = 'UniqueName', 'Question_Key' = 'Question', 'Response')) %>%
    mutate(Response = ifelse(is.na(Translated),Response,Translated)) %>%
    select(-(Translated:last_col())) %>%
    left_join(QToMap, by = c('Country', 'Unique_Name' = 'UniqueName', 'Question_Key' = 'Question', 'Response')) %>%
    mutate(Response = ifelse(is.na(`EN-GB`),Response,`EN-GB`)) %>%
    select(-(`EN-GB`:last_col()))
}
TTranslate <- function(orig) {
  
  TTranslateOrMap <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1pDTfJUJnFoUxUbEnOSQrACg0vqSDv2czC52_6fM9Ozc/edit#gid=0') %>%
    select(-Comment)
  # The tables of previously translated materials (those we recompute now will be merged with these)
  TToTranslate <- gsheet2tbl('https://docs.google.com/spreadsheets/d/16pewaHuHCu8YStxHvF9Nis_RZOB3hT4utYLm5T9DSS4/edit#gid=516354689') %>%
    select(-Comment)
  
  TToMap <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1FrAebQ2Y9PQVMx-omsCbhkosmXJT371NNBWr_QhU4OU/edit#gid=36287472') %>%
    pivot_longer(cols = 4:last_col(), names_to = 'Country', values_to = 'Value')
  
  tmp <- left_join(orig,TToTranslate, by = c('Country', Unique_Name = 'UniqueName', 'Response' = 'Value')) %>%
    mutate(Response = ifelse(is.na(Translated),Response,Translated)) %>%
    select(-(Question:last_col())) %>%
    left_join(QToMap, by = c('Country', 'UniqueName', 'Question Key', 'Response')) %>%
    mutate(Response = ifelse(is.na(`EN-GB`),Response,`EN-GB`)) %>%
    select(-(Question:last_col()))
}

UpdateTables <- function(datadir = dirData, wherefrom='online') {
  
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

    allnodes.S4 <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=1241468640') %>%
      filter(prefix != 'Comment')
    
    allnodes.SC <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1Mwy2aGCJ6vSpp4a32NOs83e2H73MQRFUOL_193yb8sQ/edit#gid=644252489') %>%
      filter(prefix != 'Comment')
    
    (allnodes <- bind_rows(`1` = allnodes.S1,
                           `2` = allnodes.S2,
                           `3` = allnodes.S3,
                           `4_Confinement2` = allnodes.S4,
                           `Control` = allnodes.SC,
                           .id = "Session")%>%
        mutate_all(.funs = ~ na_if(.,'N/A'))) %>%
      write_csv(f)
    
    rm(allnodes.S1,allnodes.S2,allnodes.S3,allnodes.S4,allnodes.SC)
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
list2env(UpdateTables(dirData), envir = globalenv())


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

paramsMatch <- function(params = NULL, ExperimentName = NULL, ExperimentID = NULL, Session = NULL, Country = NULL, experimentIDs = experimentIDs) {
  
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
    d <- c(d,file.path(dirData,paste0('data_exp_', paste0(unique(p$ExperimentID),collapse = '-'),'_',p$ExperimentName[1],'_Session',unique(p$Session))))
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
    group_by(Country,Session,Run, UniqueName,`Experiment ID`,`Repeat Key`) %>%
    mutate(Duration = as.numeric(`Reaction Time`)/1000)
  
  p <- ggplot(d,aes(x = PID, y = Duration,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_grid(Session~Run, labeller = label_both) +
    ylab('Time to complete (s, log scale)') +
    scale_y_log10()
  
  print(p)
  
  return(d %>% select(Country,Session, Run, UniqueName, `Participant Private ID`,`Repeat Key`, Duration))
  
}


library(gtable)
library(cowplot)

shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

polmer <- function(formula, data, lnk = "logit",  ...){
  if(require('lme4', character.only = TRUE)) invisible() else stop("need lme4 package installed!\n")
  # Get fixed and random terms
  fxForm <- lme4:::nobars(formula)
  fxForm[[2]] <- NULL
  rTerms <- lme4:::findbars(formula)
  ranTerms <- sapply(rTerms, deparse)
  fixTerms <- labels(terms(fxForm))
  ranNames <- as.vector(sapply(ranTerms, function(x)
    strsplit(x, "\\| ")[[1]][2]
  ))
  LeftRanNames <- as.vector(sapply(ranTerms, function(x)
    strsplit(x, " | ", fixed = TRUE)[[1]][1]
  ))
  NoRanEff <- !(length(ranTerms) > 0)
  
  
  # Make fixed-effect model matrix
  Resp <- ordered(data[[as.character(formula[[2]])]])
  Cuts <- as.numeric(levels(Resp)[-length(levels(Resp))])
  cumRat <- as.vector(sapply(Cuts, 
                             function(x) Resp <= x))
  fRat <- gl(length(Cuts), nrow(data), 
             nrow(data) * length(Cuts))
  X <- model.matrix(~ fRat - 1)
  labs <- sapply(Cuts, function(x)
    paste(x, "|", x+1, sep = "")
  )
  colnames(X) <- labs
  
  fX <- -model.matrix(fxForm, data)[, -1]
  fX.names <- if (inherits(fX, "matrix")) colnames(fX) else paste(fixTerms, levels(data[[fixTerms]])[2], sep = "")
  fX <- kronecker(matrix(rep(1, length(Cuts)), ncol = 1),
                  fX)
  X <- cbind(X, fX)
  colnames(X)[-seq(length(Cuts))] <- fX.names
  p.df <- data.frame(cumRat = cumRat, X = X)
  names(p.df) <- c("cumRat", colnames(X))
  
  # Make random-effect model vectors
  frm <- if(!NoRanEff){
    tmp <- sapply(seq_len(length(ranNames)), 
                  function(x)
                    rep(data[[ranNames[x]]], length(Cuts)))
    
    for(ix in seq_len(ncol(tmp))) 
      assign(ranNames[ix], tmp[, ix])
    rxForm <- paste(paste("(", ranTerms, ")", 
                          sep = "", collapse = " + "), " - 1")
    as.formula(paste("cumRat ~ .  + ", rxForm))
  } else as.formula("cumRat ~ . - 1")
  for(ix in LeftRanNames)
    if(ix != "1") 
      assign(ix, rep(data[[ix]], each = length(Cuts)))
  
  CLL <- if (NoRanEff)
    substitute(
      glm(FRM, data = p.df, 
          family =  binomial(LNK), ...), 
      list(FRM = frm, LNK = lnk)) 
  else
        substitute(
          glmer(FRM, data = p.df, 
                family =  binomial(LNK), ...),
          list(FRM = frm, LNK = lnk))	
  res <- eval(CLL)
  #	if (inherits(res, "mer")) print(res, cor = cor) else
  #		print(res)
  res
}