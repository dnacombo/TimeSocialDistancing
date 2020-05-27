
Q_cols <- cols(`Participant Private ID` = col_factor())


Q_read <- function(datafiles) {
  
  orig <- tibble()
  for (f in datafiles) {
    orig <- read_csv(f,col_types = cols()) %>%
      mutate(File = basename(f))%>%
      bind_rows(orig)
  }
  
  orig %>%
    mutate(`Participant Private ID` = as.factor(`Participant Private ID`))
}
Q_Nsuj <- function(orig){
  
  N <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    group_by(File) %>%
    dplyr::summarize(N = n())
}

Q_Complete <- function(orig) {
  d <- orig %>% filter(`Question Key` %in% 'END QUESTIONNAIRE') %>%
    group_by(File) %>%
    mutate(Response = as.numeric(Response)/1000)
  p <- ggplot(d,aes(x = `Participant Private ID`, y=Response,fill= `Participant Private ID`)) +
    geom_col(show.legend = F) +
    theme(axis.text.x = element_text(angle = 30,hjust = 1)) +
    facet_wrap(~File) +
    ylab('Time to complete (s)')
  print(p)
  
}