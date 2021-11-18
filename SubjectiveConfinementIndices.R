source('helpers.R')

SubjectiveConfinementIndices <-  gimmeRdata(dirBlursday, UniqueName = 'UCLA') %>%
  QTranslate() %>%
  filter(!`Question_Key`%in%c("BEGIN QUESTIONNAIRE","END QUESTIONNAIRE")) %>%
  pivot_wider(id_cols = c(Country, Session, Run, PID),names_from=`Question_Key`,values_from=Response, values_fn = first) %>%
  mutate(Reported_Loneliness = as.numeric(`OSRN-2-quantised`) + as.numeric(`OSRN-4-quantised`),
         Felt_Loneliness = as.numeric(`OSRN-11-quantised`) + as.numeric(`OSRN-14-quantised`),
         Subjective_Confinement = Reported_Loneliness + Felt_Loneliness) %>%
  select(Country, Session, PID, Reported_Loneliness, Felt_Loneliness, Subjective_Confinement)

save(SubjectiveConfinementIndices, file=file.path(dirBlursday,"SubjectiveConfinementIndices.RData"))
