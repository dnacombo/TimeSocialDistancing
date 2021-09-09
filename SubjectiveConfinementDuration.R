source("helpers.R")

SubjectiveConfinementDuration <- gimmeRdata('TSDshiny/data',UniqueName = 'ConfinementTrack') %>%
  QTranslate() %>%
  filter(`Question_Key` == 'ConfDuration') %>%
  unite('Date_ConfDuration', Local_Date, Response) %>%
  pivot_wider(id_cols = c(Country, Session, PID), names_from = c(Question_Key, Run), values_from = Date_ConfDuration, values_fn = first) %>%
  separate(col = ConfDuration_1, into = c('Local_Date_CT1','ConfDuration_CT1'),sep = '_') %>%
  separate(col = ConfDuration_2, into = c('Local_Date_CT2','ConfDuration_CT2'),sep = '_') %>%
  separate(col = ConfDuration_3, into = c('Local_Date_CT3','ConfDuration_CT3'),sep = '_') %>%
  separate(col = ConfDuration_, into = c('Local_Date_CT','ConfDuration_CT'),sep = '_') %>%
  mutate(ConfDuration_CT1 = ifelse(!is.na(ConfDuration_CT),ConfDuration_CT,ConfDuration_CT1),
         Local_Date_CT1 = ifelse(!is.na(Local_Date_CT),Local_Date_CT,Local_Date_CT1)) %>%
  select(-Local_Date_CT, -ConfDuration_CT) %>%
  mutate(across(starts_with("Local_Date_CT"), lubridate::date))

save(SubjectiveConfinementDuration, file=file.path('TSDshiny/data',"SubjectiveConfinementDuration.RData"))

  
##################################################################################################################################################



# Function add_ConfinementIndices

