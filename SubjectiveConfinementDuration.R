source("helpers.R")

SubjectiveConfinementDuration <- gimmeRdata('TSDshiny/data',UniqueName = 'ConfinementTrack') %>%
  QTranslate() %>%
  filter(`Question_Key` == 'ConfDuration') %>%
  unite('Date_ConfDuration', Local_Date, Response) %>%
  mutate(Run = ifelse(Session == "S1", Run,
                      ifelse(Session == "S2", 4,
                             ifelse(Session == "S3", 5, 
                                    ifelse(Session == "SC", 1,NA))))) %>%
  pivot_wider(id_cols = c(Country, PID), names_from = c(Question_Key, Run), values_from = Date_ConfDuration, values_fn = first)%>%
  separate(col = ConfDuration_1, into = c('Local_Date_CT1','ConfDuration_CT1'),sep = '_') %>%
  separate(col = ConfDuration_2, into = c('Local_Date_CT2','ConfDuration_CT2'),sep = '_') %>%
  separate(col = ConfDuration_3, into = c('Local_Date_CT3','ConfDuration_CT3'),sep = '_') %>%
  separate(col = ConfDuration_4, into = c('Local_Date_CT4','ConfDuration_CT4'),sep = '_') %>%
  separate(col = ConfDuration_5, into = c('Local_Date_CT5','ConfDuration_CT5'),sep = '_') %>%
  mutate(across(starts_with('ConfDuration'), ~ suppressWarnings(parse_number(.x)))) %>%
  filter(across(starts_with('ConfDuration'),~ is.na(.x) | .x < 800)) %>%
  mutate(across(starts_with("Local_Date_CT"), lubridate::date)) %>%
  mutate(diff_Duration1 = as.numeric(lubridate::ddays(ConfDuration_CT2 - ConfDuration_CT1),unit = 'days'),
         diff_Date1 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT1, end = Local_Date_CT2)), unit = 'days'),
         slope1 = diff_Duration1 / diff_Date1,
         slope1 = ifelse(is.na(slope1), 1, slope1),
         diff_Duration2 = as.numeric(lubridate::ddays(ConfDuration_CT3 - ConfDuration_CT2), unit = 'days'),
         diff_Date2 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT2, end = Local_Date_CT3)), unit = 'days'),
         slope2 = diff_Duration2 / diff_Date2,
         slope2 = ifelse(is.na(slope2), 1, slope2),
         diff_Duration3 = as.numeric(lubridate::ddays(ConfDuration_CT4 - ConfDuration_CT3), unit = 'days'),
         diff_Date3 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT3, end = Local_Date_CT4)), unit = 'days'),
         slope3 = diff_Duration3 / diff_Date3,
         slope3 = ifelse(is.na(slope3), 1, slope3),
         diff_Duration4 = as.numeric(lubridate::ddays(ConfDuration_CT5 - ConfDuration_CT4), unit = 'days'),
         diff_Date4 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT4, end = Local_Date_CT5)), unit = 'days'),
         slope4 = diff_Duration4 / diff_Date4,
         slope4 = ifelse(is.na(slope4), 1, slope4)) %>%
  filter(across(starts_with('slope'),function(x){between(x,0.001,10)}))

save(SubjectiveConfinementDuration, file=file.path('TSDshiny/data',"SubjectiveConfinementDuration.RData"))

  
