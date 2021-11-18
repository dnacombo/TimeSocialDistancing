source('helpers.R')

library(lubridate)

TimeTranslate <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1jLOOtz1WV2fgbJDkD26V0TZ3Yq6u9FVOYPq54PvGre4/edit#gid=0') 
# ci-dessous pour intégrer une colonne "Country" dans la table data
l <- paramsMatch(experimentIDs = ExperimentIDs)
names(l$Country) <- l$ExperimentID

data <- gimmedata(DataDir = dirData,UniqueName = "ConfinementTrack") %>%
  mutate(Country = recode(`Experiment ID`,!!!l$Country),
         .before = Session)

if (exists('dataprocess')) rm("dataprocess")

(dataprocess <- data %>% 
    filter(Country == 'CA') %>%
    group_by(Country) %>%
    filter(!`Question Key`%in%c("BEGIN QUESTIONNAIRE","END QUESTIONNAIRE")) %>%
    pivot_wider(id_cols = c(Country,Session,UniqueName,Run,`Participant Private ID`),names_from=`Question Key`,values_from=Response) %>%
    mutate( detectsemaine = str_detect(confinement_tracker,filter(TimeTranslate, Country == Country[1]) %>% .$week),
            detectmois = str_detect(confinement_tracker,filter(TimeTranslate, Country == Country[1]) %>% .$month),
            confinement_tracker = ifelse( detectsemaine, parse_number(confinement_tracker) * 7, confinement_tracker),
            confinement_tracker = ifelse( detectmois, parse_number(confinement_tracker) * 30, confinement_tracker),
            ct1 = parse_date_time(confinement_tracker, orders = '%j'),
            ct2 = as.duration(ct1 - as.POSIXct(dmy('01/01/2021'))),
            confinement_tracker = as.numeric(ct2,'days')+1,
           .after=confinement_tracker)) %>%
  select(-ct1,-ct2,-starts_with('detect'))




ggplot(dataprocess,aes(x=confinement_tracker,fill=Country)) +
  geom_histogram(position=position_dodge(.5))
