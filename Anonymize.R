datadir <- '/home/maximilien.chaumon_local/ownCloud/Lab/00-Projects/TimeSocialDistancing/TSDshiny/data'

source('helpers.R')


# hide all Welcome questionnaires
# (contain emails and publicIDs)

fs <- list.files(path = datadir, pattern = '^TSD_.*_Welcome.RData', recursive = T, full.names = T)
tmp <- sapply(fs, file.hide)
  

# extract all useful demographics info then hide files
# (contain date of birth)

# check that files were not hidden before.
# for all hidden files found, check if original (not hidden) exists
# if also original exists, then delete hidden file
# if no original exists, then rename hidden file to original
fs <- list.files(path = datadir, pattern = '^\\.TSD_.*_Demographics.RData', recursive = T, full.names = T, all.files = T)
tmp <- sapply(fs, file.unhide)

library(lubridate)
demo <- gimmeRdata(DataDir = datadir, UniqueName = 'Demographics')

fs <- list.files(path = datadir, pattern = '^TSD_.*_Demographics.RData', recursive = T, full.names = T)
tmp <- sapply(fs, file.hide)

gsheet2tbl <- function (url) {
  read_csv(file=gsheet::gsheet2text(url, format='csv'), col_types = cols(.default = col_character()))
}

Keys <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1bwKj-ngDrHFVXpSD13l183FHsXoZu1HqQmz9ZtYROYM/edit#gid=1012544807') %>%
  filter(UniqueName == 'Demographics')

recoder <- Keys$Standard
names(recoder) <- Keys$Question

HandednessMap <- read_csv('Δεξιόχειρας,right-handed
Αριστερόχειρας,left-handed
Αμφιδέξιος,ambidextrous
Sağlak,right-handed
İki elini de kullanabilen,ambidextrous
Solak,left-handed
Derecha,right-handed
Right-handed,right-handed
Left-handed,left-handed
Ambidextrous,ambidextrous
Rechtshändig,right-handed
Linkshändig,left-handed
Beidhändig,ambidextrous
右利き,right-handed
左利き,left-handed
両利き,ambidextrous
Droite,right-handed
Gauche,left-handed
Ambidextre,ambidextrous
Ambidestro,ambidextrous
Ambidiestro,ambidextrous
Destrimane,right-handed
Izquierda,left-handed
Mancino,left-handed', col_names = F, show_col_types = F)
recodeHandedness <- HandednessMap$X2
names(recodeHandedness) <- HandednessMap$X1

Demo <- demo %>%
  mutate(`Question_Key` = recode(`Question_Key`,!!!recoder)) %>%
  select(`Experiment_ID`, `PID`, Session, `Question_Key`, `Response`, `UTC_Date`) %>%
  group_by(Experiment_ID) %>%
  mutate(Country = as.factor(ID2Country(Experiment_ID,ExperimentIDs = ExperimentIDs))) %>%
  ungroup() %>%
  mutate(Question_Key = recode(Question_Key,
                                 `dob-year` = "Age-year",
                                 `dob-month` = "Age-month")) %>%
  filter(Question_Key %in% c("Age-year", "Age-month", "sex-quantised", "Handedness", "DOB")) %>%
  mutate(UTC_Date = floor_date(ymd_hms(UTC_Date), unit = 'days')) %>%
  pivot_wider(id_cols = c(Country,Session,PID, UTC_Date), values_from = Response, names_from = Question_Key, values_fn = first) %>%
  mutate(Sex = as.factor(recode(`sex-quantised`,`1`='M',`2`='F',`2`='W')),
         Handedness = as.factor(recode(Handedness,!!!recodeHandedness)),
         Age = as.numeric(dyears(abs(as.numeric(`Age-year`))) + dmonths(abs(as.numeric(`Age-month`))),'years'),
         DOB2 =  floor_date(UTC_Date - dyears(Age), unit = 'days'))
Demo$DOB2[is.na(Demo$DOB2)] <- parse_date_time(Demo$DOB[is.na(Demo$DOB2)],orders = '%d.%m.%y',truncated = 1)
Demographics <- Demo %>%
  select(-DOB) %>%
  rename(DOB = DOB2) %>%
  mutate(Age = as.numeric(as.period(interval(end=as.Date(UTC_Date), start = as.Date(DOB))),'years'),
         Age = ifelse(between(Age,5,90),Age,NA),
         Age = round(Age, digits = 1),
         Session = as.factor(Session),
         PID = as.factor(PID)) %>%
  select(-`Age-month`, -`Age-year`, -`sex-quantised`, -DOB) %>%
  filter(Age >= 18)

save(Demographics,file = file.path(datadir,'Demographics.RData'))



