
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
  tmp <- d %>% left_join(SubjectiveConfinementDuration, by= c('Country', "PID", "Session")) %>%
    mutate(is1 = Local_Date < Local_Date_CT2,
           is2 = Local_Date >= Local_Date_CT2) %>%
    mutate(ConfDuration_1 = as.numeric(ConfDuration_CT1),
           Days_since_CT1 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT1, end = Local_Date)),unit = 'days'),
           ConfDuration_1 = ConfDuration_1 + slope1 * Days_since_CT1,
           ConfDuration_2 = as.numeric(ConfDuration_CT2),
           Days_since_CT2 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT2, end = Local_Date)),unit = 'days'),
           ConfDuration_2 = ConfDuration_2 + slope2 * Days_since_CT2,
           ConfDuration = ifelse(is1, ConfDuration_1,
                                 ifelse(is2,ConfDuration_2,NA)))
  return(tmp)
}


add_TimeOfDay <- function(d){
  if (! 'Local_Date' %in% colnames(d)) {
    stop('Must have Local_Date to add ConfinementIndices')
  }
  d %>% mutate(Hour_Of_Day = lubridate::hour(Local_Date),
               Time_Of_Day = cut(Hour_Of_Day, breaks = c(0,6,12,18,24), labels = c('night', 'morning', 'afternoon', 'evening')))
}

