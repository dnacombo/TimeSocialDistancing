
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
  if (! 'Local_Date' %in% colnames(d)) {
    stop('Must have Local_Date to add ConfinementIndices')
  }
  d %>% mutate(Hour_Of_Day = lubridate::hour(Local_Date),
               Time_Of_Day = cut(Hour_Of_Day, breaks = c(0,6,12,18,24), labels = c('night', 'morning', 'afternoon', 'evening')))
}

