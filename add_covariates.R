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
                      JP = 'Japan', 
                      US = 'United States'
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

add_Demographics <- function(d) {
  load(file = file.path('TSDshiny/data','Demographics.RData'))
  
  d %>% 
    left_join(Demographics %>% select(-Session,-Country, -UTC_Date), by = 'PID')
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
  tmp <- d %>% left_join(SubjectiveConfinementDuration) %>%
    mutate(is1 = Local_Date < Local_Date_CT2,
           is2 = Local_Date >= Local_Date_CT2,
           is3 = Local_Date >= Local_Date_CT3,
           is4 = Local_Date >= Local_Date_CT4) %>%
    mutate(ConfDuration_1 = as.numeric(ConfDuration_CT1),
           Days_since_CT1 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT1, end = Local_Date)),unit = 'days'),
           ConfDuration_1 = ConfDuration_1 + slope1 * Days_since_CT1,
           ConfDuration_2 = as.numeric(ConfDuration_CT2),
           Days_since_CT2 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT2, end = Local_Date)),unit = 'days'),
           ConfDuration_2 = ConfDuration_2 + slope2 * Days_since_CT2,
           ConfDuration_3 = as.numeric(ConfDuration_CT3),
           Days_since_CT3 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT3, end = Local_Date)),unit = 'days'),
           ConfDuration_3 = ConfDuration_3 + slope3 * Days_since_CT3,
           ConfDuration_4 = as.numeric(ConfDuration_CT4),
           Days_since_CT4 = as.numeric(lubridate::as.duration(lubridate::interval(start = Local_Date_CT4, end = Local_Date)),unit = 'days'),
           ConfDuration_4 = ConfDuration_4 + slope4 * Days_since_CT4,
           ConfDuration = ifelse(is1, ConfDuration_1,
                                 ifelse(is2, ConfDuration_2,
                                        ifelse(is3, ConfDuration_3,
                                               ifelse(is4, ConfDuration_4, NA))))) %>%
    select(-(Local_Date_CT1:Days_since_CT4))
  return(tmp)
}


add_TimeOfDay <- function(d){
  if (! 'Local_Date' %in% colnames(d)) {
    stop('Must have Local_Date to add ConfinementIndices')
  }
  d %>% mutate(Hour_Of_Day = lubridate::hour(Local_Date),
               Time_Of_Day = cut(Hour_Of_Day, breaks = c(0,6,12,18,24), labels = c('night', 'morning', 'afternoon', 'evening')))
}

add_Mobility <- function(d){
  if (! 'Local_Date' %in% colnames(d)) {
    stop('Must have Local_Date to add mobility indices')
  }
  
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
                      JP = 'Japan', 
                      US = 'United States'
  )
  
  # mobility <- read_csv('TSDshiny/data/Global_Mobility_Report.csv', col_types = cols(
  #   country_region_code = col_character(),
  #   country_region = col_character(),
  #   sub_region_1 = col_character(),
  #   sub_region_2 = col_character(),
  #   metro_area = col_logical(),
  #   iso_3166_2_code = col_character(),
  #   census_fips_code = col_logical(),
  #   place_id = col_character(),
  #   date = col_date(format = ""),
  #   retail_and_recreation_percent_change_from_baseline = col_double(),
  #   grocery_and_pharmacy_percent_change_from_baseline = col_double(),
  #   parks_percent_change_from_baseline = col_double(),
  #   transit_stations_percent_change_from_baseline = col_double(),
  #   workplaces_percent_change_from_baseline = col_double(),
  #   residential_percent_change_from_baseline = col_double()  )) %>%
  #   filter(country_region %in% countryMapping,
  #          is.na(sub_region_1),
  #          is.na(sub_region_2)) %>%
  #   transmute(Country = country_region,
  #             Day = lubridate::date(date),
  #             Mobility_Transit = transit_stations_percent_change_from_baseline,
  #             Mobility_Retail = retail_and_recreation_percent_change_from_baseline,
  #             Mobility_Parks = parks_percent_change_from_baseline,
  #             Mobility_WorkPlaces = workplaces_percent_change_from_baseline,
  #             Mobility_Residential = residential_percent_change_from_baseline
  #             ) %>%
  # write_csv('TSDshiny/data/MyGlobal_Mobility_Report.csv')
  
  
  mobility <- read_csv('TSDshiny/data/MyGlobal_Mobility_Report.csv', col_types = cols(
    Country = col_character(),
    Day = col_date(format = ""),
    Mobility_Transit = col_double(),
    Mobility_Retail = col_double(),
    Mobility_Parks = col_double(),
    Mobility_WorkPlaces = col_double(),
    Mobility_Residential = col_double()
  ))
  
  d <- d %>% 
    mutate(Day = lubridate::date(Local_Date)) %>%
    mutate(Country2 = recode(Country,!!!countryMapping),.before = 1) %>%
    left_join(mobility, by = c('Day', Country2 = 'Country')) %>%
    select(-Country2, -Day)
}
