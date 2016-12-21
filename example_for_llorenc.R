# Attach packages
library(dplyr)
library(EpiWeek)
library(icd)
library(cism)
library(data.table)
library(DBI)
library(ggplot2)


# Load in census data
if('census_data.RData' %in% dir()){
  load('census_data.RData')
} else {
  residency <- cism::get_data(tab = 'residency',
                              dbname = 'openhds',
                              port = 4706)
  individual <- cism::get_data(tab = 'individual',
                               dbname = 'openhds',
                               port = 4706)
  location <- cism::get_data(tab = 'location',
                             dbname = 'openhds',
                             port = 4706)
  save(residency,
       individual,
       location,
       file = 'census_data.RData')
}

# Create a time at risk dataset for the opd period
if('cleaned_time_at_risk.RData' %in% dir()){
  load('cleaned_time_at_risk.RData')
} else {
  
  # Create time at risk
  tar <- cism::create_time_at_risk(residency = residency,
                                   individual = individual,
                                   location = location)
  
  # Keep only those who are in the non-expanded zone
  # (ie, bairro <= 3499) (need to confirm with charfudin)
  tar$bairro <- as.numeric(as.character(substr(tar$locationName, 1, 4)))
  tar <- tar %>%
    filter(bairro <= 3499)
  
  # Keep only those who are < 15 (since this is the OPD cut-off)
  # at the start date
  tar <- tar %>%
    filter(as.numeric(startDate - dob) / 365.25 < 15)
  
  # Expand time at risk into a daily dataset
  etar <- cism::expand_time_at_risk(time_at_risk = tar,
                                    start_date = '2002-01-01',
                                    end_date = '2015-12-31')
  rm(tar)
  
  # Remove from the expanded table any >15 y.o. dates
  etar <- 
    etar %>%
    left_join(individual %>%
                dplyr::select(uuid,
                              dob) %>%
                mutate(dob = as.Date(dob)),
              by = c('individual_uuid' = 'uuid')) 
  etar <- data.table::data.table(etar)
  etar[,yo := as.numeric(date -dob) / 365.25]
  etar <- etar[yo < 15]
  etar <- data.frame(etar)
  
  # Save a copy for faster later use
  save(etar,
       file = 'cleaned_time_at_risk.RData')
}

# Create a daily ptar (person-time at risk) dataframe
ptar <- etar %>%
  group_by(date) %>% 
  tally

# Plot it
ggplot(data = ptar,
       aes(x = date, y = n)) +
  geom_area(alpha = 0.6,
            fill = 'orangered') +
  theme_cism() +
  labs(x = 'Date',
       y = 'Number of people',
       title = 'Person time at risk',
       subtitle = 'Only < 15 year-olds, non-expanded census area')
