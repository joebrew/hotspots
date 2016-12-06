# This script creates a data.frame of time at risk 
library(readstata13)
library(dplyr)

if('time_at_risk.RData' %in% dir('data')){
  load('data/time_at_risk.RData')
} else {
  # Read in the BaseTMR data sent from Llorenc
  tmr <- readstata13::read.dta13(file = 'data/ohds_basetmr.dta')
  tmr <- data.frame(tmr)
  
  # Use only expand = 0
  tmr <-
    tmr %>% filter(expand == 0)
  
  # Manually adjust dates to not take into account anything before 2000 (# ask bea about this)
  tmr$ini_date[tmr$ini_date < as.Date('2000-01-01')] <-  as.Date('2000-01-01')
  
  # Keep only those who are <=15 at the initiation
  tmr <-
    tmr %>%
    filter(as.numeric(ini_date - date_birth) / 365.25 < 15)
  
  # Adjust exit date to be moved forward
  tmr <- tmr %>%
    filter(is.na(exit_date) |
             exit_date >= as.Date('2000-01-01'))
  
  # Define and end date (# Ask BEA about this)
  max_date <- max(tmr$exit_date, na.rm = TRUE)
  
  # Create a dataset of just individuals (no time element)
  individuals <-
    tmr %>%
    dplyr::select(perm_id, 
                  date_birth,
                  sex,
                  region,
                  family) %>%
    filter(!duplicated(perm_id))
  
  # Define a function which generates all the days necessary for a given in and out
  generate_days <- function(row_number = 1){
    
    # Subset to just the row in question
    sub_data <- tmr[row_number,]
    
    # Get the permid of this individual
    this_perm_id <- sub_data$perm_id
    
    # Get the birth day
    dob <- sub_data$date_birth
    
    # Get the start and end dates
    start_date <- sub_data$ini_date
    end_date <- sub_data$exit_date
    end_date <- if_else(is.na(end_date), max_date, end_date)
    
    # Generate the vector of dates for this episode of at risk ness
    dates <- seq(start_date,
                 end_date,
                 by = 1)
    the_rows <- data.frame(perm_id = this_perm_id,
                           date = dates)
    
    # Subset to only include up to fifteenth birthday
    bday15 <- dob + round((365.25 * 15))
    the_rows <-
      the_rows %>%
      filter(date <= bday15)
    
    # Spit back the rows
    return(the_rows)
  }
  
  # Go through each row of tmr, and generate the 
  # necessary rows for a final dataset
  n_row_tmr <- nrow(tmr)
  pb <- txtProgressBar(min = 0, max = n_row_tmr)
  for (i in 1:n_row_tmr){
    setTxtProgressBar(pb, i)
    if(i == 1){
      time_at_risk <- generate_days(row_number = i)
    } else {
      time_at_risk <-
        rbind(time_at_risk,
              generate_days(row_number = i))
    }
  }

  # Save  
  save(time_at_risk,
       file = 'data/time_at_risk.RData')
}
