# This script creates a data.frame of time at risk 
# Must first run "clean_opd.R"
library(readstata13)
library(dplyr)
library(ggplot2)
library(tidyr)

# Define days out of eligibility for both diarrhea and malaria
days_out_malaria <- 28
days_out_diarrhea <- 14

# Define the minimum date of opd
min_opd_date <- min(opd$date)

if('time_at_risk.RData' %in% dir('data')){
  load('data/time_at_risk.RData')
} else {
  # Read in the BaseTMR data sent from Llorenc
  tmr <- readstata13::read.dta13(file = 'data/ohds_basetmr.dta')
  tmr <- data.frame(tmr)
  
  # Use only expand = 0
  tmr <-
    tmr %>% filter(expand == 0)
  
  # Manually adjust dates to not take into account anything before opd
  tmr$ini_date[tmr$ini_date < min_opd_date] <-  min_opd_date
  
  # Manually adjust exit dates to only go up to the end of observation
  # ask BEA about this
  max_date <- max(tmr$exit_date, na.rm = TRUE)
  tmr$exit_date[is.na(tmr$exit_date)] <- max(tmr$exit_date, na.rm = TRUE)
  
  # Remove any exits prior to the study period
  tmr <- tmr %>%
    filter(exit_date >= min_opd_date)
  
  # Keep only those who are <=15 at the time
  tmr <-
    tmr %>%
    filter(as.numeric(ini_date - date_birth) / 365.25 < 15)
  
  # Create a dataset of just individuals (no time element)
  individuals <-
    tmr %>%
    dplyr::select(perm_id, 
                  date_birth,
                  sex,
                  region,
                  family) %>%
    filter(!duplicated(perm_id))
  
  # Remove those with no permid # ASK BEA ABOUT THIS
  opd <- opd %>%
    filter(!is.na(perm_id))
  
  # Create dataframe for aggregated time at risk
  
  # Get aggregated time at risk
  atar <- 
    data.frame(date = seq(min(tmr$ini_date),
                          max(tmr$exit_date),
                          by = 1))
  atar$malaria <- 0
  atar$diarrhea <- 0
  
  # Try simple loop approach
  for (i in 1:nrow(atar)){
    atar$malaria[i] <-
      atar$diarrhea[i] <- tmr %>%
      filter(ini_date <= atar$date[i],
             exit_date >= atar$date[i]) %>%
      summarise(x = n()) %>%
      .$x
    message(i)
  }
  
  # Subtract those ineligible days
  ineligible_days_malaria <-
    opd %>%
    filter(malaria) %>%
    group_by(date) %>% 
    tally
  ineligible_days_diarrhea <-
    opd %>%
    filter(diarrhea) %>%
    group_by(date) %>% 
    tally
  
  for (i in 1:nrow(ineligible_days_malaria)){
    this_date <- ineligible_days_malaria$date[i]
    subtract_this <- ineligible_days_malaria$n[i]
    these_indices <- which(atar$date > this_date &
      atar$date <= (this_date + days_out_malaria))
    atar$malaria[these_indices] <- atar$malaria[these_indices] - 
      subtract_this
  }
  for (i in 1:nrow(ineligible_days_diarrhea)){
    this_date <- ineligible_days_diarrhea$date[i]
    subtract_this <- ineligible_days_diarrhea$n[i]
    these_indices <- which(atar$date > this_date &
                             atar$date <= (this_date + days_out_diarrhea))
    atar$diarrhea[these_indices] <- atar$diarrhea[these_indices] - subtract_this
  }
  
  ggplot(data = atar %>%
           gather(key, value, malaria:diarrhea),
         aes(x = date,
             y = value,
             group = key,
             color = key)) +
    geom_line() +
    theme_bw() +
    ggtitle('Days at risk')
  
  numerator <-
    opd %>%
    group_by(date) %>%
    summarise(malaria = sum(length(which(malaria))),
              diarrhea = sum(length(which(diarrhea)))) %>%
    gather(key, value, malaria:diarrhea) %>%
    rename(numerator = value)
  denominator <-
    atar %>%
    gather(key, value, malaria:diarrhea) %>%
    rename(denominator = value)
  
  x <- full_join(numerator, denominator)
  x$p <- x$numerator / x$denominator * 100
    
  ggplot(data = x %>%
           filter(key == 'malaria'),
         aes(x = date,
             y = p)) +
    # geom_point(alpha = 0.7,
               # size = 1,
               # alpha = 0.6) +
    geom_line(alpha = 0.8, size = 0.2) +
    theme_fivethirtyeight() +
    ggtitle('Daily incidence',
            'Malaria') +
    ylab('Percentage') #+
    geom_smooth()
    
  
  # Denominator only
  ggplot(data = x,
         aes(x = date,
             y = denominator,
             group = key,
             color = key)) +
    geom_line()    
  
  # Numerator only
  ggplot(data = x,
         aes(x = date,
             y = numerator,
             group = key,
             color = key)) +
    geom_line()   
  
  # Define a function which generates all the days necessary for a given in and out
  generate_days <- function(row_number = 1){
    
    # Subset to just the row in question
    sub_data <- tmr[row_number,]
    
    # Get the permid of this individual
    this_perm_id <- sub_data$perm_id
    
    # Get the birth day
    dob <- sub_data$date_birth
    
    # Get the ineligible time for the person in question 
    # (ie, time immediately following infection)
    ineligible_start_malaria <-
      opd %>%
      filter(perm_id == this_perm_id,
             malaria) %>%
      .$date
    ineligible_start_diarrhea <-
      opd %>%
      filter(perm_id == this_perm_id,
             diarrhea) %>%
      .$date
    
   
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
    
    # Create columns for diseases
    the_rows$malaria <- the_rows$diarrhea <- TRUE
    
    # Remove ineligible dates
    # Create ineligible data frames
    if(length(ineligible_start_malaria) > 0){
      ineligible_dates_malaria <-
        sort(unique(do.call('c', 
                            lapply(ineligible_start_malaria, 
                                   function(x){x+1:28}))))
      the_rows <- 
        the_rows %>%
        mutate(malaria = ifelse(date %in% ineligible_dates_malaria,
                                FALSE,
                                malaria))
    }
    if(length(ineligible_start_diarrhea) > 0){
      ineligible_dates_diarrhea <-
        sort(unique(do.call('c', 
                            lapply(ineligible_start_diarrhea, 
                                   function(x){x+1:28}))))
      the_rows <- 
        the_rows %>%
        mutate(diarrhea = ifelse(date %in% ineligible_dates_diarrhea,
                                FALSE,
                                diarrhea))
    }
    
    # Spit back the rows
    return(the_rows)
  }
  
  
  # Go through each row of tmr, and generate the
  # necessary rows for a final dataset
  n_row_tmr <- nrow(tmr)
  # pb <- txtProgressBar(min = 0, max = n_row_tmr)
  results_list <- list()
  for (i in 1:n_row_tmr){
    # setTxtProgressBar(pb, i)
    message(i)
    try({
      # Get the person-specific time at risk
      these_rows <- generate_days(i)
      # results_list[[i]] <- these_rows
      
      # Get aggregated time at risk
      atar$diarrhea[atar$date %in% these_rows$date[these_rows$diarrhea]] <- 
        atar$diarrhea[atar$date %in% these_rows$date[these_rows$diarrhea]] + 1
      
      atar$malaria[atar$date %in% these_rows$date[these_rows$malaria]] <- 
        atar$malaria[atar$date %in% these_rows$date[these_rows$malaria]] + 1
    })  
    
  }
  
  # Combine results
  time_at_risk <- do.call('rbind', results_list)

  # Save  
  save(time_at_risk,
       file = 'data/time_at_risk.RData')
}
