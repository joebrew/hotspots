# Libraries
library(ggplot2)
library(RColorBrewer)
library(lubridate)

# Get data
source('prepare_data.R')

# Get malaria incidence for those younger than 15 in the
# area of interest
malaria_incidence <- 
  opd %>%
  filter(malaria,
         ageyr < 15) %>%
  group_by(date) %>%
  summarise(malaria_cases = n()) %>%
  left_join(
    etar %>%
      group_by(date) %>%
      summarise(population_at_risk = n()),
    by = 'date'
  ) %>%
  mutate(p = malaria_cases / population_at_risk) %>%
  mutate(pk = p * 1000) %>% 
  mutate(day_number = format(date, '%j'),
         year = format(date, '%Y'),
         week = week(date)) %>%
  filter(year >= 2004)

# Plot by week
x = malaria_incidence %>% group_by(week) %>% summarise(x = mean(pk, na.rm = TRUE)) %>% arrange(desc(x))
x <- x %>% arrange(week)
barplot(x$x, names.arg = x$week, xlab = 'Week', ylab = 'Average childhood incidence', main = 'Average daily incidence aggregated at weekly level')


# Plot over time
ggplot(data = malaria_incidence,
       aes(x = date,
           y = pk)) +
  geom_area(alpha = 0.6,
            fill = 'darkgreen') +
  labs(x = 'Date',
       y = 'Cases per 1,000',
       title = 'Malaria incidence in Manhiça',
       subtitle = '< 15 years of old, OPD data') +
  theme_cism()

# Plot on 1 year timeline
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(malaria_incidence$year)))
ggplot(data = malaria_incidence,
       aes(x = day_number,
           y = pk,
           group = factor(year),
           color = factor(year))) +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.2) +
  labs(x = 'Day',
       y = 'Cases per 1,000',
       title = 'Malaria incidence in Manhiça',
       subtitle = '< 15 years of old, OPD data') +
  theme_cism() +
  scale_color_manual(name = 'Year',
                     values = cols)

# The above has too much day specific noise. Get aggregated at weekly level
malaria_incidence_weekly <- 
  malaria_incidence %>%
  group_by(year, week) %>%
  summarise(malaria_cases = sum(malaria_cases),
            population_at_risk = mean(population_at_risk)) %>%
  mutate(p = malaria_cases / population_at_risk) %>%
  mutate(pk = p * 1000)

# Replot, at weekly level
# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(malaria_incidence_weekly$year)))
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(4)
ggplot(data = malaria_incidence_weekly %>%
         filter(year >= 2013),
       aes(x = week,
           y = pk,
           group = factor(year),
           color = factor(year))) +
  geom_line() +
  labs(x = 'Week',
       y = 'Cases per 1,000',
       title = 'Weekly malaria incidence in Manhiça',
       subtitle = '< 15 years of old, OPD data') +
  # facet_wrap()
  theme_cism() +
  scale_color_manual(name = 'Year',
                     values = cols)

# Let's examine by age
malaria_incidence_by_age <-
  opd %>%
  filter(malaria,
         ageyr < 15) %>%
  mutate(age_group = ifelse(ageyr <1, '0',
                            ifelse(ageyr < 5, '1-4',
                                   '5-14'))) %>%
  group_by(date, 
           age_group) %>%
  summarise(malaria_cases = n()) %>%
  left_join(
    etar %>%
      mutate(ageyr = round(yo)) %>%
      filter(ageyr < 15) %>%
      mutate(age_group = ifelse(ageyr <1, '0',
                                ifelse(ageyr < 5, '1-4',
                                       '5-14'))) %>%
      group_by(date, age_group) %>%
      summarise(population_at_risk = n()),
    by = c('date', 'age_group')
  ) %>%
  mutate(p = malaria_cases / population_at_risk) %>%
  mutate(pk = p * 1000) %>% 
  mutate(day_number = format(date, '%j'),
         year = format(date, '%Y'),
         week = week(date)) %>%
  filter(year >= 2004)


cols <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(malaria_incidence_by_age$age_group)))
ggplot(data = malaria_incidence_by_age,
       aes(x = date,
           y = pk,
           group = factor(age_group),
           color = factor(age_group))) +
  geom_line(alpha = 0.8) +
  geom_smooth() +
  labs(x = 'Week',
       y = 'Cases per 1,000',
       title = 'Weekly malaria incidence in Manhiça by age',
       subtitle = 'OPD data') +
  theme_cism() +
  scale_color_manual(name = 'Age',
                     values = cols)

# Explore inpd data

## Fever in inpd over time
fever_over_time <-
  inpd %>%
  group_by(date) %>% 
  summarise(fever = length(which(fever)),
            n = n()) %>%
  mutate(p = fever / n * 100)

ggplot(data = fever_over_time,
       aes(x = date,
           y = p)) +
  geom_area(alpha = 0.6) +
  xlab('Date') +
  ylab('Percentage') +
  ggtitle('Fever among inpatient admissions',
          'Percentage of all admissions with fever') +
  theme_cism()

# Compare inpd and opd
x <- opd %>%
  group_by(date) %>%
  summarise(fever = length(which(fever)),
            n_opd = n()) %>%
  mutate(source = 'opd') %>%
  mutate(p = fever / n_opd * 100) %>%
  mutate(pp = p / mean(p, na.rm = TRUE) * 100) %>%
  ungroup %>%
  bind_rows(inpd %>%
              group_by(date) %>%
              summarise(fever = length(which(fever)),
                        n_inpd = n()) %>%
              mutate(source = 'inpd') %>%
              mutate(p = fever / n_inpd * 100) %>%
              mutate(pp = p / mean(p, na.rm = TRUE) * 100) %>%
              ungroup) %>%
  mutate(year = as.numeric(format(date, '%Y')))

# for (the_year in sort(unique(x$year))){
#   y <- x %>% filter(year == the_year)
#   g <- ggplot(data = y,
#          aes(x = date,
#              y = p,
#              group = source,
#              color = source)) +
#     geom_point() +
#     geom_smooth() +
#     # facet_wrap(~year, scales = 'free_x') +
#     theme(axis.text.x = element_text(angle = 90)) +
#     ggtitle(the_year)
#   print(g)
#   Sys.sleep(1)
# }

dates <- sort(unique(x$date))
dates <- dates[dates >= '2002-01-01']
x <- x %>% filter(date >= '2002-01-01')
# dates <- dates[1000:length(dates)]
dates <- dates[format(dates, '%d') == '01']
backers <- round(seq(365, 2000, length = length(dates)))

for(i in 1:length(dates)){
  go_back <- backers[i]
  the_date <- dates[i]
  min_date <- as.Date(the_date - go_back)
  sub_data <- x %>%
    filter(date <= the_date,
           date >= min_date)
  smooth_data <- x %>%
    filter(date <= the_date,
           date >= (the_date - 365))
  if(nrow(sub_data) > 0){
    g <- ggplot() +
      geom_point(data = sub_data,
                 aes(x = date,
                     y = pp,
                     group = source,
                     color = source),
                 alpha = 0.3) +
      geom_line(data = sub_data,
                aes(x = date,
                    y = pp,
                    group = source,
                    color = source),
                alpha = 0.2) +
      # geom_smooth(alpha = 0.6) +
      stat_smooth(data = smooth_data,
                  aes(x = date,
                      y = pp,
                      group = source,
                      color = source),
                  geom = "smooth", position = "identity", 
                            method = "auto", formula = y ~ x, se = FALSE, n = 80,
                            fullrange = FALSE, 
                            level = 0.95, 
                            na.rm = FALSE,
                  alpha = 0.5) +
      # facet_wrap(~year, scales = 'free_x') +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = 'Date',
           y = 'Percentage',
           title = 'OPD vs. INPD: Fever as % of "normal"',
           subtitle = paste(min_date, 'through ', the_date)) +
      theme_bw() +
      scale_color_manual(name = '',
                         values = c('darkorange', 'darkgreen')) +
      ylim(0, 200) +
      geom_hline(yintercept = 100, lty = 2)
    ggsave(filename = paste0('out/the_date',
                             the_date,
                             '.JPG'),
           plot = g,
           width = 100,
           height = 50,
           units = 'mm',
           dpi = 50)
  }
}

# Absolute number of fevers
ggplot(data = x,
       aes(x = date,
           y = fever)) +
  geom_line(aes(color = source),
            alpha = 0.6) +
  theme_bw()

# Malaria over time
malaria_over_time <-
  bind_rows(
    opd %>%
      filter(place == 'Manhiça') %>%
      group_by(date) %>%
      summarise(cases = length(which(malaria))) %>%
      mutate(source = 'opd'),
    inpd %>%
      group_by(date) %>%
      summarise(cases = length(which(malaria))) %>%
      mutate(source = 'inpd')
  )

ggplot(data = malaria_over_time,
       aes(x = date,
           y = cases,
           color = source,
           group = source)) +
  geom_line(alpha = 0.7,
            lwd = 0.2)

malaria_by_year <-
  malaria_over_time %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year, source) %>%
  summarise(cases = sum(cases))

ggplot(data = malaria_by_year,
       aes(x = year, y = cases,
           group = source,
           color = source)) +
  geom_line()

# Action:
# Joe : read in and clean up npd data (on usb) (done)
# Joe: Figure out servolab trail (to get parasite density) (wrote to bea)
# Joe: explore symptomology more
# Joe: matching opd/npd (need to get creative in some cases)
# Next steps (Katarina):
# Let's prepare a document for her
# Get NPD data
# Evaluate association between non-complicated and complicated cases
# Classify severe/non-severe
# She'll calculate case-fatality rates

# More OPD exploration
# - Parasite density
# - General trends and metrics

# Spatial stuff:
  # see if demographers are done with gps coordiantes for everyone