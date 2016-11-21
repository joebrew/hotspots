# This File Imports and Manages the OPD1+2+3+4+5 data extracted from CISM's Server on 
# the 26th of May of 2016 using the R code provided by Agnaldo Samuel. 
# 
# It was written by Joe Brew, but is based on "misc/opd_cleaning_joe.do"
# by Bea Galatas
# Still in the making: 
# - Physical Examincation 
# - Diagnosis from OPD and ICD 
# - Get Densities from Servolab 
# - Get Hematocrit Info from Servolab 

# Attach packages
library(dplyr)
library(EpiWeek)

# Read in data
opd <- read.csv('data/opd_2016-05-26.csv')

# Clean up health Facility ###############
opd <-
  opd %>%
  # stata: replace place="" if place=="NA"
  # R: do nothing, leave as NA
  # stata: destring place, replace 
  mutate(place = as.numeric(as.character(place))) %>%
  # replace place=. if place==0
  mutate(place = ifelse(place == 0, NA, place)) %>%
  # replace place=1 if serialno<200000
  mutate(place = ifelse(serialno < 200000, 1, place)) %>%
  # label def hflab 1 "Manhiça" 2 "Maragra" 3 "Malavele" 4 "Palmeira" 5 "Ilha Josina" 6 "Taninga" 
  # label values place hflab
  mutate(place = ifelse(place == 1, 
                        'Manhiça',
                        ifelse(place == 2,
                               'Maragra', 
                               ifelse(place == 3,
                                      'Malavele', 
                                      ifelse(place == 4,
                                             'Palmeira',
                                             ifelse(place == 5,
                                                    'Ilha Josinha',
                                                    ifelse(place == 6,
                                                          'Taninga',
                                                          place)))))))

# Clean up Date #############
opd <- 
  opd %>%
  # rename date datest 
  rename(datest = date) %>%
  mutate(datest = as.Date(datest)) %>%
  # gen yr=substr(datest, 1, 4)
  mutate(yr = format(datest, '%Y')) %>%
  # destring yr, replace 
  mutate(yr = as.numeric(yr)) %>%
  # replace yr=. if yr==0
  # replace yr=. if yr==2999
  mutate(yr = ifelse(yr %in% c(0, 2999), NA, yr)) %>%
  # gen mon=substr(datest, 6, 2) 
  mutate(mon = format(datest, '%m')) %>%
  # destring mon, replace 
  mutate(mon  = as.numeric(mon)) %>%
  # stata: replace mon=. if mon==0
  # R: do nothing, impossible to get a 0 month date
  # gen day=substr(datest, 9, 2)
  mutate(day = format(datest, '%d')) %>%
  # destring day, replace 
  mutate(day = as.numeric(day)) %>%
  # stata: replace day=. if day==0
  # R: do nothing, impossible to get a 0 day date
  # gen date=mdy(mon, day, yr)
  # format date %td
  mutate(date = as.Date(paste0(yr, '-', mon, '-', day))) %>%
  # drop if yr<2000
  filter(yr >= 2000)

# Correct Date errors according to the launch of each HF #########
  # replace date=. if (year(date)<2002 & place==2) | 
  # (year(date)<2002 & place==3) | (year(date)<2005 & place==4)
opd$date[(opd$place == 'Maragra' &
                 opd$yr < 2002) |
           (opd$place == 'Malavele' &
                   opd$yr < 2002) |
           (opd$place == 'Palmeira' &
                   opd$yr < 2005)] <- NA

# *Create Epidemiological Weeks for OPD #############
# epiweek date, epiw(epi_week) epiy(epi_year)
epi_dates <- EpiWeek::dateToEpiweek(opd$date)
opd$epi_week <- epi_dates$weekno
opd$epi_year <- epi_dates$year

# Child referred from another HF ############
opd <- 
  opd %>%
  # stata: replace referred="" if referred=="NA"
  # R: do nothing, keep as NA
  # destring referred, replace 
  mutate(referred = as.numeric(as.character(referred))) %>%
  # replace referred=. if (referred<1 | referred>2)
  mutate(ifelse(referred < 1 | referred > 2, NA, referred))
  
# OUTPATIENT INFORMATION #############################

# PermID ############


  
  *PermID 
gen noperm=substr(perm_id, 1, 1)
replace noperm="" if noperm=="L"
destring noperm, replace 
replace perm_id="" if noperm>3 | noperm==.
replace perm_id="" if perm_id=="288"
replace perm_id="3830" if perm_id=="38308"
replace perm_id="3404-201" if perm_id=="340420126"

*Format PermID
gen permlen=length(perm_id)
gen end=substr(perm_id, -1, .)
replace perm_id=substr(perm_id, 1, 8) if end=="-" & permlen==9
replace perm_id=substr(perm_id, 1, 8) if permlen==10
replace perm_id="3207" if end=="-" & permlen==11
drop permlen end 

*Household
gen permlen=length(perm_id)
gen agregado=substr(perm_id, 1, 8) if permlen>=8
gen end=substr(agregado, -1, .)
replace agregado="" if end=="-"
replace perm_id=substr(perm_id, 1, 4) if end=="-"
replace perm_id=substr(perm_id, 1, 4) if permlen==7
drop permlen end 

*Neighbourhood 
gen bairro=substr(perm_id, 1, 4) 
gen end=substr(bairro, -1, .)
replace bairro="" if end=="-"
replace perm_id="" if end=="-"
drop end 


*Date of birth 
replace day_birth=. if day_birth<1 | day_birth>31
replace mon_birth=. if mon_birth<1 | mon_birth>12
replace yea_birth=. if yea_birth>yr	| yea_birth<1982
replace yea_birth=. if yea_birth==6720
gen dob=mdy(mon_birth, day_birth, yea_birth)
format dob %td
replace dob=. if dob>date

*Age 

*In Months 
gen age= (date- dob)/30.4375 if dob!=. & date!=.
replace age=. if age<0

*In Years 
gen ageyr= (date-dob)/365.25 if (dob!=. & date!=.)
  replace ageyr=round(ageyr, 0.1)
replace ageyr=. if ageyr<0
drop if ageyr>=15 & age!=.

*Create Age Groups
*Based on Months 
gen agegrp=age if age!=.
recode agegrp 0/1=0 1/6=1 6/12=2 12/24=3 24/36=4 36/48=5 48/60=6  60/120=7 120/max=8 
label define agegr 0 "<1m" 1 "1-<6m" 2 "6-<12m" 3 "1-<2y" 4 "2-<3y" 5 "3-<4y" 6 "4-<5y" 7 "5-<10y" 8 "10-<15y"
label values agegrp agegr

*Based on Years - Option 1
gen agegrpyr=ageyr if age!=.
recode agegrpyr 0/1=0 1/2=1 2/3=2 3/4=3 4/5=4  5/max=5 
label define edatanys 0 "<1y" 1 "1-<2y" 2 "2-<3y" 3 "3-<4y" 4 "4-<5y" 5 ">=5y" 
label values agegrpyr edatanys

*Based on Years - Option 2 
gen agegrpyr2=ageyr if age!=.
recode agegrpyr2 0/1=0 1/2=1 2/3=2 3/4=3 4/5=4  5/6=5 6/7=6 7/8=7 8/9=8 9/10=9 10/11=10 11/12=11 12/13=12 13/14=13 14/15=14
label define edatanys2 0 "<1y" 1 "1- y" 2 "2- y" 3 "3- y" 4 "4- y" 5 "5- y" 6 "6- y" 7 "7- y" 8 "8- y" 9 "9- y" 10 "10- y" 11 "11- y" 12 "12- y" 13 "13- y" 14 "14- y"
label values agegrpyr2 edatanys2

*Sex
replace sex=. if (sex<1 | sex>2)
  replace sex=sex-1
lab define sexlab 0 "Male" 1 "Female" 
lab values sex sexlab 

*<<<< BODY MEASURES <<<<<<<
  
  *Respiratory Frequency 
replace resprate=. if (resprate<20 | resprate>110)
  
  *Body Weight 
replace weigth=. if weigth<1 | weigth>80

*Temperature 
replace temp=. if (temp<32 | temp>42)
  
  *Fever based on Body Temperature
gen fever=temp>=37.5 if temp!=.
label define feverlab 0 "no fever" 1 ">=37.5∫C" 
label values fever feverlab
label variable fever "Fever from Axillary Temperature"

*Fever in ast 24 hours 
replace feveryno=. if (feveryno<1 | feveryno>2)
  recode feveryno 2=0
lab define yno 0 "No" 1 "Yes" 
lab values feveryno yno 

*<<<< SAMPLES TAKEN <<<<<<<<
  
  *Slide taken 
replace slidesyno=. if (slidesyno<1 | slidesyno>2)
  recode slidesyno 2=0 
lab values slidesyno yno 

*Number of Sample Identification (NIDA) 
gen nidalen=length(brady)
replace brady="" if nidalen<6

*Create a new NIDA variable Supposing length 6 and 7 are missing the ".0"
gen nida=brady 
replace nida=brady+".0" if nidalen==6 | nidalen==7
drop nidalen

*Reason for slide not taken 
replace slideswhy=. if (slideswhy<1 | slideswhy>3)
  replace slideswhy=slideswhy-1 
lab define slideswhylab 0 "No criteria" 1 "Previous results in last 24h" 2 "Not authorized"
lab values slideswhy slideswhylab 

*Number of Sample Identification (NIDA) if a previous sample had been collected 
gen nidalen=length(brady2)
replace brady2="" if nidalen<6 

*Create a new NIDA variable Supposing length 6 and 7 are missing the ".0"
gen nida2=brady2 
replace nida2=brady2+".0" if nidalen==6 | nidalen==7
drop nidalen

*<<<< INFORMATION ABOUT CURRENT ILLNESS <<<<<<<
  
  *Has fever
replace fever2yno=. if fever2yno<1 | fever2yno>2
recode fever2yno 2=0
lab values fever2yno yno

*Days of fever reported 
replace feverdays=. if feverdays<0

*Has cough 
replace coughyno=. if coughyno<1 | coughyno>2
recode coughyno 2=0
lab values coughyno yno 

*Days of cough reported 
replace coughdays="" if coughdays=="NA"
destring coughdays, replace 
replace coughdays=. if coughdays<0

*Difficulties breathing 
replace breathyno=. if breathyno<1 | breathyno>2
recode breathyno 2=0 
lab values breathyno yno

*Days of difficulties in breathing reported
replace breathdays="" if breathdays=="NA"
destring breathdays, replace 
replace breathdays=. if breathdays<0

*Has diarrhea 
replace diarryno=. if diarryno<1 | diarryno>2
recode diarryno 2=0 
lab values diarryno yno 

*Days of Diarrhea 
replace diarrdays=. if diarrdays<0

*If diarrhea, number of Diarrheas reported 
replace diarrnum=. if diarrnum<0

*If diarrhea, characteristics of feces 
replace diarchar=. if (diarchar<1 | diarchar>2)
  replace diarchar=diarchar-1
lab define diarcharlab 0 "Wattery" 1 "Bloody"
lab values diarchar diarcharlab

*Vomits 
replace vomityno=. if vomityno<1 | vomityno>2
recode vomityno 2=0
lab values vomityno yno 

*Days of Vomit reported 
replace vomitdays="" if vomitdays=="NA"
destring vomitdays, replace 
replace vomitdays=. if vomitdays<0

*Has seizures
replace fittedyno=. if fittedyno<1 | fittedyno>2
recode fittedyno 2=0 
lab values fittedyno yno 

*NUmber of seizures 
replace fittednum="" if fittednum=="NA"
destring fittednum, replace 
replace fittednum=. if fittednum<0

*Burns
replace burnyno=. if burnyno<1 | burnyno>2
recode burnyno 2=0
lab values burnyno yno 

*Accidents	
replace accidenyno=. if accidenyno<1 | accidenyno>2
recode accidenyno 2=0
lab values accidenyno yno 

*Other Symptoms 


*<<<< PHYSICAL EXAMINATION <<<<<<<
  
  
  replace fontanelle=. if (fontanelle<1 | fontanelle>4)
    replace fontanelle=4 if age>18
replace deshidrat=. if (deshidrat<1 | deshidrat>4)
  
  
  
  *<<<<< OUTPATIENT DIAGNOSIS AND TREATMENT <<<<<<<<
  
  *Thicksmear results (Number of Cross based on Microscopy Reading)
destring thicksmear, force replace 
replace thicksmear=. if thicksmear>6


*Identify Positive Slides 
gen slidepos=thicksmear
recode slidepos 1/5=1 6=.
label define posneg 1 "Pos" 2 "Neg"
label values slidepos posneg

*Clean RDT results 
replace rdt=. if (rdt==0 | rdt>=3) 
  label values rdt posneg

*Packed Cell Volume (PCV) --> IMPORT FROM SERVOLAB 
/*replace pcv=. if date<mdy(08,27,1998)
gen an33=(pcv<33) if pcv!=.
gen an25=(pcv<25) if pcv!=.
gen an15=(pcv<15) if pcv!=.
label define anemia33 0 "pcv>=33" 1 "pcv<33" 
label values an33 anemia33
label define anemia25 0 "pcv>=25" 1 "pcv<25" 
label values an25 anemia25
label define anemia15 0 "pcv>=15" 1 "pcv<15" 
label values an15 anemia15

gen anemiagrp=pcv if pcv!=.
recode anemiagrp min/14=4 14/24=3 24/32=2 32/max=1
label define anemiagrp 4 "pcv<15" 3 "pcv 15-<25" 2 "pcv 25-<33" 1 "pcv=>33" 
label values anemiagrp anem*/
  
  *<<<<< INPATIENT DIAGNOSIS AND TREATMENT <<<<<<<<
  
  label define afterpcd 1 "casa" 2 "icd" 3 "transf" 4 "aband"
label values afterpcd afterpcd
label define aftericd 1 "casa" 2 "internam" 3 "transf" 4 "obito" 5 "aband"
label values aftericd aftericd



*END OF OUTPATIENT QUESTIONNAIRE 


*<<<<<<< ADDITIONAL DATA MANAGEMENT <<<<<<	
  
  *MALARIA	
*Create a malaria diagnosis variable 
gen malpos=slidepos
replace malpos=rdtpos if malpos==.

*Seasons
keep if date >= d(01jul1997) & date <= d(30jun2014)
gen season=.
replace season=1 if date>=d(01jul1997) & date <= d(30jun1998)
replace season=2 if date>=d(01jul1998) & date <= d(30jun1999)
replace season=3 if date>=d(01jul1999) & date <= d(30jun2000)
replace season=4 if date>=d(01jul2000) & date <= d(30jun2001)
replace season=5 if date>=d(01jul2001) & date <= d(30jun2002)
replace season=6 if date>=d(01jul2002) & date <= d(30jun2003)
replace season=7 if date>=d(01jul2003) & date <= d(30jun2004)
replace season=8 if date>=d(01jul2004) & date <= d(30jun2005)
replace season=9 if date>=d(01jul2005) & date <= d(30jun2006)
replace season=10 if date>=d(01jul2006) & date <= d(30jun2007)
replace season=11 if date>=d(01jul2007) & date <= d(30jun2008)
replace season=12 if date>=d(01jul2008) & date <= d(30jun2009)
replace season=13 if date>=d(01jul2009) & date <= d(30jun2010)
replace season=14 if date>=d(01jul2010) & date <= d(30jun2011)
replace season=15 if date>=d(01jul2011) & date <= d(30jun2012)
replace season=16 if date>=d(01jul2012) & date <= d(30jun2013)
replace season=17 if date>=d(01jul2013) & date <= d(30jun2014)
replace season=18 if date>=d(01jul2014) & date <= d(30jun2015)
replace season=19 if date>=d(01jul2015) & date <= d(30jun2016)

*Wet and Rainy Season 
gen seastrad=mon
recode seastrad 1/4=1 5/10=2 11/12=1
label define season 1 "rainy" 2 "dry"
label values seastrad season		


save $dta/pri/opd_tot, replace 
