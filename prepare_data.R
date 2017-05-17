# This File Imports and Manages the OPD1+2+3+4+5 data extracted from CISM's Server on 
# the 26th of May of 2016 using the R code provided by Agnaldo Samuel. 
# 
# It was written by Joe Brew, but is based largely
# on "misc/opd_cleaning_joe.do"
# by Bea Galatas
# Still in the making: 
# - Physical Examincation 
# - Diagnosis from OPD and ICD 
# - Get Densities from Servolab 
# - Get Hematocrit Info from Servolab 

# Attach packages
library(dplyr)
library(EpiWeek)
library(icd)
library(cism)
library(data.table)
library(DBI)
library(readxl)


if('all_data.RData' %in% dir('data')){
  load('data/all_data.RData')
} else {
  
  if('opd_cleaned.RData' %in% dir('data')){
    load('data/opd_cleaned.RData')
  } else {
    
    # Read in raw opd data
    if('data/opd_dirty.RData' %in% dir('data')){
      load('data/opd_dirty.RData')
    } else {
      # opd <- read.csv('data/opd_2016-05-26.csv')
      con <- credentials_connect(credentials_extract(credentials_file = 'credentials/bea_credentials.yaml', all_in_file = TRUE))
      
      dbf_opd3 <- 
        get_data(query = paste0(
          "SELECT a.serialno, a.date,a.perm_id, b.name,a.day_birth, a.mon_birth, a.yea_birth, a.sex,b.mother_nam,
          b.father_nam, b.head_nam, a.region_nam,a.resprate,a.weigth, a.temp, a.fever2yno, a.slidesyno,
          a.slideswhy, a.stoolsyno, a.stoolswhy,a.feveryno, a.feverdays, a.coughyno,  a.coughdays,
          a.breathyno, a.diarryno, a.diarrdays, a.diarrnum, a.diarchar, a.vomityno,  a.vomitdays,
          a.othersymp, a.fontanelle, a.deshidrat, a.pallor, a.jaundice, a.oedema, a.eardischar,
          a.indrawin,  a.nasa_flar, a.cra_cre_bt, a.wheeze_ron, a.hepatomega, a.splenomega,  a.neck_stiff,
          a.diag1, a.diag2, a.diag3, a.afterpcd, a.treatment, a.fiel_wrkr, a.afterinp, a.dataclerk1,  a.dataclerk2,
          a.data_time1, a.data_time2, a.data_clerk, a.data_statu,  a.data_date,  a.accidenyno,  a.brady, a.burnyno,
          a.fittednum,a.fittedyno, a.foldskin,  a.lethargic, a.ssblo, a.ssdia, a.ssran, a.under5,  a._id,a._convertstamp
          FROM cism.dbf_opd3 a inner join phi.dbf_opd3 b on a.serialno =b.serialno limit 1000000"),
          connection_object = con)
      
      dbf_opd4 <- get_data(query = paste0(
        "SELECT a.serialno,a.date, a.perm_id,b.name,a.day_birth,a.mon_birth, a.yea_birth, a.sex,b.mother_nam,b.father_nam,
        b.head_nam,a.region_nam, a.resprate,a.weigth, a.temp,a.fever2yno,a.slidesyno,    a.slideswhy,a.feveryno,
        a.feverdays, a.coughyno,a.coughdays, a.breathyno,a.diarryno,a.diarrdays,a.diarrnum,a.foldskin,a.diarchar,
        a.vomityno,a.vomitdays, a.othersymp, a.fontanelle,a.deshidrat, a.pallor,a.jaundice,a.oedema, a.eardischar,
        a.indrawin,a.nasa_flar,a.cra_cre_bt, a.wheeze_ron,a.hepatomega,a.splenomega,a.neck_stiff,a.lethargic,
        a.diag1,a.diag2, a.diag3,a.afterpcd,a.brady,a.fittedyno,a.fittednum, a.burnyno,a.accidenyno,a.place,
        a.referred,a.ref_serial,a.studyno,a.thicksmear, a.ttopcd1,a.ttopcd2,a.ttopcd3,a.ttopcd4,a.fieldwrk1,
        a.icd_diag1,a.icd_diag2,a.icd_diag3,a.aftericd,a.ttoicd1,a.ttoicd2, a.ttoicd3, a.ttoicd4, a.filedwrk2,
        a.fieldwrk3, a.dataclerk1,a.dataclerk2,a.data_time1,a.data_time2,a.data_clerk,a.data_statu,a.data_date,
        a.hto, a.breathdays, a.brady2, a._id,a._convertstamp
        FROM cism.dbf_opd4 a inner join phi.dbf_opd4 b on a.serialno =b.serialno limit 10000000"),
        connection_object = con)
      
      
      dbf_opd5 <- get_data(query = paste0(
        "SELECT a.serialno,a.date,a.perm_id,b.name,a.day_birth,a.mon_birth,a.yea_birth,a.sex,b.mother_nam,b.father_nam,
        b.head_nam,a.region_nam,a.resprate,a.weigth,a.temp,a.fever2yno,a.slidesyno,a.slideswhy,a.feveryno,a.feverdays,a.coughyno,
        a.coughdays,a.breathyno,a.diarryno,a.diarrdays,a.diarrnum,a.foldskin,a.diarchar,a.vomityno,a.vomitdays,a.othersymp,
        a.fontanelle,a.deshidrat,a.pallor,a.jaundice,a.oedema,a.eardischar,a.indrawin,a.nasa_flar,a.cra_cre_bt,a.wheeze_ron,
        a.hepatomega,a.splenomega,a.neck_stiff,a.lethargic,a.diag1,a.diag2,a.diag3,a.afterpcd,a.brady,a.fittedyno,a.fittednum,
        a.burnyno,a.accidenyno,a.place,a.referred,a.ref_serial,a.studyno,a.thicksmear,a.ttopcd1,a.ttopcd2,a.ttopcd3,a.ttopcd4,
        a.fieldwrk1,a.icd_diag1,a.icd_diag2,a.icd_diag3,a.aftericd,a.ttoicd1,a.ttoicd2,a.ttoicd3,a.diag4,a.filedwrk2,a.fieldwrk3,
        a.dataclerk1,a.dataclerk2,a.data_time1,a.data_time2,a.data_clerk,a.data_statu,a.data_date,a.hto,a.breathdays,a.brady2,
        a.qesnum,a.ttoicd4,a.bradytime,a.rdt,a._id,a._convertstamp
        FROM cism.dbf_opd5 a inner join phi.dbf_opd5 b on a.serialno=b.serialno limit 100000000"),
        connection_object = con)
      
      
      oc_opd_2014 <- get_data(query = paste0(
        "SELECT a.serialno,a.date,a.perm_id,b.name,a.day_birth,a.mon_birth,a.yea_birth,a.sex,b.mother_nam,b.father_nam,b.head_nam,
        a.region_nam,a.resprate,a.weigth,a.temp,a.fever2yno,a.slidesyno,a.slideswhy,a.feveryno,a.feverdays,a.coughyno,a.coughdays,
        a.breathyno,a.diarryno,a.diarrdays,a.diarrnum,a.foldskin,a.diarchar,a.vomityno,a.vomitdays,a.othersymp,a.fontanelle,
        a.deshidrat,a.pallor,a.jaundice,a.oedema,a.eardischar,a.indrawin,a.nasa_flar,a.cra_cre_bt,a.wheeze_ron,a.hepatomega,
        a.splenomega,a.neck_stiff,a.lethargic,a.diag1,a.diag2,a.diag3,a.afterpcd,a.brady,a.fittedyno,a.fittednum,a.burnyno,
        a.accidenyno,a.place,a.referred,a.ref_serial,a.studyno,a.thicksmear,a.ttopcd1,a.ttopcd2,a.ttopcd3,a.ttopcd4,
        a.fieldwrk1,a.icd_diag1,a.icd_diag2,a.icd_diag3,a.aftericd,a.ttoicd1,a.ttoicd2,a.ttoicd3,a.diag4,a.filedwrk2,
        a.fieldwrk3,a.dataclerk1,a.dataclerk2,a.data_time1,a.data_time2,a.data_clerk,a.data_statu,a.data_date,
        a.hto,a.breathdays,a.brady2,a.qesnum,a.ttoicd4,a.bradytime,a.rdt,a.crfstatus,a._id,a._convertstamp
        FROM cism.oc_opd5_2014 a inner join phi.oc_opd5_2014 b on a.serialno=b.serialno limit 1000000"),
        connection_object = con)
      
      oc_opd_2015 <- get_data(query = paste0(
        "SELECT a.serialno,a.date,a.perm_id,b.name,a.day_birth,a.mon_birth,a.yea_birth,a.sex,b.mother_nam,b.father_nam,b.head_nam,
        a.region_nam,a.resprate,a.weigth,a.temp,a.fever2yno,a.slidesyno,a.slideswhy,a.feveryno,a.feverdays,a.coughyno,a.coughdays,
        a.breathyno,a.diarryno,a.diarrdays,a.diarrnum,a.foldskin,a.diarchar,a.vomityno,a.vomitdays,a.othersymp,a.fontanelle,
        a.deshidrat,a.pallor,a.jaundice,a.oedema,a.eardischar,a.indrawin,a.nasa_flar,a.cra_cre_bt,a.wheeze_ron,a.hepatomega,
        a.splenomega,a.neck_stiff,a.lethargic,a.diag1,a.diag2,a.diag3,a.afterpcd,a.brady,a.fittedyno,a.fittednum,a.burnyno,
        a.accidenyno,a.place,a.referred,a.ref_serial,a.studyno,a.thicksmear,a.ttopcd1,a.ttopcd2,a.ttopcd3,a.ttopcd4,
        a.fieldwrk1,a.icd_diag1,a.icd_diag2,a.icd_diag3,a.aftericd,a.ttoicd1,a.ttoicd2,a.ttoicd3,a.diag4,a.filedwrk2,
        a.fieldwrk3,a.dataclerk1,a.dataclerk2,a.data_time1,a.data_time2,a.data_clerk,a.data_statu,a.data_date,
        a.hto,a.breathdays,a.brady2,a.qesnum,a.ttoicd4,a.bradytime,a.rdt,a.crfstatus,a._id,a._convertstamp
        FROM cism.oc_opd5_2015 a inner join phi.oc_opd5_2015 b on a.serialno=b.serialno limit 100000000"),
        connection_object = con)
      
      oc_opd_2016 <- get_data(query = paste0(
        "SELECT a.serialno,a.date,a.perm_id,b.name,a.day_birth,a.mon_birth,a.yea_birth,a.sex,b.mother_nam,b.father_nam,b.head_nam,
        a.region_nam,a.resprate,a.weigth,a.temp,a.fever2yno,a.slidesyno,a.slideswhy,a.feveryno,a.feverdays,a.coughyno,a.coughdays,
        a.breathyno,a.diarryno,a.diarrdays,a.diarrnum,a.foldskin,a.diarchar,a.vomityno,a.vomitdays,a.othersymp,a.fontanelle,
        a.deshidrat,a.pallor,a.jaundice,a.oedema,a.eardischar,a.indrawin,a.nasa_flar,a.cra_cre_bt,a.wheeze_ron,a.hepatomega,
        a.splenomega,a.neck_stiff,a.lethargic,a.diag1,a.diag2,a.diag3,a.afterpcd,a.brady,a.fittedyno,a.fittednum,a.burnyno,
        a.accidenyno,a.place,a.referred,a.ref_serial,a.studyno,a.thicksmear,a.ttopcd1,a.ttopcd2,a.ttopcd3,a.ttopcd4,
        a.fieldwrk1,a.icd_diag1,a.icd_diag2,a.icd_diag3,a.aftericd,a.ttoicd1,a.ttoicd2,a.ttoicd3,a.diag4,a.filedwrk2,
        a.fieldwrk3,a.dataclerk1,a.dataclerk2,a.data_time1,a.data_time2,a.data_clerk,a.data_statu,a.data_date,
        a.hto,a.breathdays,a.brady2,a.qesnum,a.ttoicd4,a.bradytime,a.rdt,a.crfstatus,a._id,a._convertstamp
        FROM cism.oc_opd5_2016 a inner join phi.oc_opd5_2016 b on a.serialno=b.serialno limit 10000000"),
        connection_object = con)
      
      ## Append the datasets 
      opd <- plyr::rbind.fill(dbf_opd3,dbf_opd4,dbf_opd5,oc_opd_2016,oc_opd_2015, oc_opd_2014)
      
      save(opd,
           file = 'data/opd_dirty.RData')
      
      ## rm partial data sets
      rm(dbf_opd3,dbf_opd4,dbf_opd5,oc_opd_2016,oc_opd_2015, oc_opd_2014)
    }
    
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
      mutate(referred = ifelse(referred < 1 | referred > 2, NA, referred))
    
    # OUTPATIENT INFORMATION #############################
    
    # PermID ############
    # gen noperm=substr(perm_id, 1, 1)
    opd$noperm <- substr(opd$perm_id, 1, 1)
    opd$noperm <- ifelse(opd$noperm == 'L', NA, opd$noperm)
    opd$noperm <- as.numeric(as.character(opd$noperm))
    # replace perm_id="" if noperm>3 | noperm==.
    # replace perm_id="" if perm_id=="288"
    opd$perm_id <- as.character(opd$perm_id)
    opd$perm_id <- 
      ifelse(is.na(opd$noperm) |
               opd$noperm > 3 |
               opd$perm_id == '288',
             NA,
             opd$perm_id)
    # replace perm_id="3830" if perm_id=="38308"
    opd$perm_id[opd$perm_id == '38308'] <- '3830'
    # replace perm_id="3404-201" if perm_id=="340420126"
    opd$perm_id[opd$perm_id == '340420126'] <- '3404-201'
    
    # *Format PermID #####
    # gen permlen=length(perm_id)
    opd$permlen <- nchar(opd$perm_id)
    # gen end=substr(perm_id, -1, .)
    opd$end <- substr(opd$perm_id, opd$permlen - 1, opd$permlen)
    # replace perm_id=substr(perm_id, 1, 8) if end=="-" & permlen==9
    # THIS DOES NOTHING
    opd$perm_id <- ifelse(opd$end == '-' &
                            opd$permlen,
                          substr(perm_id, 1, 9),
                          opd$perm_id)
    # replace perm_id=substr(perm_id, 1, 8) if permlen==10
    opd$perm_id <- ifelse(opd$permlen == 10,
                          substr(opd$perm_id, 1, 9),
                          opd$perm_id)
    # replace perm_id="3207" if end=="-" & permlen==11
    opd$perm_id <- ifelse(opd$end == '-' &
                            opd$permlen == 11,
                          '3207',
                          opd$perm_id)
    # drop permlen end 
    opd$permlen <- NULL
    
    # *Household ############
    # gen permlen=length(perm_id)
    opd$permlen <- nchar(opd$perm_id)
    # gen agregado=substr(perm_id, 1, 8) if permlen>=8
    opd$agregado <- ifelse(opd$permlen >= 8,
                           substr(opd$perm_id, 1, 9),
                           opd$perm_id)
    # gen end=substr(agregado, -1, .)
    opd$end <- substr(opd$agregado, nchar(opd$agregado) - 1, nchar(opd$agregado))
    # replace agregado="" if end=="-"
    opd$agregado[opd$end == '-'] <- NA
    # replace perm_id=substr(perm_id, 1, 4) if end=="-"S - seems wrong
    opd$perm_id <- ifelse(opd$end == '-',
                          substr(opd$perm_id, 1, 5),
                          opd$perm_id)
    # replace perm_id=substr(perm_id, 1, 4) if permlen==7
    opd$perm_id <- ifelse(opd$permlen == 7,
                          substr(opd$perm_id, 1, 5),
                          opd$perm_id)
    # drop permlen end 
    opd$permlen <- NULL
    
    # *Neighbourhood ##############################
    # gen bairro=substr(perm_id, 1, 4) 
    opd$bairro <- substr(opd$perm_id, 1, 5)
    # gen end=substr(bairro, -1, .)
    opd$end <- substr(opd$bairro, nchar(opd$bairro) - 1, nchar(opd$bairro))
    # replace bairro="" if end=="-"
    opd$bairro[opd$end == '-'] <- NA
    # replace perm_id="" if end=="-"
    opd$perm_id[opd$end == '-'] <- NA
    # drop end 
    opd$end <- NULL
    
    # *Date of birth #############################
    # replace day_birth=. if day_birth<1 | day_birth>31
    opd$day_birth <- ifelse(opd$day_birth < 1 | opd$day_birth > 31,
                            NA,
                            opd$day_birth)
    # replace mon_birth=. if mon_birth<1 | mon_birth>12
    opd$mon_birth <- ifelse(opd$mon_birth <1 | opd$mon_birth > 12,
                            NA,
                            opd$mon_birth)
    # replace yea_birth=. if yea_birth>yr	| yea_birth<1982
    opd$yea_birth <- ifelse(opd$yea_birth > opd$yr | opd$yea_birth < 1982,
                            NA,
                            opd$yea_birth)
    # replace yea_birth=. if yea_birth==6720
    opd$yea_birth <- ifelse(opd$yea_birth == 6720,
                            NA,
                            opd$yea_birth)
    # gen dob=mdy(mon_birth, day_birth, yea_birth)
    # format dob %td
    opd$dob <- as.Date(paste0(opd$yea_birth, '-',
                              opd$mon_birth, '-',
                              opd$day_birth))
    # replace dob=. if dob>date
    opd$dob <- if_else(opd$dob > opd$date,
                       NA,
                       opd$dob)
    # *Age ################## 
    # 
    # *In Months 
    # gen age= (date- dob)/30.4375 if dob!=. & date!=.
    opd$age <- as.numeric((opd$date - opd$dob)) / 30.4375
    # replace age=. if age<0
    opd$age <- ifelse(opd$age < 0, NA, opd$age)
    
    # *In Years 
    # gen ageyr= (date-dob)/365.25 if (dob!=. & date!=.)
    opd$ageyr <- as.numeric(opd$date - opd$dob) / 365.25
    #   replace ageyr=round(ageyr, 0.1) 
    opd$ageyr <- round(opd$ageyr, 0.1)
    # replace ageyr=. if ageyr<0
    opd$ageyr <- ifelse(opd$ageyr < 0, NA, opd$ageyr)
    # drop if ageyr>=15 & age!=. 
    opd <- opd %>% filter(ageyr < 15 & !is.na(age))
    
    # *Create Age Groups
    # SKIPPING FOR NOW, SEEMS UNECESSARY
    # *Based on Months 
    # gen agegrp=age if age!=.
    # recode agegrp 0/1=0 1/6=1 6/12=2 12/24=3 24/36=4 36/48=5 48/60=6  60/120=7 120/max=8 
    # label define agegr 0 "<1m" 1 "1-<6m" 2 "6-<12m" 3 "1-<2y" 4 "2-<3y" 5 "3-<4y" 6 "4-<5y" 7 "5-<10y" 8 "10-<15y"
    # label values agegrp agegr
    # 
    # *Based on Years - Option 1
    # gen agegrpyr=ageyr if age!=.
    # recode agegrpyr 0/1=0 1/2=1 2/3=2 3/4=3 4/5=4  5/max=5 
    # label define edatanys 0 "<1y" 1 "1-<2y" 2 "2-<3y" 3 "3-<4y" 4 "4-<5y" 5 ">=5y" 
    # label values agegrpyr edatanys
    # 
    # *Based on Years - Option 2 
    # gen agegrpyr2=ageyr if age!=.
    # recode agegrpyr2 0/1=0 1/2=1 2/3=2 3/4=3 4/5=4  5/6=5 6/7=6 7/8=7 8/9=8 9/10=9 10/11=10 11/12=11 12/13=12 13/14=13 14/15=14
    # label define edatanys2 0 "<1y" 1 "1- y" 2 "2- y" 3 "3- y" 4 "4- y" 5 "5- y" 6 "6- y" 7 "7- y" 8 "8- y" 9 "9- y" 10 "10- y" 11 "11- y" 12 "12- y" 13 "13- y" 14 "14- y"
    # label values agegrpyr2 edatanys2
    
    
    # *Sex
    # replace sex=. if (sex<1 | sex>2)
    opd$sex[opd$sex < 1 | opd$sex > 2] <- NA
    #   replace sex=sex-1
    opd$sex <- opd$sex - 1
    # lab define sexlab 0 "Male" 1 "Female" 
    opd$sex_lab <- ifelse(opd$sex == 0, 'Male', 
                          ifelse(opd$sex == 1, 'Female', 
                                 NA))
    # lab values sex sexlab 
    
    # *<<<< BODY MEASURES <<<<<<< ################
    #   
    #   *Respiratory Frequency 
    # replace resprate=. if (resprate<20 | resprate>110)
    opd$resprate <- ifelse(opd$resprate < 20 | 
                             opd$resprate > 110,
                           NA,
                           opd$resprate)
    #   *Body Weight 
    # replace weigth=. if weigth<1 | weigth>80
    opd$weigth <- ifelse(opd$weigth < 1 | opd$weigth > 80, 
                         NA,
                         opd$weigth)
    
    # *Temperature 
    # replace temp=. if (temp<32 | temp>42)
    opd$temp[opd$temp < 32 | opd$temp > 42] <- NA
    #   *Fever based on Body Temperature
    # gen fever=temp>=37.5 if temp!=.
    opd$fever <- opd$temp >= 37.5
    # label define feverlab 0 "no fever" 1 ">=37.5∫C" 
    opd$feverlab <- ifelse(opd$fever, '>=37.5C',
                           ifelse(!opd$fever, 'no fever',
                                  NA))
    # label values fever feverlab
    # label variable fever "Fever from Axillary Temperature"
    # 
    # *Fever in ast 24 hours 
    # replace feveryno=. if (feveryno<1 | feveryno>2)
    opd$feveryno[opd$feveryno < 1 | opd$feveryno > 2] <- NA
    #   recode feveryno 2=0
    opd$feveryno[opd$feveryno == 2] <- 0
    # lab define yno 0 "No" 1 "Yes" 
    opd$feverynolab <- ifelse(opd$feveryno == 0, 'No', 
                              ifelse(opd$feveryno, 'Yes', 
                                     NA))
    # lab values feveryno yno 
    # 
    # *<<<< SAMPLES TAKEN <<<<<<<< ####################
    
    #   *Slide taken 
    # replace slidesyno=. if (slidesyno<1 | slidesyno>2)
    opd$slidesyno[opd$slidesyno < 1 | opd$slidesyno >2] <- NA
    #   recode slidesyno 2=0 
    opd$slidesyno[opd$slidesyno == 2] <- 0
    # lab values slidesyno yno 
    # 
    # *Number of Sample Identification (NIDA) 
    # gen nidalen=length(brady)
    opd$nidalen <- nchar(opd$brady)
    # replace brady="" if nidalen<6
    opd$brady[opd$nidalen < 6] <- NA
    
    # *Create a new NIDA variable Supposing length 6 and 7 are missing the ".0"
    # gen nida=brady 
    opd$nida <- opd$brady
    # replace nida=brady+".0" if nidalen==6 | nidalen==7
    opd$nida <- ifelse(opd$brady %in% c(6, 7),
                       paste0(opd$brady, '.0'),
                       opd$nida)
    # drop nidalen
    opd$nidalen <- NULL
    
    save.image('~/Desktop/temp.RData')
    
    # *Reason for slide not taken 
    # replace slideswhy=. if (slideswhy<1 | slideswhy>3)
    opd$slideswhy <- ifelse(opd$slideswhy < 1 | opd$slideswhy > 3, NA,
                            opd$slideswhy)
    # replace slideswhy=slideswhy-1 
    opd$slideswhy <- opd$slideswhy - 1
    # lab define slideswhylab 0 "No criteria" 1 "Previous results in last 24h" 2 "Not authorized"
    # lab values slideswhy slideswhylab 
    opd$slidewhylab <- ifelse(opd$slideswhy == 0,
                              'No criteria',
                              ifelse(opd$slideswhy == 1,
                                     'Previous results in last 24h',
                                     ifelse(opd$slideswhy == 2,
                                            'Not authorized',
                                            NA)))
    # 
    # *Number of Sample Identification (NIDA) if a previous sample had been collected 
    # gen nidalen=length(brady2)
    # replace brady2="" if nidalen<6 
    opd$nidalen <- nchar(opd$brady2)
    opd$brady2 <- ifelse(opd$nidalen < 6,
                         NA,
                         opd$brady2)
    
    # *Create a new NIDA variable Supposing length 6 and 7 are missing the ".0"
    # gen nida2=brady2 
    opd$nida2 <- opd$brady2
    # replace nida2=brady2+".0" if nidalen==6 | nidalen==7
    opd$nida2 <- ifelse(opd$nidalen %in% c(6, 7),
                        paste0(opd$brady, '.0'),
                        opd$nida2)
    # drop nidalen
    opd$nidalen <- NULL
    
    # *<<<< INFORMATION ABOUT CURRENT ILLNESS <<<<<<< ####################################
    #   
    # *Has fever
    # replace fever2yno=. if fever2yno<1 | fever2yno>2
    opd$fever2yno <- 
      ifelse(opd$fever2yno <1 | opd$fever2yno > 2,
             NA,
             opd$fever2yno)
    # recode fever2yno 2=0
    opd$fever2yno <- 
      ifelse(opd$fever2yno == 2,
             0,
             opd$fever2yno)
    # lab values fever2yno yno 
    opd$fever2yno <- ifelse(opd$fever2yno == 1, TRUE, 
                            ifelse(opd$fever2yno == 2, FALSE, NA))
    
    
    # *Days of fever reported 
    # replace feverdays=. if feverdays<0
    opd$feverdays <-
      ifelse(opd$feverdays <0,
             NA,
             opd$feverdays)
    
    # *Has cough 
    # replace coughyno=. if coughyno<1 | coughyno>2
    opd$coughyno <- ifelse(opd$coughyno < 1 | opd$coughyno > 2,
                           NA,
                           opd$coughyno)
    # recode coughyno 2=0
    opd$coughyno[opd$coughyno == 2] <- 0
    # lab values coughyno yno  
    opd$coughyno <- ifelse(opd$coughyno == 1, TRUE,
                           ifelse(opd$coughyno == 2, FALSE,
                                  NA))
    # *Days of cough reported 
    # replace coughdays="" if coughdays=="NA"
    # destring coughdays, replace 
    # replace coughdays=. if coughdays<0
    opd$coughdays <- ifelse(opd$coughdays < 0, NA, opd$coughdays)
    
    # This is getting a bit repetive. Going to functionalize
    beafy <- function(var){
      var <- ifelse(var < 1 | var > 2,
                    NA,
                    var)
      var[var == 2] <- 0
      var <- ifelse(var == 1, TRUE,
                    ifelse(var == 0, FALSE,
                           NA))
      return(var)
    }
    
    # *Difficulties breathing 
    # replace breathyno=. if breathyno<1 | breathyno>2
    # recode breathyno 2=0 
    # lab values breathyno yno
    opd$breathyno <- beafy(opd$breathyno)
    
    
    # *Days of difficulties in breathing reported
    # replace breathdays="" if breathdays=="NA"
    # destring breathdays, replace 
    # replace breathdays=. if breathdays<0
    opd$breathdays <- ifelse(opd$breathdays < 0, NA,
                             opd$breathdays)
    
    # 
    # *Has diarrhea 
    # replace diarryno=. if diarryno<1 | diarryno>2
    # recode diarryno 2=0 
    # lab values diarryno yno 
    opd$diarryno <- beafy(opd$diarryno)
    
    # *Days of Diarrhea 
    # replace diarrdays=. if diarrdays<0
    opd$diarrdays <- ifelse(opd$diarrdays < 0, NA, opd$diarrdays)
    
    # *If diarrhea, number of Diarrheas reported 
    # replace diarrnum=. if diarrnum<0
    opd$diarrnum <- ifelse(opd$diarrnum < 0, NA, opd$diarrnum)
    
    # *If diarrhea, characteristics of feces 
    # replace diarchar=. if (diarchar<1 | diarchar>2)
    opd$diarchar <- ifelse(opd$diarchar < 1 | opd$diarchar > 2, 
                           NA,
                           opd$diarchar)
    # replace diarchar=diarchar-1
    opd$diarchar <- opd$diarchar - 1
    # lab define diarcharlab 0 "Wattery" 1 "Bloody"
    # lab values diarchar diarcharlab
    opd$diarcharlab <- ifelse(opd$diarchar == 0, 'Wattery',
                              ifelse(opd$diarchar == 2, 'Bloody', NA))
    
    # *Vomits 
    # replace vomityno=. if vomityno<1 | vomityno>2
    # recode vomityno 2=0
    # lab values vomityno yno 
    opd$vomityno <- beafy(opd$vomityno)
    
    # *Days of Vomit reported 
    # replace vomitdays="" if vomitdays=="NA"
    # destring vomitdays, replace 
    # replace vomitdays=. if vomitdays<0
    opd$vomitdays <-
      ifelse(opd$vomitdays < 0, NA, opd$vomitdays)
    # 
    # *Has seizures
    # replace fittedyno=. if fittedyno<1 | fittedyno>2
    # recode fittedyno 2=0 
    # lab values fittedyno yno 
    opd$fittedyno <- beafy(opd$fittedyno)
    
    # *NUmber of seizures 
    # replace fittednum="" if fittednum=="NA"
    # destring fittednum, replace 
    # replace fittednum=. if fittednum<0
    opd$fittednum <- ifelse(opd$fittednum <0, NA,
                            opd$fittednum)
    
    # *Burns
    # replace burnyno=. if burnyno<1 | burnyno>2
    # recode burnyno 2=0
    # lab values burnyno yno 
    opd$burnyno <- beafy(opd$burnyno)
    
    # *Accidents	
    # replace accidenyno=. if accidenyno<1 | accidenyno>2
    # recode accidenyno 2=0
    # lab values accidenyno yno 
    opd$accidenyno <- beafy(opd$accidenyno)
    
    # *Other Symptoms 
    
    # *<<<< PHYSICAL EXAMINATION <<<<<<<##########################
    #   replace fontanelle=. if (fontanelle<1 | fontanelle>4)
    opd$fontanelle <-
      ifelse(opd$fontanelle <1 | opd$fontanelle > 4,
             NA,
             opd$fontanelle)
    # replace fontanelle=4 if age>18
    opd$fontanelle[opd$age > 18] <- 4
    # replace deshidrat=. if (deshidrat<1 | deshidrat>4)
    opd$deshidrat <- ifelse(opd$deshidrat <1 | opd$deshidrat > 4, NA,
                            opd$deshidrat)
    
    #   *<<<<< OUTPATIENT DIAGNOSIS AND TREATMENT <<<<<<<< ####################
    #   
    #   *Thicksmear results (Number of Cross based on Microscopy Reading)
    # destring thicksmear, force replace 
    opd$thicksmear <- as.numeric(as.character(opd$thicksmear))
    # replace thicksmear=. if thicksmear>6
    opd$thicksmear[opd$thicksmear > 6] <- NA
    
    # *Identify Positive Slides 
    # gen slidepos=thicksmear
    opd$slidepos <- opd$thicksmear
    # recode slidepos 1/5=1 6=.
    opd$slidepos <- 
      ifelse(opd$slidepos %in% 1:5,
             1,
             NA)
    # label define posneg 1 "Pos" 2 "Neg" # 
    # original error in stata code, corrected here
    # label values slidepos posneg
    opd$slidepos <- ifelse(opd$slidepos == 1, 'Pos',
                           ifelse(opd$slidepos == 0, 'Neg', 
                                  NA))
    
    # *Clean RDT results 
    # replace rdt=. if (rdt==0 | rdt>=3) 
    #   label values rdt posneg
    opd$rdt[opd$rdt %in% c(0, 3)] <- NA
    opd$rdt <- ifelse(opd$rdt == 1, 'Pos',
                      ifelse(opd$rdt == 2, 'Neg', NA))
    # SERVOLAB 
    # Source servolab functions (Agnaldo's work)
    # library(ServolabR)
    # servo_lab_functions <- dir('servo_lab_functions/')
    # for (i in 1:length(servo_lab_functions)){
    #   source(paste0('servo_lab_functions/',
    #                servo_lab_functions[i]))
    # }
    
    ## Establish a connection using the servolabr package
    # (Run the below to install the package)
    # library(devtools)
    # library(roxygen2)
    # document('ServolabR/'); install('ServolabR/')
    # library(ServolabR)
    # servo_credentials <- read.delim('credentials/servo_lab_credentials.txt',
    #                                 header = FALSE)$V1
    # servo_credentials <- as.character(servo_credentials)
    # servo <- ServolabGetConnection("172.16.234.223",
    #                                servo_credentials[1],
    #                                servo_credentials[2])
    # nidas <- data_frame(nida = as.character(sort(unique(opd$nida))))
    # nidas <- nidas[!is.na(nidas$nida),]
    # x <- rep(NA, length(nidas$nida))
    # for (i in 1:length(x)){
    #   message(i)
    #   x[i] <-ServolabGetResultsByNidas(nidasVector = nidas$nida[i],
    #                                 methodID = c(1802, 1813),
    #                                 servoConnection = servo)
    # }
    # 
    # 
    # # Join back to opd
    # 
    # *Packed Cell Volume (PCV) --> IMPORT FROM SERVOLAB  # ASK BEA
    # /*replace pcv=. if date<mdy(08,27,1998)
    # gen an33=(pcv<33) if pcv!=.
    # gen an25=(pcv<25) if pcv!=.
    # gen an15=(pcv<15) if pcv!=.
    # label define anemia33 0 "pcv>=33" 1 "pcv<33" 
    # label values an33 anemia33
    # label define anemia25 0 "pcv>=25" 1 "pcv<25" 
    # label values an25 anemia25
    # label define anemia15 0 "pcv>=15" 1 "pcv<15" 
    # label values an15 anemia15
    # 
    # gen anemiagrp=pcv if pcv!=.
    # recode anemiagrp min/14=4 14/24=3 24/32=2 32/max=1
    # label define anemiagrp 4 "pcv<15" 3 "pcv 15-<25" 2 "pcv 25-<33" 1 "pcv=>33" 
    # label values anemiagrp anem*/
    #   
    # <<<<< INPATIENT DIAGNOSIS AND TREATMENT <<<<<<<< #############
    #   
    # label define afterpcd 1 "casa" 2 "icd" 3 "transf" 4 "aband"
    # label values afterpcd afterpcd
    opd$afterpcd <- ifelse(opd$afterpcd == 1, 'casa',
                           ifelse(opd$afterpcd == 2, 'icd',
                                  ifelse(opd$afterpcd ==3, 'transf',
                                         ifelse(opd$afterpcd == 4, 'aband', NA))))
    # label define aftericd 1 "casa" 2 "internam" 3 "transf" 4 "obito" 5 "aband"
    # label values aftericd aftericd
    opd$aftericd <- ifelse(opd$aftericd == 1, 'casa',
                           ifelse(opd$aftericd == 2, 'internam',
                                  ifelse(opd$aftericd == 3, 'transf',
                                         ifelse(opd$aftericd == 4, 'obito',
                                                ifelse(opd$aftericd == 5, 'aband', NA)))))
    
    # *END OF OUTPATIENT QUESTIONNAIRE 
    # *<<<<<<< ADDITIONAL DATA MANAGEMENT <<<<<<	###################################
    #   
    #   *MALARIA	
    # *Create a malaria diagnosis variable 
    # gen malpos=slidepos
    # replace malpos=rdtpos if malpos==.  rdtpos does not exit - using rdt
    opd$malpos <- opd$slidepos
    opd$malpos <- ifelse(is.na(opd$malpos), opd$rdt, opd$malpos)
    # 
    # *Seasons
    # keep if date >= d(01jul1997) & date <= d(30jun2014)
    # For our purposes go through June 30 2016
    opd <- opd %>%
      filter(date >= '1997-07-01',
             date <= '2016-06-30')
    
    # gen season=.
    opd$season <- NA
    # replace season=1 if date>=d(01jul1997) & date <= d(30jun1998)
    # replace season=2 if date>=d(01jul1998) & date <= d(30jun1999)
    # replace season=3 if date>=d(01jul1999) & date <= d(30jun2000)
    # replace season=4 if date>=d(01jul2000) & date <= d(30jun2001)
    # replace season=5 if date>=d(01jul2001) & date <= d(30jun2002)
    # replace season=6 if date>=d(01jul2002) & date <= d(30jun2003)
    # replace season=7 if date>=d(01jul2003) & date <= d(30jun2004)
    # replace season=8 if date>=d(01jul2004) & date <= d(30jun2005)
    # replace season=9 if date>=d(01jul2005) & date <= d(30jun2006)
    # replace season=10 if date>=d(01jul2006) & date <= d(30jun2007)
    # replace season=11 if date>=d(01jul2007) & date <= d(30jun2008)
    # replace season=12 if date>=d(01jul2008) & date <= d(30jun2009)
    # replace season=13 if date>=d(01jul2009) & date <= d(30jun2010)
    # replace season=14 if date>=d(01jul2010) & date <= d(30jun2011)
    # replace season=15 if date>=d(01jul2011) & date <= d(30jun2012)
    # replace season=16 if date>=d(01jul2012) & date <= d(30jun2013)
    # replace season=17 if date>=d(01jul2013) & date <= d(30jun2014)
    # replace season=18 if date>=d(01jul2014) & date <= d(30jun2015)
    # replace season=19 if date>=d(01jul2015) & date <= d(30jun2016)
    opd$season <- 
      floor((as.numeric(opd$date - as.Date('1997-07-01')) / 365.25))
    # 
    # *Wet and Rainy Season 
    # gen seastrad=mon
    opd$seastrad <- opd$mon
    # recode seastrad 1/4=1 5/10=2 11/12=1
    opd$seastrad <- ifelse(opd$seastrad %in% 1:4,
                           1,
                           ifelse(opd$seastrad %in% 5:10,
                                  2,
                                  ifelse(opd$seastrad == 12,
                                         1,
                                         NA)))
    # label define season 1 "rainy" 2 "dry"
    # label values seastrad season		
    opd$seastrad <- ifelse(opd$seastrad == 1, 'rainy',
                           ifelse(opd$seastrad == 2, 'dry', NA))
    # 
    # 
    # save $dta/pri/opd_tot, replace 
    # 
    # 
    # # # USE PDF TO GET EXAM FISICO AND DIAGNOSIS (WHO ICD codes) #################
    # Use this ICD package: https://jackwasey.github.io/icd/
    # library(devtools) 
    # devtools::install_github("wtcooper/icdcoder")
    # library(icdcoder)
    icd_columns <-
      names(opd)[grepl('icd', names(opd))]
    icd_columns <- icd_columns[icd_columns != 'aftericd']
    
    # Create explanation columns
    for (j in 1:length(icd_columns)){
      opd[,paste0(icd_columns[j],
                  '_explanation')] <- NA
    }
    for (j in 1:length(icd_columns)){
      the_column <- opd[,icd_columns[j]]
      the_column <- as.character(the_column)
      the_column[the_column == ''] <- NA
      the_column <- trimws(the_column, which = 'both')
      result <- icd9Explain(the_column)
      opd[,paste0(icd_columns[j],
                  '_explanation')] <- result
    }
    
    # Create a simple malaria boolean
    opd$malaria <- 
      ifelse(opd$malpos == 'Pos',
             TRUE,
             FALSE)
    opd$malaria[is.na(opd$malaria)] <- FALSE
    
    # Create a simple diarrhea boolean
    opd$diarrhea <- opd$diarryno
    opd$diarrhea[is.na(opd$diarrhea)] <- FALSE
    
    # Save a snapshot
    save(opd,
         file = 'data/opd_cleaned.RData')
  }
  
  # Load in census data
  if('census_data.RData' %in% dir('data')){
    load('data/census_data.RData')
  } else {
    residency <- cism::get_data(tab = 'residency',
                                dbname = 'openhds',
                                port = 3306)
    individual <- cism::get_data(tab = 'individual',
                                 dbname = 'openhds',
                                 port = 3306)
    location <- cism::get_data(tab = 'location',
                               dbname = 'openhds',
                               port = 3306)
    save(residency,
         individual,
         location,
         file = 'data/census_data.RData')
  }
  
  # Read in expansion data
  expansion <- read_excel('data/Bairros de area de Expansao_Demo.xls',
                          skip = 3)$Bairro
  
  # Create a time at risk dataset for the opd period
  if('cleaned_time_at_risk.RData' %in% dir('data')){
    load('data/cleaned_time_at_risk.RData')
  } else {
    
    # Create time at risk
    tar <- cism::create_time_at_risk(residency = residency,
                                     individual = individual,
                                     location = location)
    
    # Keep only those who are in the non-expanded zone
    # (ie, bairro <= 3499) (need to confirm with charfudin)
    tar$bairro <- as.numeric(as.character(substr(tar$locationName, 1, 4)))
    tar <- tar %>%
      filter(!bairro %in% expansion)
    
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
         file = 'data/cleaned_time_at_risk.RData')
  }
  
  if(file.exists('data/cleaned_inpd.RData')){
    load('data/cleaned_inpd.RData')
  } else {
    
    # Read in inpd data
    inpd <- readr::read_csv('data/inpd_2016-01-22.csv')
    
    # Clean up dates
    inpd$date <-
      as.Date(inpd$date,
              format = '%d%b%Y')
    inpd$dish_date <-
      as.Date(inpd$dish_date,
              format = '%d%b%Y')
    
    # Clean up fever
    inpd$fever <-
      ifelse(inpd$feveryno == 1, TRUE,
             ifelse(inpd$feveryno == 2, FALSE,
                    NA))
    
    # Clean up diarrhea
    inpd$diarrhea <-
      ifelse(inpd$diarryno == 1, TRUE,
             ifelse(inpd$diarryno == 2, FALSE,
                    NA))
    
    # Keep only those dates beginning in 2001 
    # (everyting prior is sporadic)
    inpd <- inpd %>%
      filter(date >= '2001-01-01')
    
    # Keep only those 0-15 years old, like opd
    inpd$dob <- 
      as.Date(paste0(inpd$yea_birth, '-',
                     inpd$mon_birth, '-',
                     inpd$day_birth))
    inpd <- inpd %>%
      mutate(age = as.numeric(date - dob) / 365.25) %>%
      filter(age >= 0,
             age < 15)
    
    # Get malaria (mal_tt? ask bea)
    inpd <- inpd %>%
      mutate(malaria = ifelse(mal_tt == 1, TRUE,
                              ifelse(mal_tt == 2, FALSE,
                                     NA)))
    save(inpd,
         file = 'data/cleaned_inpd.RData')
  }
  save.image('data/all_data.RData')
}

# Divide age groups
opd$age_group <- cut(opd$age / 12,
                     c(0, 1, 3, 5, 10, 15),
                     include.lowest = TRUE)

# Divide age groups
inpd$age_group <- cut(inpd$age,
                     c(0, 1, 3, 5, 10, 15),
                     include.lowest = TRUE)
