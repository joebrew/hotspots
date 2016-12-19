
################################ MISC Functions ####################################################################

############## Receive a vector of nidas and add .0 to nidas with lenght==7#######################

completeNidas <-  function(nidasVector) {
  ve <- nidasVector
  for (i in 1:length(ve))
  {
    nida <- ve[i]
    if (nchar(nida) == 7) {
      ve[i] <-  paste0(nida, ".0")
    }
  }

  return (ve)

}



####################################################################################################
############## Receive a vector of nidas and return a string to use in mysql #######################
##############       ('nida1','nida2','nida3','nida4')                       #######################

nidasToString <-  function(nidasVector) {
  stringNidas <- ""
  for (i in 1:length(nidasVector))
  {
    nida <- nidasVector[i]

    if(!(is.na(nida) | is.null(nida))) {

      if (nchar(nida) == 7) {
        nida <- paste0(nida, ".0")
      }

      stringNidas <- paste0(stringNidas, "'", nida, "',")

    } #else{cat("there is a empty or incorect nida at postition",i, sep = " ")}


  }
  stringNidas <- substr(stringNidas, 1, nchar(stringNidas) - 1)
  stringNidas <- paste0("(", stringNidas, ")")
  return (stringNidas)

}


############## Convert a vector (method_id) to sql comma separet values #######################

convertVectorSql <- function(methodid_vector) {
  sql_vector = ""

  for (i in 1:length(methodid_vector)) {
    sql_vector = paste0(sql_vector, methodid_vector[i], ',')

  }
  sql_vector <- substr(sql_vector, 1, nchar(sql_vector) - 1)
  sql_vector <- paste0("(", sql_vector, ")")
  return (sql_vector)

}


#################### Make sure the date paramenter is in a valid format ###################################################


checkDatePattern <- function(date) {
  dateChecker = FALSE

  if (grepl(pattern = '[0][0-9]/[0-2][0-9]/[0-9]{4}|[1][0-2]/[0-2][0-9]/[0-9]{4}|[0][0-9]/[3][0-1]/[0-9]{4}|[1][0-2]/[3][0-1]/[0-9]{4}',
            x = date,
            perl = TRUE)) {
    dateChecker = TRUE

  }


  return(dateChecker)

}


############## Convert a vector (method_id) to sql comma separet values #######################
convertDateSql <- function(date) {
  return (paste0("'", date, "'"))

}

