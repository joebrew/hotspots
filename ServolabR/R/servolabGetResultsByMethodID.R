
#' Get Sample Results from Servolab
#'
#' The function takes in a study subject patern and creates a SQL statement to query the database and retrieve all records with the patientID matching the patern
#' @param methodID a vector (integer) containing the identification number for a certain lab technique,  if you need all results just  create an ampty vector like c("")
#' @param startDate   Date reception start
#' @param endDate     Date receptioin end
#' @return  a data frame with lab results
#' @usage ServolabGetResultsByMethodID( methodID, startDate, endDate, servoConnection)
#' @examples
#' # First create a connection to servolab
#' con <- ServolabGetConnection("xxx.xxx.xxx.xxx",username,password)
#' startDate = '05/22/2016'
#' endDate = '07/22/2016'
#' method <- c(3212,3245)
#' result <- ServolabGetResultsByMethodID(method,startDate,endDate,con)
#' @export
ServolabGetResultsByMethodID <-
  function (methodID,
            startDate ,
            endDate,
            servoConnection) {
    results <- data.frame()

    if (is.null(methodID)) {
      stop(paste("Error! Vector  expected. Found :", class(methodID)))
    }

    if (is.null(servoConnection)) {
      stop(paste("Error! null connection"))

    }
    if (methodID[1]=="") {
      stop(paste("You must provite a vector with methodID"))

    }
    if (!checkDatePattern(startDate)) {
      stop(paste("Error! wrong  startDate. expected mm/dd/yyyy"))
    }
    if (!checkDatePattern(endDate)) {
      stop(paste("Error! wrong  endDate  expected mm/dd/yyyy"))
    }
    sd =  as.Date(startDate, '%m/%d/%Y')
    ed =  as.Date(endDate, '%m/%d/%Y')
    if (!(is.vector(methodID) | is.numeric(methodID))) {
      stop(paste("methodID must be a numeric vector"))

    }
    if (sd > ed) {
      stop(paste("Error! startdate cannot be after enddate"))
    }
    else  {
      results <-
        dbGetQuery(
          servoConnection,
          paste0(
            "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area, a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,   FreiTextNr , MaterialNr , IsolatNr,MessDatum as date_result  from Untersuchung  a  inner join Methode  b on a.MethNr = b.MethNr  where  a.MethNr in ",
            convertVectorSql(methodID),
            " and EinDat between ",
            convertDateSql(startDate),
            " and ",
            convertDateSql(endDate),
            " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Luntersuchung c
            inner join Methode  d on c.MethNr = d.MethNr  and c.MethNr in ",
            convertVectorSql(methodID),
            " and EinDat between ",
            convertDateSql(startDate),
            " and ",
            convertDateSql(endDate)
          )
        )
    }

    if (dim(results)[1] == 0) {
      stop("No results found in servolab database")
    }
    if (dim(results)[1] > 0) {
      ## remove records with status 'k' -> killed
      results <- results[which(results$nida != 'K'),]
      results$date_reception <- substr(results$date_reception, 1, 10)
      results$date_result <- substr(results$date_result, 1, 10)

      ## Remove trailling spaces
      results$nida <- trimws(x = results$nida, which = "both")
    }

    return(results)

  }
