
#' Get Sample Results from Servolab
#'
#' The function takes in an vector of Nidas and creates a SQL statement to query the database
#' @param nidasVector an vector containing NIDAS
#' @param methodID a vector (integer) which contains identification number for a certain lab technique,  if you need all results just create an ampty vector like c("")
#' @param servoConnection  a JDBCConnection  to servolab
#' @return  returns a data frame with lab results
#' @usage  ServolabGetResultsByNidas(nidasVector, methodID,servoConnection)
#' @examples
#' # First create a connection to servolab
#' con <- ServolabGetConnection("xxx.xxx.xxx.xxx",username,password)
#' # Then get the results
#' method_id   <- c(3212,3245)
#' result <- ServolabGetResultsByNidas(c('1234567.8','15555567.5'),method_id,con)
#' @export
ServolabGetResultsByNidas <-
  function(nidasVector, methodID, servoConnection) {
    results <- data.frame()

    if (!is.vector(nidasVector)) {
      stop(paste("Error! vector expected. Found :", class(nidasVector)))
    }
    if (!is.vector(methodID)) {
      stop(paste("Error! vector expected. Found :", class(methodID)))
    }
    if (is.null(servoConnection)) {
      stop(paste("Error! null connection"))

    }
    if(is.vector(nidasVector)) {
      if (length(nidasVector) == 0) {
        stop("Nidas vector is empty")

      }
      else{
        if (methodID[1] =="") {
          results <-
            dbGetQuery(
              servoConnection,
              paste0(
                "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Untersuchung  a  inner join Methode
                b on a.MethNr = b.MethNr  where AuftrNr in ",
                nidasToString(nidasVector),
                " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr, MessDatum as date_result  from Luntersuchung c
                inner join Methode  d on c.MethNr = d.MethNr where AuftrNr in ",
                nidasToString(nidasVector)
              )
            )


        }
        else{
          results <-
            dbGetQuery(
              servoConnection,
              paste0(
                "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Untersuchung  a  inner join Methode
                b on a.MethNr = b.MethNr  where a.MethNr in ",
                convertVectorSql(methodID),
                " and AuftrNr in ",
                nidasToString(nidasVector),
                " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr, MessDatum as date_result  from Luntersuchung c
                inner join Methode  d on c.MethNr = d.MethNr where c.MethNr in ",
                convertVectorSql(methodID),
                " and AuftrNr in ",
                nidasToString(nidasVector)
              )
            )



        }



      }

    }
    if (dim(results)[1] == 0) {
      cat("No results found in servolab database")
    }
    if (dim(results)[1] > 0) {
      ## remove  records with status 'k' -> killed
      results <- results[which(results$nida != 'K'),]

      results$date_reception <-
        substr(results$date_reception, 1, 10)
      results$date_result <- substr(results$date_result, 1, 10)
      ## Remove trailling spaces
      results$nida <- trimws(x = results$nida, which = "both")

      ## Report nidas not found
      #     vec <- completeNidas(nidasVector)
      #     not_found <- vec[which(!(vec %in% results$nida))]
      #     if (length(not_found) > 0)
      #     {
      #       print(paste("Este nida nao foi encontrado no servolab:",not_found))
      #       filename <- paste0(getwd(),"/nidas_not_found.csv")
      #       write.csv(not_found,filename)
      #       print(filename)
      #
      #     }

    }

    return(results)
  }
