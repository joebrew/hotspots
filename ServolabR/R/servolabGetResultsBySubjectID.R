
#' Get Sample Results from Servolab
#'
#' The function takes in a study subject patern and creates a SQL statement to query the database and retrieve all records with the patientID matching the patern
#' @param subjectID the subjectID pattern ex: "GAMA" ,"XMAL","VIMAG", etc.
#' @param methodID a vector (integer) containing the identification number for a certain lab technique,  if you need all results ust create an ampty vector like c("")
#' @param year    the year when the samples were processed, if you dont want to specify , leave it empty: ""
#' @return  a data frame with lab results
#' @usage ServolabGetResultsBySubjectID(subjectID, methodID, year,servoConnection)
#' @examples
#' # First create a connection to servolab
#' con <- ServolabGetConnection("xxx.xxx.xxx.xxx",username,password)
#' # Then get the results
#' patern <- "INPD"
#' year <- 2014
#' method <- c(3212,3245)
#' result <- ServolabGetResultsBySubjectID(patern,method,year,con)
#' @export
ServolabGetResultsBySubjectID <-
  function(subjectID,
           methodID,
           year,
           servoConnection) {
    results <- data.frame()

    if (!is.character(subjectID)) {
      stop(paste("Error! Character  expected. Found :", class(subjectID)))
    }

    if (subjectID=="") {
      stop(paste("Error! Null subjectID"))
    }
    if (!is.vector(methodID)) {
      stop(paste("Error! vector expected. Found :", class(methodID)))
    }
    if (is.null(servoConnection)) {
      stop(paste("Error! null connection"))

    }  else if (is.character(subjectID)) {
      if (length(subjectID) == 0) {
        stop("subjectID  is empty")

      }

      else{
        if (length(methodID)==1 & methodID[1]=="") {
          if (year == "")
          {
            results <-
              dbGetQuery(
                servoConnection,
                paste0(
                  "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                  a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                  FreiTextNr , MaterialNr , IsolatNr,MessDatum as date_result  from Untersuchung  a  inner join Methode
                  b on a.MethNr = b.MethNr  where AZ like '%",
                  subjectID,
                  "%' union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Luntersuchung c
                  inner join Methode  d on c.MethNr = d.MethNr where AZ like '%",
                  subjectID,
                  "%'order by AZ"
                )
              )
          }
          else{
            if (is.numeric(year)) {
              results <-
                dbGetQuery(
                  servoConnection,
                  paste0(
                    "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                    a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                    FreiTextNr , MaterialNr , IsolatNr,MessDatum as date_result  from Untersuchung  a  inner join Methode
                    b on a.MethNr = b.MethNr  where AZ like '%",
                    subjectID,
                    "%'" ,
                    " and YEAR(EinDat)=",
                    year,
                    " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Luntersuchung c
                    inner join Methode  d on c.MethNr = d.MethNr where AZ like '%",
                    subjectID,
                    "%'",
                    " and YEAR(EinDat)=",
                    year
                  )
                )

            }
            else{
              stop("year must be numeric")
            }
          }



        }
        else{
          if (is.numeric(methodID) & is.vector(methodID)) {
            if (year == "")
            {
              results <-
                dbGetQuery(
                  servoConnection,
                  paste0(
                    "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                    a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                    FreiTextNr , MaterialNr , IsolatNr,MessDatum as date_result  from Untersuchung  a  inner join Methode
                    b on a.MethNr = b.MethNr  where AZ like '%",
                    subjectID,
                    "%' " ,
                    " and a.MethNr in ",
                    convertVectorSql(methodID),
                    " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Luntersuchung c
                    inner join Methode  d on c.MethNr = d.MethNr where AZ like '%",
                    subjectID,
                    "%' " ,
                    " and c.MethNr in ",
                    convertVectorSql(methodID)
                  )
                )
            } else{
              if (is.numeric(year)) {
                results <-
                  dbGetQuery(
                    servoConnection,
                    paste0(
                      "Select AZ as subject_id, AuftrNr as nida, a.LabNr,EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, a.LabBerNr as work_area,
                      a.MethNr , b.MethName,  Resultat , ResStat as nivel_patol , ResTyp , TextNr , KurzText ,
                      FreiTextNr , MaterialNr , IsolatNr,MessDatum as date_result  from Untersuchung  a  inner join Methode
                      b on a.MethNr = b.MethNr  where AZ like '%",
                      subjectID,
                      "%'" ,
                      " and YEAR(EinDat)=",
                      year,
                      " and a.MethNr in ",
                      convertVectorSql(methodID),
                      " union all select AZ as subject_id, AuftrNr as nida, c.LabNr, EinDat  as date_reception, ZaehlerR as cont_meth, StatusL, c.LabBerNr as work_area, c.MethNr , d.MethName,  Resultat , ResStat as nivel_patol, ResTyp , TextNr , KurzText , FreiTextNr , MaterialNr , IsolatNr , MessDatum as date_result from Luntersuchung c
                      inner join Methode  d on c.MethNr = d.MethNr where AZ like '%",
                      subjectID,
                      "%'",
                      " and YEAR(EinDat)=",
                      year,
                      " and c.MethNr in ",
                      convertVectorSql(methodID)
                    )
                  )

              }
              else{
                stop("year must be numeric")
              }


            }




          } else {
            stop("methoID must be numeric vector Found :",
                 class(subjectID))
          }
        }


      }

      }



    if (dim(results)[1] == 0) {
      stop("No results found in servolab database")
    }
    if (dim(results)[1] > 0) {
      ## remove records with status 'k' -> killed
      results <- results[which(results$nida != 'K'),]
      results$date_reception <-
        substr(results$date_reception, 1, 10)
      results$date_result <- substr(results$date_result, 1, 10)

      ## Remove trailling spaces
      results$nida <- trimws(x = results$nida, which = "both")
    }

    return(results)
    }

