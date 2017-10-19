#' Estabilish a connection to Servolab Database
#'
#' The function  connects to sybase using a JDBC driver. It takes in the host IP address
#' @param  servolabIPaddress the ipAddress of the server
#' @param  username database username
#' @param  password database password
#' @import RJDBC
#' @return returns  a jdbc connection to Servolab
#' @usage  ServolabGetConnection(servolabIPaddress,username,password)
#' @examples
#' host <-"172.11.11.11"
#' us <-"servo"
#' pass <- "pwd"
#' con <- ServolabGetConnection(host,us,pass)
#' @export

ServolabGetConnection <-
  function(servolabIPaddress, username, password) {
    conn <- NULL


    # JDBC function has two purposes. One is to initialize the Java VM and load a Java JDBC driver
    # (not to be confused with the JDBCDriver R object which is actually a DBI driver).
    # The second purpose is to create a proxy R object which can be used to a call dbConnect which
    # actually creates a connection.
    drv <-
      JDBC(
        driverClass = "net.sourceforge.jtds.jdbc.Driver",
        classPath = paste0(
          path.package("ServolabR", quiet = FALSE),
          "/java/jtds-1.3.1.jar"
        ),
        identifier.quote = "'"
      )
    conn <-
      dbConnect(
        drv,
        paste0("jdbc:jtds:sybase://", servolabIPaddress, ":5000/LAB"),
        username,
        password
      )

    return(conn)
  }


