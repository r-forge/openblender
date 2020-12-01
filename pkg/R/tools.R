#'@title Date string to Unix timestamp.
#'@description Returns a Unix timestamp with a date or datetime string. It also accepts a list or vector of strings and returns a converted list of Unix timestamps. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param date_time_str String with a date in any format. For example: ‘6.2.1986 22:11:00'. This can also be a datetime string vector or the datetime column in your anchor.
#'@param date_format The format of the date string.
#'@param timezone String of timezone, Default GMT. To list possible timezones execute OlsonNames()
#'@example
#'\dontrun{
#'df_anchor$timestamp <- openblender::dateToUnix(date_time_str="08.10.2020 12:37:14",
#'                                               date_format="%d.%m.%Y %H:%M:%S",
#'                                               timezone="US/Eastern")
#'}
#'@return A double unix timestamp or a vector of those doubles.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
dateToUnix <- function(date_time_str, date_format, timezone="GMT") {
  if (!(timezone %in% OlsonNames())) {
    return(list(status="internal error", msg="Time zone not recognized. Consult allowed time zones with OlsonName()"))
  }
  if (typeof(date_time_str) == "character") {
    fecha <- as.POSIXct(date_time_str, date_format, tz=timezone)
    attributes(fecha)$tzone <- "GMT"
    ts <- as.integer(fecha)
    return(ts)
  } else {
    tss = c()
    for (dt_str in date_time_str) {
      fecha <- as.POSIXct(dt_str, date_format, tz=timezone)
      attributes(fecha)$tzone <- "GMT"
      tss <- append(tss, as.integer(fecha))
    }
    return(tss)
  }
}


#'@title Unix timestamp to Date string.
#'@description Returns a Unix timestamp with a date or datetime string. It also accepts a list or vector of strings and returns a converted list of Unix timestamps. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param unix_timestamp Unix timestamp as double. For example : 508133460 This can also be a vector of those timestamps or the timestamp column in your anchor.
#'@param date_format The format of the date string.
#'@param timezone String of timezone, Default GMT. To list possible timezones execute OlsonNames()
#'@examples
#'\dontrun{
#'df_anchor$timestamp <- openblender::dateToUnix(date_time_str=1602160634,
#'                                               date_format="%d.%m.%Y %H:%M:%S",
#'                                               timezone="US/Eastern")
#'}
#'@return A double unix timestamp or a vector of those doubles.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
unixToDate <- function(unix_timestamp, date_format="%d-%m-%Y %H:%M:%S", timezone="GMT") {
  if (!(timezone %in% OlsonNames())) {
    return(list(status="internal error", msg="Time zone not recognized. Consult allowed time zones with OlsonName()"))
  }
  if (typeof(unix_timestamp) == "double") {
    return(strftime(as.POSIXct(unix_timestamp, origin="1970-01-01", tz=timezone), format=date_format))
  } else {
    dts <- c()
    for (ts in unix_timestamp) {
      dts <- append(ts, strftime(as.POSIXct(unix_timestamp, origin="1970-01-01", tz=timezone), format=date_format))
    }
    return(dts)
  }
}
