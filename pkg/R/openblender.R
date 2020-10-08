#'@title Make HTTP request to \href{http://openblender.io}{openblender.io} services
#'@description Call 'OpenBlender' API services.
#'@param action Task you're requesting
#'@param parameters Request options
#'@examples
#'\dontrun{
#'#CREATE DATASET
#'df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
#'action <- "API_createDataset"
#'parameters <- list(
#'token = "YOUR TOKEN",
#'id_user = "YOUR USER ID",
#'name = "Assign a name",
#'descriptipon = "Set a description",
#'visibility = "public",
#'tags = list("topic", "tag"),
#'insert_observations = "off",# set "on" if you want include the observations
#'dataframe = df
#')
#'call(action, parameters)
#'#INSERT OBSERVATIONS
#'df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
#'action <- "API_insertObservations"
#'parameters <- list(
#'token = "YOUR TOKEN",
#'id_user = "YOUR USER ID",
#'id_dataset = "DATASET ID",
#'notification = "on",
#'observations = df
#')
#'call(action, parameters)
#'
#'#GET OBSERVATIONS
#'action <- "API_getObservationsFromDataset"
#'parameters <- list(
#'token = "YOUR TOKEN",
#'id_user = "YOUR USER ID",
#'id_dataset = "DATASET ID"
#')
#'call(action, parameters)
#'}
#'@return A list that includes the new dataset id in case you create one, success/error message when you insert observations or the list of observations requested.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
call <- function(action, parameters) {
  respuesta <- tryCatch({
    url <- getUrl(parameters)
    switch(action,
           API_createDataset = {
             respuesta <- create_dataset(parameters, url)
           },
           API_insertObservations = {
             respuesta <- insert_observations(parameters, url)
           },
           API_getObservationsFromDataset = {
             respuesta <- get_observations(parameters, url)
           },
           API_powerModel = {
             respuesta <- power_model(parameters, url)
           },
           API_getDataWithVectorizer = {
             respuesta <- getDataWithVectorizer(parameters, url)
           },
           API_getOpenTextData = {
             respuesta <- getOpenTextData(parameters, url)
           },
           {
             data <- list(action = action, json = parameters)
             respuesta <- dameRespuestaLlamado(url, data)
            }
           )
    return(respuesta)
  },
  error = function(e) {
    if ("oblender" %in% attributes(parameters)$names && parameters$oblender == 1) {
      warning(paste("internal error", e))
    } else {
      warning(list(status = "internal error openblender", msg = e))
    }
    return(list(status = "internal error", msg = e))
  })
  return(respuesta)
}

timeBlend <- function(token, anchor_ts, blend_source,
                      blend_type = "closest_observation",
                      direction = "time_prior",
                      interval_output = "count",
                      ts_restriction = NULL,
                      oblender = FALSE,
                      interval_size = 3600,
                      consumption_confirmation = "off",
                      missing_values = "raw",
                      data_format = "dataframe"){
  tryCatch({
    url <- getUrl(list(), oblender = oblender)
    if (typeof(anchor_ts) == "list") {
      anchor_ts <- unname(anchor_ts["timestamp"])[[1]]
    }
    anchor_ts <- sort(anchor_ts)
    json_parametros <- list(
      token=token,
      anchor_ts=anchor_ts,
      blend_source=blend_source,
      blend_type=blend_type,
      direction=direction,
      agg_output=interval_output,
      ts_restriction=ts_restriction,
      agg_interval_size=interval_size,
      missing_values=missing_values)
    task_params <- list(token=token, number_of_rows=length(anchor_ts), blend_source=blend_source, consumption_confirmation=consumption_confirmation)
    task <- initializeTask(task_params, url)
    if ("id_dataset" %in% attributes(blend_source)$names) {
      tam_ini <- 1000
    } else {
      tam_ini <- 350
    }
    if (task$confirm == "y") {
      message("Task confirmed. Starting download...")
      df_resp <- NULL
      resp_vacio <- TRUE
      universe_size <- length(anchor_ts)
      if (length(anchor_ts) <= tam_ini) {
        piece_size <- length(anchor_ts)
      } else {
        piece_size <- tam_ini
      }
      secuencia <- seq(0, universe_size - 1, by = piece_size)
      for (i_act in secuencia) {
        if ((i_act + piece_size) < universe_size) {
          val <- (i_act + piece_size) / universe_size
        } else {
          val <- 1
        }
        progreso <- round(val, 2)
        json_parametros$consumption_id <- task$consumption_id
        json_parametros$r_version <- packageVersion("openblender")
        json_parametros$progreso <- progreso
        json_parametros$anchor_ts <- anchor_ts[(i_act + 1):(i_act + piece_size)]
        data <- list(action="API2_getTimeBlend", json=json_parametros)
        respuesta <- dameRespuestaLlamado(url, data)
        if (typeof(respuesta) == "list" && respuesta$status == "success") {
          message(paste(progreso * 100, "%"))
          if (resp_vacio) {
            df_resp <- clean_dataframe(respuesta$df_resp, rep = 2)
            df_resp <- df_resp[order(df_resp$timestamp), ]
            row.names(df_resp) <- NULL
            resp_vacio <- FALSE
          } else {
            df_piece <- clean_dataframe(respuesta$df_resp, rep = 2)
            df_resp <- rbind(df_resp, df_piece)
            df_resp <- df_resp[order(df_resp$timestamp), ]
            row.names(df_resp) <- NULL
          }
        } else {
          message(respuesta)
        }
      }
      if (data_format == "dataframe") {
        return(df_resp)
      } else {
        return(toJSON(df_resp, pretty=TRUE, auto_unbox = TRUE))
      }
    } else {
      message("")
      message("Task cancelled. To execute tasks without prompt set 'consumption_confirmation' to 'off'.")
      return(list(status="cancelled"))
    }
  },
  error = function(e) {
    return(list(status="internal error", msg=str(e)))
  })
}

#'@title Search Time Blends.
#'@description Search Time Blends that intersect your anchor in time providing a string. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param token Obtained in \href{https://www.openblender.io/#/user/account?section=account}{openblender.io}
#'@param anchor_ts Dataframe with a timestamp column in unix timestamp format.
#'@param search_text Text provided to find Time Blends.
#'@examples
#'\dontrun{
#'
#'}
#'@return A list that includes the new dataset id in case you create one, success/error message when you insert observations or the list of observations requested.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
searchTimeBlends <- function(token, anchor_ts, search_text, oblender=FALSE) {
  tryCatch({
    url <- getUrl(list(), oblender = oblender)
    json_parametros = list(
      token=token,
      anchor_max=max(anchor_ts),
      anchor_min=min(anchor_ts),
      search_text=search_text)
    json_parametros$r_version <- packageVersion("openblender")
    data <- list(action = "API2_searchTimeBlends", json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    if (respuesta$status == "success") {
      return(respuesta$blends)
    } else {
      message(respuesta)
    }
  },
  error = function(e) {
    return(list(status="internal error", msg=str(e)))
  })
}

dateToUnix <- function(dt_string, dt_format, timezone="GMT") {
  if (!(timezone %in% OlsonNames())) {
    return(list(status="internal error", msg="Time zone not recognized. Consult allowed time zones with OlsonName()"))
  }
  if (typeof(dt_string) == "character") {
    fecha <- as.POSIXct(dt_string, dt_format, tz=timezone)
    attributes(fecha)$tzone <- "GMT"
    ts <- as.integer(fecha)
    return(ts)
  } else {
    tss = c()
    for (dt_str in dt_string) {
      fecha <- as.POSIXct(dt_str, dt_format, tz=timezone)
      attributes(fecha)$tzone <- "GMT"
      tss <- append(tss, as.integer(fecha))
    }
    return(tss)
  }
}

unixToDate <- function(unix_ts, dformat="%d-%m-%Y %H:%M:%S", timezone="GMT") {
  if (!(timezone %in% OlsonNames())) {
    return(list(status="internal error", msg="Time zone not recognized. Consult allowed time zones with OlsonName()"))
  }
  if (typeof(unix_ts) == "double") {
    return(strftime(as.POSIXct(unix_ts, origin="1970-01-01", tz=timezone), format=dformat))
  } else {
    dts <- c()
    for (ts in unix_ts) {
      dts <- append(ts, strftime(as.POSIXct(unix_ts, origin="1970-01-01", tz=timezone), format=dformat))
    }
    return(dts)
  }
}
