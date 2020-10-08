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


#'@title Time Blend.
#'@description Get Dataframe with DataToBlend ordered time by sending a list or Series of Anchor timestamps. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param token Obtained in \href{https://www.openblender.io/#/user/account?section=account}{openblender.io}
#'@param anchor_ts Array of Unix timestamp values to blend data to.
#'@param blend_source JSON object of Data To Blend configuration obtained from a data source in OpenBlender.io
#'@param blend_type Type of blend to gather data: 'closest_observation', 'agg_in_intervals', (Default: closest_observation).
#'@param direction (Only if blend_type = 'agg_in_intervals'). Select chronological direction of observation or interval to aggregate from 'blend source' to 'anchor': 'time_prior': Gather data from blend source prior or at the same time as each observation from anchor. 'time_prior_strict': Gather data from blend source strictly prior to each observation from anchor. 'time_post': Gather data subsequent to or at the same time than each observation from anchor. 'time_post_strict': Gather data strictly subsequent to each observation from anchor. 'time_prior_and_post': Gather data prior and subsequent to each observation from anchor. Default (time_prior)
#'@param interval_size (Only if blend_type = 'agg_in_intervals'). Time interval size in seconds to be used with the selected agg_interval_order.
#'@param interval_output (Only if blend_type = 'agg_in_range'), options: 'count' (any type of feature). 'sum' 'avg' (for numerical features). 'text', 'text_list' (for text features). (Default = count)
#'@param missing_values Treatment of missing values. 'raw': Ignore missing values (return NaN). 'impute': Impute missing values. (Default 'raw')
#'@param ts_restriction None
#'@param consumption_confirmation "on" or "off", default "on"
#'@param data_format Default "Dataframe"
#'@param oblender Omit this parameter
#'@examples
#'\dontrun{
#'#Blend a dataset
#'#You provide df_anchor data
#'blend_source <- list(
#'id_dataset="5f063dda198b5854284dae58",
#'feature="price",
#'dataset_filter=c(list(low=list("$gt"=300)))
#')
#'
#'df_blend <- openblender::timeBlend(token = "YOUR_TOKEN",
#'                                   anchor_ts = df_anchor$timestamp,
#'                                   blend_type = "agg_in_intervals",
#'                                   direction = "time_prior",
#'                                   interval_output = "avg",
#'                                   interval_size = 60 * 60 * 24 * 1,
#'                                   blend_source = blend_source,
#'                                   missing_values = "impute")
#'}
#'@return A list that includes the new dataset id in case you create one, success/error message when you insert observations or the list of observations requested.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
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
#'@param oblender Omit this parameter.
#'@examples
#'\dontrun{
#'results <- openblender::searchTimeBlends(token="YOUR_TOKEN",
#'                                         anchor_ts=df_anchor$timestamp,
#'                                         search_text="coronavirus")
#'}
#'@return A list of datasets with ids, features, and more info.
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
