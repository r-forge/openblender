#'@title Location Blend.
#'@description Get Dataframe with DataToBlend ordered time by latitude, sending two lists or Series of Anchor latitude and longitude. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param token Obtained in \href{https://www.openblender.io/#/user/account?section=account}{openblender.io}
#'@param anchor_lat Array of ‘latitude’ values to blend data by.
#'@param anchor_lon Array of ‘longitude’ values to blend data by.
#'@param blend_source JSON object of Data To Blend configuration obtained from a data source in OpenBlender.io
#'@param blend_type (Only if blend_type = 'agg_in_intervals'). Select chronological direction of observation or interval to aggregate from 'blend source' to 'anchor':’n_closest_agg’: Aggregate closest n. ‘in_radio_agg’: Aggregate in radio r (measured in km). Default: 'in_radio_agg'.
#'@param n (Only if ‘n_closest_agg’ is selected) Number of closest observations to blend and aggregate.
#'@param r (Only if ‘in_radio_agg’ is selected) Radio in kilometers to blend and aggregate data points.
#'@param agg_output 'sum', 'avg': For numerical features. Default: 'count'
#'@param consumption_confirmation "on" or "off", default "on"
#'@param data_format Default "Dataframe"
#'@param oblender Omit this parameter
#'@examples
#'\dontrun{
#'#Blend a dataset
#'#You provide df_anchor data
#'  blend_source <- list(
#'  id_dataset = "5d4ca3089516290b02feec4f",
#'  feature = "category"
#'}
#'
#'df_blend <- openblender::locationBlend(token = "YOUR_TOKEN",
#'                                       blend_source = blend_source,
#'                                       anchor_lat = df_anchor.latitude,
#'                                       anchor_lon = df_anchor.longitude,
#'                                       blend_type = "in_radio_agg", #  agg_in_intervals
#'                                       agg_output = "count",
#'                                       r = 3)
#'}
#'@return A Blended dataset intersected with your anchor in location, success/error message when you insert observations or the list of observations requested.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
locationBlend <- function(token, anchor_lat, anchor_lon, blend_source,
                      blend_type = "closest_observation",
                      agg_output = "count",
                      n = 3,
                      r = 1000,
                      data_format = 'dataframe',
                      consumption_confirmation = "off",
                      oblender = FALSE){
  tryCatch({
    url <- getUrl(list(), oblender = oblender)
    if (typeof(anchor_lat) == "list") {
      if ("latitude" %in% attributes(anchor_lat)$names) {
        anchor_lat <- unname(anchor_lat["latitude"])[[1]]
      }
    }
    if (typeof(anchor_lon) == "list") {
      if ("longitude" %in% attributes(anchor_lon)$names) {
        anchor_lon <- unname(anchor_lon["longitude"])[[1]]
      }
    }
    anchor_lat <- sort(anchor_lat)
    anchor_lon <- sort(anchor_lon)
    if (length(anchor_lat) != length(anchor_lon)) {
      message(paste("ERROR: Size of 'anchor_lat' (", str(length(anchor_lat)), ") and 'anchor_lon' (", str(length(anchor_lon)), ") must be the same."))
    }
    tryCatch({
      json_parametros <- list(
        token=token,
        anchor_rectangle=list(
          top= max(anchor_lat),
          bottom=min(anchor_lat),
          right=max(anchor_lon),
          left=min(anchor_lon))
      )
    },
    error = function(e) {
      message('ERROR: All values of "anchor_lat" and "anchor_lon" must be numerical.')
      return(FALSE)
    })
    json_parametros <- list(
      token=token,
      anchor_lat=anchor_lat,
      anchor_lon=anchor_lon,
      blend_source=blend_source,
      blend_type=blend_type,
      agg_output=agg_output,
      n=n,
      r=r)
    task_params <- list(
      token=token,
      number_of_rows=length(anchor_lat),
      blend_source=blend_source,
      consumption_confirmation=consumption_confirmation)
    task <- initializeTask(task_params, url)
    tam_ini <- 500
    if (task$confirm == "y") {
      message("Task confirmed. Starting download...")
      df_resp <- NULL
      resp_vacio <- TRUE
      universe_size <- length(anchor_lat)
      if (length(anchor_lat) <= tam_ini) {
        piece_size <- length(anchor_lat)
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
        json_parametros$anchor_lat <- anchor_lat[(i_act + 1):(i_act + piece_size)]
        json_parametros$anchor_lon <- anchor_lon[(i_act + 1):(i_act + piece_size)]
        data <- list(action="API2_getLocationBlend", json=json_parametros)
        respuesta <- dameRespuestaLlamado(url, data)
        if (typeof(respuesta) == "list" && respuesta$status == "success") {
          message(paste(progreso * 100, "%"))
          if (resp_vacio) {
            df_resp <- clean_dataframe(respuesta$df_resp, rep = 2)
            df_resp <- df_resp[order(df_resp$latitude), ]
            row.names(df_resp) <- NULL
            resp_vacio <- FALSE
          } else {
            df_piece <- clean_dataframe(respuesta$df_resp, rep = 2)
            df_resp <- rbind(df_resp, df_piece)
            df_resp <- df_resp[order(df_resp$latitude), ]
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


#'@title Search Location Blends.
#'@description Search Blends that intersect your anchor in location providing a string. Consult documentation \href{https://www.openblender.io/#/api_documentation}{here}.
#'@param token Obtained in \href{https://www.openblender.io/#/user/account?section=account}{openblender.io}
#'@param anchor_lat Latitude array of your anchor.
#'@param anchor_lon Longitude array of your anchor.
#'@param search_text Text provided to find Time Blends.
#'@param oblender Omit this parameter.
#'@examples
#'\dontrun{
#'results <- openblender::searchTimeBlends(token="YOUR_TOKEN",
#'                                         anchor_lat=df_anchor$latitude,
#'                                         anchor_lon=df_anchor$longitude,
#'                                         search_text="coronavirus")
#'}
#'@return A list of datasets with ids, features, and more info.
#'@seealso To see more details go to \href{http://openblender.io}{openblender.io}
searchLocationBlends <- function(token, anchor_lat, anchor_lon, search_text, oblender=FALSE) {
  tryCatch({
    url <- getUrl(list(), oblender = oblender)
    if (length(anchor_lat) != length(anchor_lon)) {
      message(paste("ERROR: Size of 'anchor_lat' (", str(length(anchor_lat)), ") and 'anchor_lon' (", str(length(anchor_lon)), ") must be the same."))
    }
    tryCatch({
      json_parametros <- list(
        token=token,
        anchor_rectangle=list(
          top= max(anchor_lat),
          bottom=min(anchor_lat),
          right=max(anchor_lon),
          left=min(anchor_lon)),
        search_text=search_text
      )
    },
    error = function(e) {
      message('ERROR: All values of "anchor_lat" and "anchor_lon" must be numerical.')
      return(FALSE)
    })
    json_parametros$r_version <- packageVersion("openblender")
    data <- list(action = "API2_searchLocationBlends", json = json_parametros)
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
