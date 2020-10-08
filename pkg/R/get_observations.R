get_observations <- function(json_parametros, url) {
  action <- "API_getSampleObservationsFromDataset"
  json_parametros$action <- "getObservationsFromDataset"
  initialized_task <- initializeTask(json_parametros, url)
  if (initialized_task$confirm == "y") {
    message("Task confirmed. Starting download...")
    json_parametros$consumption_id <- initialized_task$consumption_id
    json_parametros$r_version <- packageVersion("openblender")
    return(genericDownloadCall(json_parametros, url, action, 25, 600))
  } else {
    message("\nTask cancelled. To execute tasks without prompt set 'consumption_confirmation' to 0.")
    return(list(status="cancelled"))
  }
}

getDataWithVectorizer <- function(json_parametros, url) {
  action <- "API_getSampleObservationsWithVectorizerPlus"
  json_parametros$action <- "getDataWithTextVectorizer"
  initialized_task <- initializeTask(json_parametros, url)
  if (initialized_task$confirm == "y") {
    message("Task confirmed. Starting download...")
    json_parametros$consumption_id <- initialized_task$consumption_id
    json_parametros$r_version <- packageVersion("openblender")
    return(genericDownloadCall(json_parametros, url, action, 5, 300))
  } else {
    message("\nTask cancelled. To execute tasks without prompt set 'consumption_confirmation' to 0.")
    return(list(status="cancelled"))
  }
}

getOpenTextData <- function(json_parametros, url) {
  action <- "API_getOpenTextData"
  json_parametros$action <- "getOpenTextData"
  initialized_task <- initializeTask(json_parametros, url)
  if (initialized_task$confirm == "y") {
    message("Task confirmed. Starting download...")
    json_parametros$consumption_id <- initialized_task$consumption_id
    json_parametros$r_version <- packageVersion("openblender")
    return(genericDownloadCall(json_parametros, url, action, 25, 500))
  } else {
    message("\nTask cancelled. To execute tasks without prompt set 'consumption_confirmation' to 0.")
    return(list(status="cancelled"))
  }
}

initializeTask <- function(json_parametros, url) {
  data <- list(action = "API_initializeTask", json = json_parametros)
  details_task <- dameRespuestaLlamado(url, data)
  consumption_id <- details_task$consumption_id
  message(paste("Task ID: ", consumption_id))
  message(paste("Total estimated consumption: ", round(details_task$details$total_consumption, 2)), " processing units.")
  if ("consumption_confirmation" %in% attributes(json_parametros)$names) {
    consumption_confirmation <- json_parametros$consumption_confirmation
  } else {
    consumption_confirmation <- 'off'
  }
  if (consumption_confirmation == 'on') {
    confirm <- readline("Continue?  [y] yes \t [n] no \n")
  } else {
    confirm <- "y"
  }
  return(list(confirm = confirm, consumption_id = consumption_id))
}

clean_dataframe <- function(df, rep=1) {
  if(rep == 1) {
    new_df <- df
  } else {
    new_df <- data.frame()
  }
  for (i in seq(1, nrow(df), by=1)) {
    for (column in colnames(df)) {
      if (typeof(df[i, column][[1]]) == "list") {
        new_df[i, column] <- c("")
      } else if (typeof(df[i, column][[1]]) == "character") {
        new_df[i, column] <- toJSON(as.vector(df[i, column][[1]]))
      } else {
        new_df[i, column] <- df[i, column][[1]]
      }
    }
  }
  return(new_df)
}


#'@title Request to the API, depending on the action provided
#'@description Prepare the data to send it 'OpenBlender' API. This function is not used by users.
#'@param json_parametros Request parameters
#'@param url Url selected
#'@return List of observations obtained with \link{dameRespuestaLlamado}.
#'@keywords internal
genericDownloadCall <- function(json_parametros, url, action, n_test_observations, slice_mult) {
  respuesta = ""
  nom_archivo = paste(as.numeric(as.POSIXct(Sys.time())), ".csv", sep = "")
  start <- Sys.time()
  if ("test_call" %in% attributes(json_parametros)$names && (json_parametros$test_call == 1 || json_parametros$test_call == "on")) {
    test_call <- 1
  } else {
    test_call <- FALSE
  }
  if (test_call == 1) {
    cat("\n")
    message("This is a TEST CALL, set \"test_call\"=\"off\" or remove to execute service.")
    cat("\n")
    data <- list(action = action, json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    df_resp <- respuesta$sample
    t_universo <- 0
  } else {
    json_parametros$tamano_bin <- n_test_observations
    json_parametros$skip <- 0
    message("Downloading...")
    data <- list(action = action, json = json_parametros)
    respuesta <- dameRespuestaLlamado(url, data)
    t_universo <- respuesta$universe_size
    stop <- Sys.time()
    segundos <- ceiling(stop - start)
    tam_pedazo <- as.integer(round(slice_mult / as.integer(segundos), digits = 0))
    if(tam_pedazo < t_universo) {
      nums_pedazos <- ceiling(t_universo / tam_pedazo)
      if (nums_pedazos <= 0) {
        nums_pedazos <- 1
      }
      df_resp <- NULL
      secuencia <- seq(0, nums_pedazos - 1, by = 1)
      for (i in secuencia) {
        tryCatch({
          json_parametros$tamano_bin <- tam_pedazo
          json_parametros$skip <- tam_pedazo * i
          data <- list(action = action, json = json_parametros)
          respuesta <- dameRespuestaLlamado(url, data)
          df <- as.data.frame(respuesta$sample)
          writeAppendInFile(df, nom_archivo, action)
          if (is.null(df_resp)) {
            message(paste("Observations are being saved in", nom_archivo))
            df_resp <- df
          } else {
            df_resp <- rbind(df_resp, df)
          }
          avance <- round(((i) / nums_pedazos) * 100, digits = 2)
          message(paste(avance, "%"))
        },
        error = function(e){
          message("Some observations could not be processed.", e)
        })
      }
      if(nrow(df_resp) >= 100 || nrow(df_resp) == t_universo) {
        message("100% completed.")
        message("Wrapping up, this may take a few minutes...")
      }
      if ("sample_size" %in% attributes(json_parametros)$names) {
        if (as.integer(json_parametros$sample_size) < nrow(df_resp)) {
          df_resp <- df_resp[-sample(nrow(df_resp), (nrow(df_resp) - as.integer(json_parametros$sample_size))), ]
        }
      }
    } else {
      df_resp <- respuesta$sample
    }
  }
  if(action != "API_getOpenTextData"){
    df_resp <- clean_dataframe(df_resp)
    df_resp <- clean_dataframe(df_resp, rep=2)
  }
  if("timestamp" %in% attributes(df_resp)) {
    df_resp <- df_resp[order(-as.integer(df_resp$timestamp)), ]
  }
  respuesta <- list(universe_size = t_universo, sample = df_resp)
  return(respuesta)
}
