#'@title HTTP request
#'@description Make HTTP requests to 'OpenBlender' API services. This function is not used by users.
#'@param url URL selected
#'@param data Request data
#'@return A list from 'OpenBlender' API (response).
#'@keywords internal
dameRespuestaLlamado <- function(url, data) {
  data$json <- toJSON(data$json, auto_unbox = TRUE, force = TRUE)
  resp <- POST(url = url, body = data, encode = "form")
  # message("RESPUESTA")
  # message(resp)
  if ("status" %in% attributes(content(resp))$names && content(resp)$status != "success") {
    if (content(resp)$status == "error") {
      message("------------------------------------------------")
      message(paste("Error OpenBlender API: ", content(resp)$response))
      message("------------------------------------------------")
    }
    return(FALSE)
  } else {
    cont <- content(resp)
    # if ("base64_zip" %in% attributes(cont)$names) {
    #   message(cont$base64_zip)
    #   raw_chars <- charToRaw(cont$base64zip)
    #   message(raw_chars)
    #   cont <- unserialize(base64_dec(raw_chars))
    #   message(cont)
    # }
    # message("CONTENIDO")
    # message(cont)
    if ("sample" %in% attributes(cont)$names) {
      sample <- toJSON(cont$sample, dataframe = "rows")
      sample <- fromJSON(sample)
      cont$sample <- sample
    }
    return(cont)
  }
}
#'@title Request to the API, depending on the action provided
#'@description Prepare the data to send it 'OpenBlender' API. This function is not used by users.
#'@param json_parametros Request parameters
#'@param url Url selected
#'@return A list obtained from \link{dameRespuestaLlamado}.
#'@keywords internal
power_model <- function(json_parametros, url) {
  action <- "API_powerModel"
  data <- list(action = action, json = json_parametros)
  respuesta <- dameRespuestaLlamado(url, data)
  return(respuesta)
}

#'@title Verify JSON
#'@description Check if a dataframe can be transformed into JSON with no errors. This function is not accessible for users.
#'@param df_json Dataframe to verify
#'@return Dataframe verified or an error message.
#'@keywords internal
comprobarJSONaDF <- function(df_json) {
  obj <- list(valido = TRUE, msj = "Success", df_nuevo = NULL)
  if ("dataframe" %in% attributes(df_json)$names) {
    ind <- "dataframe"
  } else {
    ind <- "observations"
  }
  tryCatch({
    obj$df_nuevo <- fromJSON(toJSON(df_json[[ind]]))
    if(length(obj$df_nuevo) == 0) {
      obj$valido <- FALSE
    }
  },
  error = function(e) {
    obj$df_nuevo <- NULL
    obj$valido <- FALSE
    obj$msj <- paste("Error transforming json: ", e)
    warning(obj$msj)
  })
  return(obj)
}

writeAppendInFile <- function(df, file_name, action) {
  tryCatch({
    cleaned <- df
    # clean data to write in file
    if(action != "API_getOpenTextData"){
      cleaned <- clean_dataframe(df)
      cleaned <- clean_dataframe(df, rep=2)
    }
    options(warn=-1)
    write.table(cleaned, file_name, sep = ",", col.names = !file.exists(file_name), row.names = F, append = T)
    options(warn=0)
  }, error = function(e) {
    cat("\n")
    message("Unable to save CSV locally, please save dataframe when download completes.")
  })
}
