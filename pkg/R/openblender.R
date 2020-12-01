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
