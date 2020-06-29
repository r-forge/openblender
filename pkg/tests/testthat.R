# # TESTING #
# ############################################################
# ######################Create Dataset########################
# df <- read.csv(file = "climas2018.csv", header = TRUE, sep = ",")
# df <- as.data.frame(list("FEATURE_1" = c("VALUE_1", "VALUE_2"), "FEATURE_2" = c("VALUE_3", "VALUE_4")))
# action <- "API_createDataset"
# parameters <- list(
#   token = "1AKnDhP09wa2yGRia5z1SGTxAUQAiV",
#   id_user = "5d07bed33e59b27a93e43be8",
#   name = "df_from_R_object",
#   descriptipon = "testing R library for OpenBlender",
#   visibility = "public",
#   tags = list("tag1", "tag2"),
#   insert_observations = "off",
#   dataframe = df
# )
# new_df <- openblender::call(action, parameters)
# new_df$id_dataset
# ############################################################
# #####################Insert observations####################
# df <- read.csv(file = "climas2018.csv", header = TRUE, sep = ",")
# df
# action <- "API_insertObservations"
# parameters <- list(
#   #oblender = 1,
#   token = "1AKnDhP09wa2yGRia5z1SGTxAUQAiV",
#   id_user = "5d07bed33e59b27a93e43be8",
#   id_dataset = new_df$id_dataset,
#   notification = "on",
#   observations = df
# )
# response <- openblender::call(action, parameters)
# response



# ############################################################
# #####################Obtain observations####################
# ANCHOR: 'Milenio entertainment headlines'
# action <- "API_getObservationsFromDataset"
# parameters <- list(
#   token="5d07bed33e59b27a93e43be804ij5qcy5ss9jHsoBpwpxsvLZqyrcU",
#   id_user="5d07bed33e59b27a93e43be8",
#   id_dataset="5dc48aaf9516290ee03eb91e",
#   consumption_confirmation='on'
# )
#
# df <- openblender::call(action, parameters)$sample
# head(df)

# #prueba para obtener un text vectorizer
# action <- "API_getDataWithVectorizer"
# parameters <- list(
#   oblender=1,
#   token="5d48b439275b3f05db0feee2PfZabUszloyThHiTpqqzwmOXDkzyJ9",
#   id_user="5d48b439275b3f05db0feee2",
#   id_textVectorizer="5e4ac5b6a239d58424368f72"
# )
# df <- openblender::call(action, parameters)$sample
# head(df)
#
#
# # prueba pull data con blend y text vectorizer

# action <- "API_getObservationsFromDataset"
#
# # ANCHOR: 'Time Entertainment headlines'
#
#
# parameters <- list(
#   token="5d07bed33e59b27a93e43be804ij5qcy5ss9jHsoBpwpxsvLZqyrcU",
#   id_user="5d07bed33e59b27a93e43be8",
#   id_dataset="5d88492e9516294231c59595"
# )
#
#
# df <- openblender::call(action, parameters)$sample
# head(df)

# probar get open text data
# action <- "API_getOpenTextData"
# parameters <- list(
#   token="5ca62dafe213e54e846509eaUfctPkE1Imah1QNaoNYCp9fbZKjzOW",
#   date_filter = list(start_date="2020-01-01", end_date="2020-03-10"),
#   text_filter_search = list("covid","coronavirus"),
#   aggregate_in_time_interval = list(time_interval_size = 60 * 60 * 24),
#   sources = list(list(id_dataset = '5e2ef74e9516294390e810a9', features = list('text')))
# )
# df <- openblender::call(action, parameters)$sample
# head(df)


#Load OpenBlender package
# library(openblender)
#
#
# action <- "API_getObservationsFromDataset"
#
# # ANCHOR: 'Apple Inc. Price'
#
#
# parameters <- list(
#   token="5d07bed33e59b27a93e43be8bZSHRaIfbxbyUEuviAsfPjweLj8mKz",
#   id_user="5d07bed33e59b27a93e43be8",
#   id_dataset="5d4c39d09516290b01c8307b",
#   consumption_confirmation = "on"
# )
#
#
# df <- openblender::call(action, parameters)$sample
# head(df)

