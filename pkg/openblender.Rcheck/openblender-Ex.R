pkgname <- "openblender"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "openblender-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('openblender')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("call")
### * call

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: call
### Title: Make HTTP request to openblender.io services
### Aliases: call

### ** Examples

## Not run: 
##D #CREATE DATASET
##D df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
##D action <- "API_createDataset"
##D parameters <- list(
##D token = "YOUR TOKEN",
##D id_user = "YOUR USER ID",
##D name = "Assign a name",
##D descriptipon = "Set a description",
##D visibility = "public",
##D tags = list("topic", "tag"),
##D insert_observations = "off",# set "on" if you want include the observations
##D dataframe = df
##D )
##D call(action, parameters)
##D #INSERT OBSERVATIONS
##D df <- read.csv(file = "/path/to/your/data.csv", header = TRUE, sep = ",")
##D action <- "API_insertObservations"
##D parameters <- list(
##D token = "YOUR TOKEN",
##D id_user = "YOUR USER ID",
##D id_dataset = "DATASET ID",
##D notification = "on",
##D observations = df
##D )
##D call(action, parameters)
##D 
##D #GET OBSERVATIONS
##D action <- "API_getObservationsFromDataset"
##D parameters <- list(
##D token = "YOUR TOKEN",
##D id_user = "YOUR USER ID",
##D id_dataset = "DATASET ID"
##D )
##D call(action, parameters)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("call", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
