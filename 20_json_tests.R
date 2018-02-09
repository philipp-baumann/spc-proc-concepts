################################################################################
## Author: Philipp Baumann
## License: GPL-3.0
## Project: Test ideas for extending simplerspec with database and API
## Description: Test reading, converting, and writing simplerspec spectral
##   tibbes to JSON files; test possible JSON approaches for a spectral database
##   API
################################################################################

# Load simplerspec package for spectral model development wrapper functions
library(simplerspec)
# Load tidyverse package: loads packages frequently used for data manipulation,
# data tidying, import, and plotting
library(tidyverse)

## Register parallel backend for using multiple cores ==========================

# Allows to tune the models using parallel processing (e.g. use all available
# cores of a CPU); caret package automatically detects the registered backend
library(doParallel)
# Make a cluster with all possible threads (more than physical cores)
cl <- makeCluster(detectCores())
# Register backend
registerDoParallel(cl)
# Return number of parallel workers
getDoParWorkers() # 8 threads on MacBook Pro (Retina, 15-inch, Mid 2015);
# Quadcore processor


################################################################################
## Part 1: Read and pre-process spectra, Read chemical data, and join
## spectral and chemical data sets
################################################################################

## Read spectra in list ========================================================

# List of OPUS files of NABO quality control spectral samples measured
# on gold background
lf <- dir("data/spectra/soilspec_yamsys_ref", full.names = TRUE)

# Read OPUS files into list
spc_l <- read_opus_univ(
  fnames = lf,
  extract = c("spc", "ScSm", "ScRf"), 
  parallel = TRUE)

## Spectral data processing pipe ===============================================

spc_tbl <- spc_l %>%
  gather_spc() %>% 
  resample_spc(wn_lower = 500, wn_upper = 3996, wn_interval = 2) %>%
  average_spc() %>%
  preprocess_spc(select = "sg_1_w21")


################################################################################
## Test handling json in R
################################################################################

library("jsonlite")
json <- toJSON(mtcars, pretty = TRUE)
cat(json)

tbl_json <- toJSON(spc_tbl[1:5, ], pretty = TRUE)
cat(tbl_json)
write(tbl_json, file = "out/tbl_json.json")

# Use jsonlite to read JSON
microbenchmark::microbenchmark(
  jsonlite::fromJSON(txt = read_file(file = "out/tbl_json.json")),
  rjson::fromJSON(file = "out/tbl_json.json"),
  times = 10
)

json_toR <- fromJSON(txt = read_file(file = "out/tbl_json.json"))
# json_toR <- rjson::fromJSON(file = "out/tbl_json.json")

# Spectra in <spc> list-column do not contain column names (because they 
# are matrices)
is(spc_tbl$spc[[1]]) # "matrix", "array" "structure", "vector"
is(spc_tbl$spc_rs[[1]])
# -> "data.table", "data.frame", "list", "oldClass"; "vector"


# Explore json in json viewer --------------------------------------------------

library(listviewer)
jsonedit(json_toR)

# Various tests ----------------------------------------------------------------

str(json_toR)
class(json_toR) # ! it is a nested data.frame!!!
json_toR[["wavenumbers"]]
is.data.frame(json_toR[["spc_pre"]][[1]])

library(tidyjson)
read_file(file = "out/tbl_json.json") %>%
  gather_array() %>%
  tidyjson::json_types()

read_file(file = "out/tbl_json.json") %>%
  gather_keys() %>%
  tidyjson::json_types()

read_file(file = "out/tbl_json.json") %>% 
  jsonlite::prettify()

read_file(file = "out/tbl_json.json") %>% 
  gather_keys()

# Test fst package
# "Lightning Fast Serialization of Data Frames for R"
fst::write.fst(x = mtcars, "tbl.fst")

# Nested data frames: ?fromJSON @Examples --------------------------------------
data2 <- fromJSON("https://api.github.com/users/hadley/repos")
class(data2)
names(data2)
str(data2$owner)
names(data2$owner)
class(data2$owner)
flatten(data2)
data2$owner$login