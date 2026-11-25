# Script: 3_recoding_data.R
# Created by: R. Noah Padgett & Chris Felton
# Last edited on: 2026-06-05

# WARNING: The package was set up to be as user-friendly as possible for researchers
#	part of the GFS core team who mainly have experience with other statistical analysis
#	software such as STATA, SAS, and SPSS.
#	This package and implementation of the analyses for Wave 3 of the Global Flourishing
#	Study does NOT conform to "tidy" principles in general. While some elements of tidy
#   evaluation and syntax structure are used throughout, we did not implement everything with
#   "tidyness" in mind. As such, we make no guarantees that the package will integrate or
#   "play nice" with other packages.

## ============================================================================================== ##
## ============================================================================================== ##
# ---- Part 1. R environment preparation ----

library(Rglobalflourishing)
load_packages()
# global options
options(
  survey.lonely.psu = "certainty",
  future.plan = "multisession"
)
handlers(global = TRUE)
num_cores <- availableCores(constraints = "connections") - 1

## ============================================================================================== ##

## ============================================================================================== ##
## ============================================================================================== ##
# ---- Part 3. Recode the imputed data for easily use in analyses ----
# ~~
RECODE.DEFAULTS <- list(
  FOCAL_VARIABLE = "TIME_MEDIA_MY",
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
    get_variable_codes("GENDER.RACE", appnd=""),
    get_variable_codes("RETROSPECTIVE.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y2"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y3")
  ),
  VARIABLES.VEC = c(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2"), get_variable_codes("VARS.Y3")),
  VALUES_DEFINING_UPPER_CATEGORY = list(c(5)),
  VALUES_DEFINING_LOWER_CATEGORY = list(c(1:4)),
  FORCE_BINARY = TRUE,
  FORCE_CONTINUOUS = FALSE,
  USE_DEFAULT = TRUE
)

recode.fx <- function(data){
  data |>
    mutate(
      RAW_TIME_MEDIA_MY = TIME_MEDIA_MY,
      BIN_TIME_MEDIA_MY = ifelse(TIME_MEDIA_MY == 5, 1, 0)
    )
}


gc(full=TRUE)
recode_imp_by_country(
  data.dir =  data.dir,
  wave = 3 ,
  nimp = 20,
  list.default = RECODE.DEFAULTS,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  parallel = TRUE, num_cores = num_cores,
  wgt = "ANNUAL_WEIGHT_R3",
  inject.recode.fx = recode.fx
)
## ============================================================================================== ##
## ============================================================================================== ##
# ---- Part 4. CHECK VARIABLE CODING/COLLAPSING ---- #
#

# df.raw <- gfs_get_labelled_raw_data(
#   file = here::here(data.dir, dataset.name),
#   wave = 3,
#   list.composites = get_variable_codes('LIST.COMPOSITES')
# )
#
# # # the following checks the imputed data back with the raw data
# tmp.dat1 <- df.raw %>%
#   filter(COUNTRY == "United States") %>%
#   arrange(ID)
# tmp.dat2 <- readr::read_rds(here::here(data.dir, "recoded", "recoded_imputed_data_obj_United States_imp1.rds")) %>%
#   arrange(ID)
# dnn0 <- c("Raw Data", "Recoded Imputed Data (.imp==1)")
# for(i in 1:length(FOCAL_VARIABLE)){
#   print(FOCAL_VARIABLE[i])
#   print(table(tmp.dat1[[FOCAL_VARIABLE[i]]], tmp.dat2[[FOCAL_VARIABLE[i]]], dnn = dnn0, useNA = "ifany"))
# }
# remove(df.raw, tmp.dat2,tmp.dat1)
gc()

