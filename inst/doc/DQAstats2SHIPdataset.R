## ----include = FALSE----------------------------------------------------------
# nolint start
library(magrittr)


## -----------------------------------------------------------------------------
# the next lines are commentend, since we avoided to have 'dataquieR' as yet
# another dependency to our package; however, if you have installed it, you
# can uncomment these lines to load the required files from the dataquieR-package

#list.files(system.file("extdata", package = "dataquieR"))

# load the ship dataset
# ship_data <- readRDS(
#   system.file("extdata/ship.RDS", package = "dataquieR")
# ) %>%
#   data.table::data.table()
ship_url <-
  "https://dataquality.qihs.uni-greifswald.de/extdata/ship.RDS"
tmpf <- tempfile()
download.file(ship_url, tmpf, mode = "wb")
ship_data <- readRDS(tmpf) %>%
  data.table::data.table()

# export the dataset to a CSV-file (one of the formats supported by DQAstats)
ship_data_export_fn <- "ship_data.csv"
data.table::fwrite(
  x = ship_data,
  file = file.path(tempdir(), ship_data_export_fn)
)


## -----------------------------------------------------------------------------
# load the ship metadata
# ship_meta <- readRDS(
#   system.file("extdata/ship_meta.RDS", package = "dataquieR")
# ) %>%
#   data.table::data.table()
ship_meta_url <-
  "https://dataquality.qihs.uni-greifswald.de/extdata/ship_meta.RDS"
tmpf <- tempfile()
download.file(ship_meta_url, tmpf, mode = "wb")
ship_meta <- readRDS(tmpf) %>%
  data.table::data.table()


## ----results='asis'-----------------------------------------------------------
ship_meta %>%
  DT::datatable(options = list(
    scrollX = TRUE,
    pageLength = 4
  ))


## -----------------------------------------------------------------------------
mdr <- data.table::fread(
  file = system.file(
    "demo_data/utilities/MDR/mdr_example_data.csv",
    package = "DQAstats"
  ),
  header = TRUE,
  nrows = 0,
  colClasses = "character"
)
dim(mdr)
colnames(mdr)


## ----warning=FALSE------------------------------------------------------------
# get names of dataelements from ship dataset
ship_var_names <- intersect(colnames(ship_data), ship_meta[, get("VAR_NAMES")])

# loop over dataelements
for (var in ship_var_names) {
  # get variable type from ship metadata
  data_type <- ship_meta[get("VAR_NAMES") == var, get("DATA_TYPE")]
  # get variable description from ship metadata
  labels <- ship_meta[get("VAR_NAMES") == var, get("VALUE_LABELS")]
  split_list <- strsplit(
    x = labels,
    split = " | ",
    fixed = TRUE
  )
  # convert variable description into human readable format
  if (length(split_list[[1]]) == 1 && is.na(split_list[[1]])) {
    descr <- paste0("Description for dataelement '", var, "'")
  } else {
    descr <- paste0(
      "Description for dataelement '", var,
      "': \n\n", paste0(unlist(split_list), collapse = "; ")
    )
  }
  
  # for this dataelement, fill row of DQAstats-MDR with extracted information
  mdr <- data.table::rbindlist(
    l = list(
      mdr,
      data.table::as.data.table(
        x = cbind(
          "designation" = var,
          "definition" = descr,
          "source_variable_name" = var,
          "variable_name" = var,
          "key" = var,
          "source_table_name" = ship_data_export_fn,
          "source_system_type" = "csv", # indicates that datasets format is CSV
          "source_system_name" = "ship", # arbitrary name for this dataset
          "dqa_assessment" = 1, # default value for dataelements to be analyzed
          "variable_type" = data_type
        )
      )
    ),
    fill = TRUE
  )
}


## -----------------------------------------------------------------------------
# save all categorical variablees in a vector
cat_vars <- c(
  "id", "sex", "obs_bp", "dev_bp", "obs_soma", "dev_length", "dev_weight",
  "obs_int", "school", "family", "smoking", "stroke", "myocard",
  "diab_known", "contraception", "income"
)

# change variable type from integer to "enumerated" (to get meaningful
# results from DQAstats)
mdr[get("designation") %in% cat_vars, ("variable_type") := "enumerated"]


## ----warning=FALSE------------------------------------------------------------
# loop over categorical variables
for (var in cat_vars) {
  # get definitions of category-levels
  labels <- ship_meta[get("VAR_NAMES") == var, get("VALUE_LABELS")]
  split_list <- strsplit(
    x = labels,
    split = " | ",
    fixed = TRUE
  )
  
  # extract the allowed values for each categorical dataelement
  allowed_values <- strsplit(
    x = split_list[[1]],
    split = " =",
    fixed = TRUE
  )
  
  # transform allowed values into a "value_set" array, which is stored
  # as a JSON object (required by DQAstats)
  c <- jsonlite::toJSON(
    list(
      "value_set" = sapply(
          X = allowed_values,
          FUN = function(x) {
            return(x[[1]])
          })
    ),
    pretty = TRUE
  )
  # add constraints for this dataelement to the DQAstats MDR
  mdr[get("source_variable_name") == var, ("constraints") := c]
}


## -----------------------------------------------------------------------------
# for comparison, show metadata definition from dataquieR ...
# dataquieR: string, split by pipes '|'
ship_meta[get("VAR_NAMES") == "smoking", get("VALUE_LABELS")]


## -----------------------------------------------------------------------------
# ... and DQAstats: JSON
mdr[get("source_variable_name") == "smoking", get("constraints")]
mdr[get("source_variable_name") == "smoking", get("constraints")] %>%
  jsonlite::fromJSON()


## -----------------------------------------------------------------------------
# get names of continuous variables (float/integer)
cont_vars <- mdr[
  get("variable_type") %in% c("integer", "float"),
  unique(get("source_variable_name"))
]

# loop over continuous dataelements
for (var in cont_vars) {
  # extract constraints of continuous dataelement
  labels <- ship_meta[get("VAR_NAMES") == var, get("HARD_LIMITS")]
  split_list <- strsplit(
    x = labels,
    split = ";",
    fixed = TRUE
  )

  # transform these constraints to a list, with entries "min" and "max"
  constraints <- lapply(
    X = split_list,
    FUN = function(x) {

      if (!is.na(x[[1]])) {
        return(
          list(
            "min" = as.numeric(gsub("[[:punct:]]", "", x[[1]])),
            "max" = as.numeric(gsub("[[:punct:]]", "", x[[2]]))
          )
        )
      } else {
        return("error")
      }
    }
  )

  # save list as nested JSON within the key "range" (required by DQAstats)
  if (length(constraints[[1]]) > 1) {
    c <- jsonlite::toJSON(
      list("range" = constraints[[1]]),
      pretty = TRUE,
      auto_unbox = TRUE
    )
    # write constraint for this dataelement to DQAstats MDR
    mdr[get("source_variable_name") == var, ("constraints") := c]
  }
}


## -----------------------------------------------------------------------------
# The `id` variable should actually not be treated as a categorical variable,
# since the number of potential categories is (theoretically) unlimited and
# an analysis of the categories in this case is seldom meaningful.
# Nevertheless, we could perform quality checks on the `id` variable, e.g. to
# check for a required formatting of the values.
# This can be achieved with DQAstats by changing the variable type to "string"
# and use a regular expression that checks for the expected format.

# change variable type to "string"
mdr[get("source_variable_name") == "id", ("variable_type") := "string"]

# add regular expression for the dataelement "id"
mdr[get("source_variable_name") == "id", ("constraints") := jsonlite::toJSON(
  list("regex" = "^[[:digit:]]{1,5}$"),
  pretty = TRUE,
  auto_unbox = TRUE
)]


## -----------------------------------------------------------------------------
# add plausibilities
# sex and contraception
p <- jsonlite::toJSON(list(
  "atemporal" = list(
    "contraception" = list(
      "name" = "A_present_and_B_vv",
      "description" = "Contracept in males.",
      "join_crit" = "id",
      "filter" = list(
        "ship" = "^1$"
      ),
      "constraints" = list(
        "value_set" = list(
          "ship" = "1"
        )
      )
    )
  )
),
  pretty = TRUE,
  auto_unbox = TRUE
)

# write plausibility to mdr
mdr[
  source_variable_name == "sex" &
    dqa_assessment == 1,
  plausibility_relation := p
]


## -----------------------------------------------------------------------------
p <- jsonlite::toJSON(list(
  "atemporal" = list(
    "diab_age" = list(
      "name" = "A_present_and_B_levels_vl",
      "description" = "Diab age but no diab.",
      "join_crit" = "id",
      "filter" = list(
        "ship" = "^(?!.*(99900|99901|99801)).*$"
      ),
      "constraints" = list(
        "value_set" = list(
          "ship" = "1"
        )
      )
    )
  )),
  pretty = TRUE,
  auto_unbox = TRUE
)
mdr[
  source_variable_name == "diab_known" &
    dqa_assessment == 1,
  plausibility_relation := p
]


## -----------------------------------------------------------------------------
p <- jsonlite::toJSON(list(
  "uniqueness" = list(
    "sex" = list(
      "name" =  "Every ID is associated with one Sex",
      "description" = paste0("With each distinct value of 'id', ",
      "only one value of 'sex' may be associated."
    )
  ))),
  pretty = TRUE,
  auto_unbox = TRUE
)
mdr[
  source_variable_name == "id" &
    dqa_assessment == 1,
  plausibility_relation := p
]


## ----results='asis'-----------------------------------------------------------
mdr %>%
  DT::datatable(options = list(
    scrollX = TRUE,
    pageLength = 4
  ))


## ----message=FALSE------------------------------------------------------------
utils_path <- file.path(tempdir(), "utilities")
dir.create(file.path(utils_path, "MDR"), recursive = TRUE)

data.table::fwrite(
  x = mdr,
  file = file.path(utils_path, "MDR/mdr.csv")
)


## ----results='hide'-----------------------------------------------------------
file.copy(
  from = system.file("demo_data/utilities/RMD", package = "DQAstats"),
  to = utils_path,
  overwrite = TRUE,
  recursive = TRUE
)


## ----eval=FALSE, warning=FALSE, message=FALSE---------------------------------
# # ship data set
# source_system_name <- "ship"
# target_system_name <- source_system_name
# 
# mdr_filename <- "mdr.csv"
# output_dir <- file.path(tempdir(), "output")
# logfile_dir = tempdir()
# 
# # does only work, if "ship_data.csv" is lying next to this RMD-file
# Sys.setenv("SHIP_PATH" = tempdir())
# 
# # provide all arguments to main function
# all_results <- DQAstats::dqa(
#   source_system_name = source_system_name,
#   target_system_name = target_system_name,
#   utils_path = utils_path,
#   mdr_filename = mdr_filename,
#   output_dir = output_dir,
#   logfile_dir = logfile_dir
# )


## ----eval=FALSE---------------------------------------------------------------
# library(DQAgui)
# # set basepath for file-browser in GUI-config
# Sys.setenv("CSV_SOURCE_BASEPATH" = tempdir())
# 
# ## launch GUI
# DQAgui::launch_app(
#   utils_path = utils_path,
#   mdr_filename = "mdr.csv",
#   parallel = FALSE
# )
# # nolint end

