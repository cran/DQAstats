# DQAstats - Perform data quality assessment (DQA) of electronic health
# records (EHR)
# Copyright (C) 2019-2024 Universitätsklinikum Erlangen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(data.table)

test_that("correct functioning of time_compare", {

  utils_path <- system.file(
    "demo_data/utilities/",
    package = "DQAstats"
  )
  mdr_filename <- "mdr_example_data.csv"
  rv <- list()
  rv$mdr <- read_mdr(
    utils_path = utils_path,
    mdr_filename = mdr_filename
  )

  source_system_name <- "exampleCSV_source"
  target_system_name <- "exampleCSV_target"

  rv <- c(rv, create_helper_vars(
    mdr = rv$mdr,
    source_db = source_system_name,
    target_db = target_system_name
  ))
  # save source/target vars
  rv$source$system_name <- source_system_name
  rv$target$system_name <- target_system_name
  rv$source$system_type <- "csv"
  rv$target$system_type <- "csv"

  rv$log$logfile_dir <- tempdir()

  # set headless (without GUI, progressbars, etc.)
  rv$headless <- TRUE

  # set configs
  demo_files <- system.file("demo_data", package = "DQAstats")
  Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
  Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)

  # get configs
  rv$source$settings <- DIZutils::get_config_env(
    system_name = rv$source$system_name,
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )
  rv$target$settings <- DIZutils::get_config_env(
    system_name = tolower(rv$target$system_name),
    logfile_dir = rv$log$logfile_dir,
    headless = rv$headless
  )

  # set start_time (e.g. when clicking the 'Load Data'-button in shiny
  rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")

  # define restricting date
  rv$restricting_date$use_it <- FALSE

  # load source data
  tempdat <- data_loading(
    rv = rv,
    system = rv$source,
    keys_to_test = rv$keys_source
  )
  rv$data_source <- tempdat$outdata

  # load target data
  tempdat <- data_loading(
    rv = rv,
    system = rv$target,
    keys_to_test = rv$keys_target
  )
  rv$data_target <- tempdat$outdata

  rv$time_compare_results <- time_compare(rv = rv,
                                        logfile_dir = rv$log$logfile_dir,
                                        headless = rv$headless)

  expect_type(rv$time_compare_results, "list")
  expect_length(rv$time_compare_results, n = 1)
  expect_length(rv$time_compare_results[[1]], n = 3)


  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})
