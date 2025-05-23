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


get_plausis_from_mdr <- function(atemp_vars) {
  uniques <- list()
  for (i in atemp_vars[, get("variable_name")]) {
    uniques[[i]] <-
      jsonlite::fromJSON(
        atemp_vars[get("variable_name") == i, get(
          "plausibility_relation"
        )])[["atemporal"]]
  }
  return(uniques)
}


#' @title get_atemp_plausis helper function
#'
#' @description Internal function to generate raw data for the
#'   'Atemporal Plausibility' checks.
#'
#' @param atemp_vars A data.table object. The object is created
#'   by \code{create_helper_vars} from the data represented in the
#'   metadata repository.
#'
#' @inheritParams atemp_plausi_results
#' @inheritParams create_helper_vars
#'
#' @return A list with one entry for each atemporal plausibility check
#'   containing the raw results. Each entry contains the following (nested) list
#'   items:
#'   \describe{
#'   \item{source_data}{A nested list with the raw plausibility check results
#'   for the source data system.}
#'   \item{target_data}{A nested list with the raw plausibility check results
#'   for the target data system.}
#'   }
#'
#' @examples
#' utils_path <- system.file(
#'   "demo_data/utilities/",
#'   package = "DQAstats"
#' )
#' mdr_filename <- "mdr_example_data.csv"
#' rv <- list()
#' rv$mdr <- read_mdr(
#'   utils_path = utils_path,
#'   mdr_filename = mdr_filename
#' )
#'
#' source_system_name <- "exampleCSV_source"
#' target_system_name <- "exampleCSV_target"
#'
#' rv <- c(rv, create_helper_vars(
#'   mdr = rv$mdr,
#'   source_db = source_system_name,
#'   target_db = target_system_name
#' ))
#' # save source/target vars
#' rv$source$system_name <- source_system_name
#' rv$target$system_name <- target_system_name
#' rv$source$system_type <- "csv"
#' rv$target$system_type <- "csv"
#'
#' rv$log$logfile_dir <- tempdir()
#'
#' # set headless (without GUI, progressbars, etc.)
#' rv$headless <- TRUE
#'
#' # set configs
#' demo_files <- system.file("demo_data", package = "DQAstats")
#' Sys.setenv("EXAMPLECSV_SOURCE_PATH" = demo_files)
#' Sys.setenv("EXAMPLECSV_TARGET_PATH" = demo_files)
#'
#' # get configs
#' rv$source$settings <- DIZutils::get_config_env(
#'   system_name = rv$source$system_name,
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#' rv$target$settings <- DIZutils::get_config_env(
#'   system_name = tolower(rv$target$system_name),
#'   logfile_dir = rv$log$logfile_dir,
#'   headless = rv$headless
#' )
#'
#' # set start_time (e.g. when clicking the 'Load Data'-button in shiny
#' rv$start_time <- format(Sys.time(), usetz = TRUE, tz = "CET")
#'
#' # define restricting date
#' rv$restricting_date$use_it <- FALSE
#'
#' # load source data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$source,
#'   keys_to_test = rv$keys_source
#' )
#' rv$data_source <- tempdat$outdata
#'
#' # load target data
#' tempdat <- data_loading(
#'   rv = rv,
#'   system = rv$target,
#'   keys_to_test = rv$keys_target
#' )
#' rv$data_target <- tempdat$outdata
#'
#' get_atemp_plausis(
#'   rv = rv,
#'   atemp_vars = rv$pl$atemp_vars,
#'   mdr = rv$mdr,
#'   headless = rv$headless
#' )
#'
#' @export
#'
get_atemp_plausis <- function(rv,
                              atemp_vars,
                              mdr,
                              headless = FALSE) {

  #% atemp_vars = rv$pl$atemp_vars
  #% mdr = rv$mdr
  #% sourcesystem = "p21csv"
  #% headless = T

  outlist <- list()

  # get uniqueness checks from json
  uniques <- get_plausis_from_mdr(atemp_vars = atemp_vars)

  # iterate over uniqueness checks
  for (i in names(uniques)) {

    seq_names <- sapply(
      X = uniques[[i]],
      FUN = function(x) {
        x$name
      },
      simplify = TRUE
    )

    outlist_append <- future.apply::future_sapply(
      X = unname(seq_names),
      FUN = function(j) {
        local({
          # initialize inner_outlist
          outlist <- list()

          # set list_index
          list_index <- grep(j, uniques[[i]])

          u <- uniques[[i]][[list_index]]
          u$variable_name <- gsub(
            pattern = "(\\.)(\\d)+$",
            replacement = "",
            x = names(uniques[[i]])[list_index]
          )

          # workaround to hide shiny-stuff, when going headless
          msg <- paste("Getting atemporal plausibility", u$name)
          DIZtools::feedback(
            msg,
            findme = "0c70327436",
            logfile_dir = rv$log$logfile_dir
          )

          key_cols <- get_key_col(rv)
          key_col_name_src <- key_cols$source
          key_col_name_tar <- key_cols$target

          # get information on source data
          for (k in c("source_data", "target_data")) {

            src_flag <- ifelse(
              k == "source_data",
              rv$source$system_name,
              rv$target$system_name
            )

            # get descriptions
            outlist[[k]]$name <- u$name
            outlist[[k]]$description <- u$description
            outlist[[k]]$var_dependent <- i
            outlist[[k]]$var_independent <- u$variable_name

            if (!is.null(u$filter[[src_flag]])) {
              outlist[[k]]$filter <- u$filter[[src_flag]]
            }
            if (!is.null(u$join_crit)) {
              outlist[[k]]$join_crit <- u$join_crit
            }

            # prepare specific valueset for conformance checks:
            # if factor
            if (atemp_vars[get("variable_name") == i, get("variable_type")] ==
                "enumerated") {

              constr <- u$constraints$value_set[[src_flag]]

              outlist[[k]]$checks$constraints <-
                as.character(
                  jsonlite::toJSON(
                    x = list("value_set" = constr),
                    auto_unbox = TRUE
                  )
                )
            }

            # TODO this is yet tailored to §21
            if (k == "source_data") {
              u_key <-
                mdr[get("source_system_name") == rv$source$system_name &
                      get("variable_name") == u$variable_name &
                      get("dqa_assessment") == 1, get(key_col_name_src)]
              raw_data <- "data_source"

            } else {
              u_key <-
                mdr[get("source_system_name") == rv$target$system_name &
                      get("variable_name") == u$variable_name &
                      # Back to key: 'variable_name' was assigned here:
                      get("dqa_assessment") == 1, get(key_col_name_tar)]
              raw_data <- "data_target"
            }
            stopifnot(length(u_key) == 1)

            if (i %in% colnames(rv[[raw_data]][[u_key]])) {
              if (!is.null(u$filter[[src_flag]])) {
                group_data <- rv[[raw_data]][[u_key]][grepl(
                  u$filter[[src_flag]],
                  get(u$variable_name),
                  perl = TRUE
                ), c(i, u$variable_name), with = FALSE]
              } else {
                group_data <- rv[[raw_data]][[u_key]][!is.na(
                  get(u$variable_name)
                ), c(i, u$variable_name), with = FALSE]
              }

            } else {

              msg <- paste(
                paste(i, "not in", colnames(rv[[raw_data]][[u_key]])),
                collapse = "\n"
              )
              DIZtools::feedback(
                msg,
                findme = "9d48df8805",
                logfile_dir = rv$log$logfile_dir
              )

              # we need to find the correct data and merge
              if (k == "source_data") {
                m_key <-
                  mdr[get("source_system_name") == rv$source$system_name &
                        get("variable_name") == i &
                        get("dqa_assessment") == 1, get(key_col_name_src)]
              } else {
                m_key <-
                  mdr[get("source_system_name") == rv$target$system_name &
                        get("variable_name") == i &
                        # Back to key: 'variable_name' was assigned here:
                        get("dqa_assessment") == 1, get(key_col_name_tar)]
              }
              stopifnot(length(m_key) == 1)

              if (!is.null(u$filter[[src_flag]])) {
                m_x <- unique(rv[[raw_data]][[u_key]][grepl(
                  u$filter[[src_flag]],
                  get(u$variable_name),
                  perl = TRUE
                ), ])
              } else {
                m_x <- unique(rv[[raw_data]][[u_key]])
              }

              # look, if join_crit is already in our target table, if so,
              # create m_y directly
              if (any(
                grepl(
                  pattern = u$join_crit,
                  x = colnames(rv[[raw_data]][[m_key]])
                )
              )) {
                msg <- paste(
                  "--> found",
                  i, "in", m_key,
                  " (Joined over 2 tables)"
                )
                DIZtools::feedback(
                  msg,
                  findme = "39a4e1b70b",
                  logfile_dir = rv$log$logfile_dir
                )

                m_y <- unique(rv[[raw_data]][[m_key]])
                coln_x <- NULL
              }  else {
                # else join another table
                if (k == "source_data") {
                  j_key <-
                    mdr[get("source_system_name") == rv$source$system_name &
                          get("variable_name") == u$join_crit &
                          get("dqa_assessment") == 1, get(key_col_name_src)]
                } else {
                  j_key <-
                    mdr[get("source_system_name") == rv$target$system_name &
                          get("variable_name") == u$join_crit &
                          # Back to key: 'variable_name' was assigned here:
                          get("dqa_assessment") == 1, get(key_col_name_tar)]
                }
                stopifnot(length(j_key) == 1)
                msg <- paste(
                  "--> found", i, "in",
                  j_key, " (Joined over 3 tables)"
                )
                DIZtools::feedback(
                  msg,
                  findme = "39a4e2b70b",
                  logfile_dir = rv$log$logfile_dir
                )

                # get colnames
                coln_x <- colnames(rv[[raw_data]][[m_key]])
                coln_y <- colnames(rv[[raw_data]][[j_key]])

                # find matching colname
                coln_x <- coln_x[lapply(coln_x, function(i) {
                  any(grepl(i, coln_y))
                }) == TRUE]
                coln_y <- coln_y[grepl(coln_x, coln_y)]


                m_y <- data.table::merge.data.table(
                  x = unique(rv[[raw_data]][[m_key]]),
                  y = unique(rv[[raw_data]][[j_key]]),
                  by.x = coln_x,
                  by.y = coln_y,
                  all.x = TRUE,
                  suffixes = c("", ""),
                  allow.cartesian = TRUE
                )
              }

              group_data <- data.table::merge.data.table(
                x = m_x,
                y = m_y,
                by.x = colnames(m_x)[grepl(u$join_crit, colnames(m_x))],
                by.y = colnames(m_y)[grepl(u$join_crit, colnames(m_y))],
                all.x = TRUE,
                suffixes = c("", ""),
                allow.cartesian = TRUE
              )

              # delete not necessary colnames
              vec <- setdiff(colnames(group_data), c(coln_x, i))
              group_data[, (vec) := NULL]

              rm(m_x, m_y)
              invisible(gc())
            }
            outlist[[k]][[raw_data]] <- group_data
            if (is.null(u$filter[[src_flag]])) {
              outlist[[k]]$filter <- NA
            }
            rm(group_data)
            invisible(gc())
          }
          return(outlist)
        })
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    outlist <- c(outlist, outlist_append)
  }
  names(outlist) <- tolower(names(outlist))
  return(outlist)
}
