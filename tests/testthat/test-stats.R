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

test_that("correct functioning of statistics", {

  local_edition(3)
  local_reproducible_output(rstudio = TRUE)

  set.seed(1)
  testdat <- data.table::data.table(
    "gender" = sample(x = c("m", "f", "u"),
                      size = 20,
                      replace = TRUE),
    "age" = rnorm(20, mean = 45, sd = 10)
  )
  testdat[, ("gender") := factor(get("gender"))]

  testres <- DQAstats:::count_uniques(
    data = testdat,
    var = "gender",
    sourcesystem = "testsystem",
    datamap = FALSE,
    utils_path = "testpath",
    filter = NULL
  )

  expect_true(testres$valids == 20)


  testres <- DQAstats:::extensive_summary(
    vector = testdat$age
  )

  expect_snapshot(
    testres,
    cran = FALSE,
    error = FALSE
  )

  do.call(
    file.remove,
    list(list.files(tempdir(), pattern = "log$", full.names = TRUE))
  )

})
