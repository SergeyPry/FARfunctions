library(testthat)
library(FARfunctions)

test_that("FARfunctions",{
  expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))

  expect_equal(dim(fars_read("accident_2014.csv.bz2"))[1], 30056 )
}
)
