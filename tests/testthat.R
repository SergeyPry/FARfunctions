library(testthat)

test_that("FARfunctions",{
  expect_that(FARfunctions::make_filename(2013), equals("accident_2013.csv.bz2"))
}
)
