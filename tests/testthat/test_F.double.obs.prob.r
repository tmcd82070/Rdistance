# test_F.double.obs.prob.r
library(Rdistance)
context("F.double.obs.prob")

set.seed(538392)
obsrv <- data.frame( obsby.1=rbinom(100,1,.75), obsby.2=rbinom(100,1,.5) )

test_that("numeric_observer_parameter_outside_expected_results_in_error", {
  # arrange
  
  # act
 
  # assert 
  expect_error(F.double.obs.prob(obsrv, 0), "Inappropriate 'observer' parameter")
  expect_error(F.double.obs.prob(obsrv, 3), "Inappropriate 'observer' parameter")
  expect_error(F.double.obs.prob(obsrv, -1), "Inappropriate 'observer' parameter")
})

test_that("string_observer_parameter_outside_expected_results_in_error", {
  # arrange
  
  # act
 
  # assert 
  expect_error(F.double.obs.prob(obsrv, "foo"), "Inappropriate 'observer' parameter")
  expect_error(F.double.obs.prob(obsrv, ""), "Inappropriate 'observer' parameter")
  expect_error(F.double.obs.prob(obsrv, "1"), "Inappropriate 'observer' parameter")
  expect_error(F.double.obs.prob(obsrv, "2"), "Inappropriate 'observer' parameter")
})

test_that("numeric_observer_input_using_example_data_gives_the_expected_value", {
  # arrange
  
  # act
  result_1 <- F.double.obs.prob( obsrv, observer = 1 )
  result_2 <- F.double.obs.prob( obsrv, observer = 2 )
 
  # assert 
  expect_equal_to_reference(result_1, "F.double.obs.prob_numeric_1.rds")
  expect_equal_to_reference(result_2, "F.double.obs.prob_numeric_2.rds")
})

test_that("both_observer_input_using_example_data_gives_the_expected_value", {
  # arrange
  
  # act
  result_b <- F.double.obs.prob( obsrv, observer = "both" )
 
  # assert 
  expect_equal_to_reference(result_b, "F.double.obs.prob_both.rds")
})

test_that("incorrect_name_for_obsby.2_in_dataframe_returns_expected_error", {
  # arrange
  obsrv <- data.frame( obsby.1=rbinom(100,1,.75), obsby.3=rbinom(100,1,.5) )
  
  # act
 
  # assert 
  expect_error(F.double.obs.prob( obsrv, observer = 2), "Variables 'obsby.1' and 'obsby.2' not found in input data frame.")
})