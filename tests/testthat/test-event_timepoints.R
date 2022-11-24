test_that("check if event_timepoints functions are going correctly", {
  expect_equal(.event_timepoints(c(1,2,3,4,5,6),3), list(c(1,6)))
  expect_equal(.event_timepoints(c(2,3,4,5),3),list(c(1,4)))
  expect_equal(.event_timepoints(c(1,3,4,5,6,7,11,12,13,14,15),3),list(c(2,6),c(7,11)))
  expect_equal(.event_timepoints(c(1,3,4,5,6,7,11,12,13,14,15),2),list(c(2,6),c(7,11)))
  expect_equal(.event_timepoints(c(1,3,4,5,6,7,11,12,13,14,15,16),6),list(c(7,12)))
  expect_equal(.event_timepoints(c(1,3,5,7,9),1),list(c(1,5)))
  expect_equal(.event_timepoints(c(1,3,4,5,6,9),2),list(c(2,5)))
})
