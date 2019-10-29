
test_that("sync works with no offset", {
  candidate_drift_correction <- 0
  accel <- read.csv("minimal_example/example_accel_data.csv", stringsAsFactors = FALSE)
  gps <- read.csv("minimal_example/example_gps_data.csv", stringsAsFactors = FALSE)
  expect_silent(
    out <- get_candidate_synchronization(gps = gps, accel = accel,
      seconds_offset = candidate_drift_correction
    )
  )
  expect_equal(nrow(out), 1)
  expect_equal(min(out$unix_time), 3)
  expect_equal(max(out$unix_time), 3)
  expect_true(
    all(
      (out$adjusted_unixtime - out$unix_time) == candidate_drift_correction
    )
  )
  expect_identical(out$segment_number, 1:nrow(out))
})

test_that("sync errors with no overlap", {
  candidate_drift_correction <- 100
  accel <- read.csv("minimal_example/example_accel_data.csv", stringsAsFactors = FALSE)
  gps <- read.csv("minimal_example/example_gps_data.csv", stringsAsFactors = FALSE)
  expect_error(
    out <- get_candidate_synchronization(gps = gps, accel = accel,
      seconds_offset = candidate_drift_correction
    )
  )
})
