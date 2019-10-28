
#in this example script, we load gps data and accelerometer data from the same subject on the same day,
#then identify the optimal time drift correction for these data. After doing so, we plot the effects of time drift correction
#and then save an optimally synchronized data set.

# to test:
# no overlap entries between two input files
# e.g.   overlap_entries <- (accel$adjusted_unixtime >= min(gps$unix_time) &
#   accel$adjusted_unixtime <= max(gps$unix_time))

test_that("sync works with no offset", {
  candidate_drift_correction <- 0
  expect_warning(
    out <- get_candidate_synchronization(
      gps_data = "pub_example/example_gps_data.csv",
      accel = "pub_example/example_accel_data.csv",
      seconds_offset = candidate_drift_correction
    )
  )
  expect_equal(nrow(out), 4559)
  expect_equal(min(out$unix_time), 1435957259)
  expect_equal(max(out$unix_time), 1436025629)
  expect_true(abs(mean(out$speed_m_s, na.rm = TRUE) - 0.0884) < 1e-4)
  expect_true(abs(var(out$speed_m_s, na.rm = TRUE) - 0.0528) < 1e-4)
  expect_true(abs(mean(out$meters_in_segment, na.rm = TRUE) - 1.3259) < 1e-4)
  expect_true(abs(var(out$meters_in_segment, na.rm = TRUE) - 11.8907) < 1e-4)
  expect_true(
    all(
      (out$adjusted_unixtime - out$unix_time) == candidate_drift_correction
    )
  )
  expect_identical(out$segment_number, 1:nrow(out))
})

test_that("sync works with an offset of 10", {
  candidate_drift_correction <- 10
  expect_warning(
    out <- get_candidate_synchronization(
      gps_data = "pub_example/example_gps_data.csv",
      accel = "pub_example/example_accel_data.csv",
      seconds_offset = candidate_drift_correction
    )
  )
  expect_equal(nrow(out), 4558)
  expect_true(abs(mean(out$speed_m_s, na.rm = TRUE) - 0.0884) < 1e-4)
  expect_true(abs(var(out$speed_m_s, na.rm = TRUE) - 0.0527) < 1e-4)
  expect_true(abs(mean(out$meters_in_segment, na.rm = TRUE) - 1.3261) < 1e-4)
  expect_true(abs(var(out$meters_in_segment, na.rm = TRUE) - 11.8593) < 1e-4)
  expect_true(
    all(
      (out$adjusted_unixtime - out$unix_time) == candidate_drift_correction
    )
  )
  expect_identical(out$segment_number, 1:nrow(out))
})

test_that("sync works with an offset of -10", {
  candidate_drift_correction <- (-10)
  expect_warning(
    out <- get_candidate_synchronization(
      gps_data = "pub_example/example_gps_data.csv",
      accel = "pub_example/example_accel_data.csv",
      seconds_offset = candidate_drift_correction
    )
  )
  expect_equal(nrow(out), 4558)
  expect_true(abs(mean(out$speed_m_s, na.rm = TRUE) - 0.0884) < 1e-4)
  expect_true(abs(var(out$speed_m_s, na.rm = TRUE) - 0.0521) < 1e-4)
  expect_true(abs(mean(out$meters_in_segment, na.rm = TRUE) - 1.3261) < 1e-4)
  expect_true(abs(var(out$meters_in_segment, na.rm = TRUE) - 11.7266) < 1e-4)
  expect_true(
    all(
      (out$adjusted_unixtime - out$unix_time) == candidate_drift_correction
    )
  )
  expect_identical(out$segment_number, 1:nrow(out))
})

test_that("sync works with a variety of offsets", {
  candidate_drift_correction <- seq(from = 30, to = 50, by = 1)
  for (i in 1:length(candidate_drift_correction)) {
    expect_warning(
      out <- get_candidate_synchronization(
        gps_data = "pub_example/example_gps_data.csv",
        accel = "pub_example/example_accel_data.csv",
        seconds_offset = candidate_drift_correction[i]
      )
    )
    expect_true(
      all(
        (out$adjusted_unixtime - out$unix_time) == candidate_drift_correction[i]
      )
    )
  }
})
