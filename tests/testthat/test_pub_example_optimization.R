
test_that("optimization works", {
  accel <- read.csv("pub_example/example_accel_data.csv", stringsAsFactors = FALSE)
  gps <- read.csv("pub_example/example_gps_data.csv", stringsAsFactors = FALSE)
  expect_warning(out <- optimize_synchronization(gps = gps, accel = accel))
  expect_true(
    all(
      (out$adjusted_unix_time - out$unix_time) == 40
    )
  )
})

