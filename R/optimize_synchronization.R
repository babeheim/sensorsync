

#let us now create and analyze a range of candidate data synchronizations 
#that apply drift offsets ranging from 30 to 50 seconds to the accelerometer data. 
#the reasonable range to explore is study dependent.

optimize_synchronization <- function(gps, accel, viz = TRUE) {

  candidate_drift_correction <- seq(from=30, to=50, by=1)
  #In this example the sequence of candidate drift corrections have an interval of one second,
  #but that doesn't have to be the case -- an interval of .1 second or 10 seconds would work as well.

  #a dataframe to store results
  res <- data.frame(candidate_drift_correction)
  res$r_squared <- NA

  for (i in 1:nrow(res))
  {
    candidate_synchronization <- get_candidate_synchronization(gps = gps, accel = accel, res$candidate_drift_correction[i])
    linear_model <- lm(data=candidate_synchronization, formula = step_count~meters_in_segment)
    if (viz) {
      plot(step_count~meters_in_segment, data = candidate_synchronization)
      abline(linear_model)
    }
    model_summary <- summary(linear_model)
    res$r_squared[i] <- model_summary$r.squared
  }

  #here is the maximal r-squared
  max_r_squared <- max(res$r_squared)
  #and here is the optimal drift offset
  optimal_drift_offset <- res$candidate_drift_correction[res$r_squared==max_r_squared]

  #Now let us plot the r-squared values of different time drift corrections
  if (viz) {
    plot(res$candidate_drift_correction, res$r_squared, xlab="Accelerometer drift correction (seconds)", ylab="R-squared", pch=16)
    lines(x=c(optimal_drift_offset, optimal_drift_offset), y=c(0, max_r_squared), col="red")
  }

  #now let us save the synchronization that is optimally ajusted for drift into the data directory
  the_optimal_synchronization <- get_candidate_synchronization(gps, accel, optimal_drift_offset)

  return(the_optimal_synchronization)

}
