


get_candidate_synchronization <- function(gps_data_file, accel_data_file, seconds_offset, complete_gps_overlap = TRUE)
{

  if(!file.exists(gps_data_file)) {
    stop("GPS file not found, make sure full file path is specified")
  }
  if(!file.exists(accel_data_file)) {
    stop("GPS file not found, make sure full file path is specified")
  }

  gps <- read.csv(gps_data_file, stringsAsFactors=FALSE)

  accel <- read.csv(accel_data_file, stringsAsFactors=FALSE)

  # check that timestamps are sorted correctly in both files

  accel$segment_start_time <- accel$unix_time

  accel$time_from_last_segment <- c(NA, diff(accel$segment_start_time))
  accel$time_to_next_segment <- c(diff(accel$segment_start_time), NA)
  if (var(accel$time_from_last_segment, na.rm = TRUE) > 0) {
    warning("epoch lengths are not invariant in accelerometer data!")
  }

  accel$segment_end_time <- accel$segment_start_time + accel$time_to_next_segment

  epoch_length <- median(diff(accel$segment_start_time))

  gps_columns_okay <- all(c("lat", "lon", "unix_time") %in% colnames(gps))
  accel_columns_okay <- all(c("step_count", "unix_time") %in% colnames(accel))

  if(!gps_columns_okay) {
    stop("The columns names of the GPS data file should be 'lat', 'lon', and 'unix_time'")
  }

  if(!accel_columns_okay) {
    stop("The columns names of the accelerometer data file should be 'step_count' and 'unix_time'")
  }

  # we should also rescale based on the estimated drift in microseconds per second
  accel$adjusted_start_time <- accel$segment_start_time + seconds_offset
  accel$adjusted_end_time <- accel$segment_end_time + seconds_offset

  min_gps_time <- min(gps$unix_time)
  max_gps_time <- max(gps$unix_time)

  first_segment_start_time <- min(accel$adjusted_start_time)
  last_segment_start_time <- max(accel$adjusted_start_time)
  last_segment_end_time <- last_segment_start_time + epoch_length
  # this is a guess, since the end times are not explicitly recorded

  # this drops all accelerometer segments that lie
  # partially or completely outside gps boundaries

  if (complete_gps_overlap) {
    # only accept accelerometer segments completely bounded between gps points
    overlap_entries <- which(min(gps$unix_time) <= accel$adjusted_start_time &
      accel$adjusted_end_time <= max(gps$unix_time))
  } else {
    # accept accelerometer segments containing gps points but not bounded between them
    overlap_entries <- which(min(gps$unix_time) <= accel$adjusted_end_time &
      accel$adjusted_start_time <= max(gps$unix_time))
  }

  accel <- accel[overlap_entries, ]

  ###
  #this section will inject interpolated trackpoints into the GPS data at the
  #accelerometer epoch (starting) unix_time values,
  #and thereafter, use these injected times as segment start times,
  #by which to summarize how far the subject traveled in each segment
  
  if (complete_gps_overlap) {
    gps_times_and_segment_start_times <- sort(unique(c(gps$unix_time, accel$adjusted_start_time)))
  } else {
    gps_times_and_segment_start_times <- sort(unique(c(gps$unix_time, accel$adjusted_start_time, accel$adjusted_end_time)))
  }

  #we now get interpolated trackpoints at the segment start times and the original trackpoints
  lat_interpolated <- approx(x=gps$unix_time, y=gps$lat, xout=gps_times_and_segment_start_times, rule=1, method="linear")$y
  lon_interpolated <- approx(x=gps$unix_time, y=gps$lon, xout=gps_times_and_segment_start_times, rule=1, method="linear")$y
  distances <- distHaversine(
    p1 = cbind(lon_interpolated[1:(length(lon_interpolated) - 1)], lat_interpolated[1:(length(lat_interpolated) - 1)]),
    p2 = cbind(lon_interpolated[2:length(lon_interpolated)], lat_interpolated[2:length(lat_interpolated)])
  )

  all_points <- data.frame(
    unix_time = gps_times_and_segment_start_times,
    meters_from_prior_point = c(NA, distances),
    meters_to_next_point = c(distances, NA)
  )

  # associate each timestamp with an accelerometer segment
  all_points$is_segment_start <- all_points$unix_time %in% accel$adjusted_start_time
  all_points$segment_number <- cumsum(all_points$is_segment_start)

  # trim out gps data before first (adjusted) accelerometer segment times
  keep <- all_points$unix_time >= first_segment_start_time &
    all_points$unix_time < last_segment_end_time
  trimmed_points <- all_points[keep, ]

  tps_sum <- trimmed_points %>%
    group_by(segment_number) %>%
    summarise(meters_in_segment = sum(meters_to_next_point, na.rm = TRUE),
      segment_start_time = first(unix_time)) %>%
    as.data.frame()

  accel <- merge(x=accel, y=tps_sum, by.x=("adjusted_start_time"), by.y=("segment_start_time"), all.x=TRUE)
  accel <- rename(accel, adjusted_unix_time = adjusted_start_time)
  accel$speed_m_s <- accel$meters_in_segment/accel$time_to_next_segment

  return(accel)

}