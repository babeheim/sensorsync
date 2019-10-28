
get_candidate_synchronization <- function(gps_data_file, accel_data_file, seconds_offset)
{
  
  if(!file.exists(gps_data_file)) {
    print("GPS file not found, make sure full file path is specified")
    stop()
    }
  if(!file.exists(accel_data_file)) {
    print("GPS file not found, make sure full file path is specified")
    stop()
    }
  
  gps <- read.csv(gps_data_file, stringsAsFactors=F)
  
  
  accel <- read.csv(accel_data_file, stringsAsFactors=F)
  
  epoch_length <- median(diff(accel$unix_time))
  
  gps_names <- names(gps)
  accel_names <- names(accel)
  
  gps_columns_okay <- sum(gps_names=="lat")==1 & sum(gps_names=="lon")==1 & sum(gps_names=="unix_time")==1
  accel_columns_okay <- sum(accel_names=="unix_time")==1 & sum(accel_names=="step_count")==1
  
  if(!gps_columns_okay) {
    
    print("Error. The columns names of the GPS data file should be 'lat', 'lon', and 'unix_time'")
    stop()
  }
  
  if(!accel_columns_okay) {
    
    print("Error. The columns names of the accelerometer data file should be 'step_count' and 'unix_time'")
    stop()
  }
  
  
  accel$adjusted_unixtime <- accel$unix_time+seconds_offset
  min_gps_time <- min(gps$unix_time)
  max_gps_time <- max(gps$unix_time)
  
  first_accel_adjusted_unixtime_overlapping_with_gps <- accel$adjusted_unixtime[min(which(accel$adjusted_unixtime>=min_gps_time))]
  last_accel_adjusted_unixtime_overlapping_with_gps <- accel$adjusted_unixtime[max(which(accel$adjusted_unixtime<=max_gps_time))]
  
  #this restricts the analysis to the times in which the gps data and the (adjusted) accelerometer data temporally overlap
  accel <- accel[(accel$adjusted_unixtime>=first_accel_adjusted_unixtime_overlapping_with_gps&accel$adjusted_unixtime<=last_accel_adjusted_unixtime_overlapping_with_gps),]
  
  #re-implement this
  
  ###
  #this section will inject interpolated trackpoints into the GPS data at the accelerometer epoch (starting) unix_time values,
  #and thereafter, use these injected times as segment start times, by which to summarize how far the
  #subject traveled in each segment
  
  segment_start_times <- accel$adjusted_unixtime
  
  gps_times_raw <- gps$unix_time
  gps_times_and_segment_start_times <- unique(sort(c(gps_times_raw, segment_start_times)))
  
  #we now get interpolated trackpoints at the segment start times and the original trackpoints
  
  lat_int <- approx(x=gps$unix_time, y=gps$lat, xout=gps_times_and_segment_start_times, rule=1, method="linear")$y
  lon_int <- approx(x=gps$unix_time, y=gps$lon, xout=gps_times_and_segment_start_times, rule=1, method="linear")$y
  distances <- distHaversine(p1=cbind(lon_int[1:(length(lon_int)-1)], lat_int[1:(length(lat_int)-1)]), p2=cbind(lon_int[2:length(lon_int)], lat_int[2:length(lat_int)]))
  
  meters_from_prior_trackpoint_int <- c(0,distances)
  seconds_from_prior_trackpoint_int <- c(0,diff(gps_times_and_segment_start_times))
  speed_m_s_from_prior_trackpoint_int <- meters_from_prior_trackpoint_int/seconds_from_prior_trackpoint_int
  
  
  trackpoints_with_injected_segment_points <- data.frame(unix_times=gps_times_and_segment_start_times, seconds_from_prior_trackpoint_int, lat_int, lon_int, speed_m_s_from_prior_trackpoint_int, meters_from_prior_trackpoint_int)
  
  
  
  
  trackpoints_with_injected_segment_points$is_segment_start <- trackpoints_with_injected_segment_points$unix_times %in% segment_start_times
  trackpoints_with_injected_segment_points$segment_number <- NA
  
  #now we label each segment
  trackpoints_with_injected_segment_points$segment_number <- cumsum(trackpoints_with_injected_segment_points$is_segment_start)
  
  #get rid of any stray gps data before first adjusted accel epoch times
  trackpoints_with_injected_segment_points <- trackpoints_with_injected_segment_points[trackpoints_with_injected_segment_points$segment_number>0,]
  
  
  last_segment_start_time <- max(segment_start_times)
  end_of_last_segment_time <- last_segment_start_time + epoch_length
  trackpoints_with_injected_segment_points$segment_number[trackpoints_with_injected_segment_points$unix_times>end_of_last_segment_time] <- NA
  
  
  #get rid of travel prior to first segment start and travel after end of last segment
  trackpoints_with_injected_segment_points <- trackpoints_with_injected_segment_points[trackpoints_with_injected_segment_points$segment_number!=0,]
  trackpoints_with_injected_segment_points <- trackpoints_with_injected_segment_points[!is.na(trackpoints_with_injected_segment_points$segment_number),]

  trackpoints_with_injected_segment_points$meters_to_next_trackpoint <- c(trackpoints_with_injected_segment_points$meters_from_prior_trackpoint_int[2:nrow(trackpoints_with_injected_segment_points)],0)
  
  
  tps_sum <- trackpoints_with_injected_segment_points %>%
    group_by(segment_number) %>%
    summarise(meters_in_segment = sum(meters_to_next_trackpoint), segment_start_time=first(unix_times)) %>%
    as.data.frame()
  
  meters_in_epochs_gps <- tps_sum[,c("meters_in_segment", "segment_start_time")]
  accel_backup <- accel
  
  accel <- merge(x=accel, y=tps_sum, by.x=("adjusted_unixtime"), by.y=("segment_start_time"), all.x=TRUE) 
  accel$speed_m_s <- accel$meters_in_segment/epoch_length
  
  return(accel)
  
}