downsample_welcome <- function() {
  cat("\n")
  cat("===================================================\n")
  cat("             YOU LOADED downsample_deg                 \n")
  cat("===================================================\n")
  cat("\n")
  cat("this function subsamples DEG files by an allotted time period\n")
  cat("Combines each 10 minute period into 2 hr periods, taking the\n")
  cat("mean of wets0-20 for each 2 hr period.\n")
  
  cat("\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")
  cat("- Define downsampled.dir in project_settings.R\n")
  
  
  cat("=============================================\n\n")}

downsample_welcome()

#function to downsample by 2hrs - move to utils once ready
downsample_deg <- function(file_path) {
  deg_data <- read_csv(file_path, show_col_types = FALSE)
  
  #bring back a datetime column so R can read dates in POSIXct format
  deg_data <- deg_data %>%
    mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    filter(!is.na(datetime) & !is.na(`wets0-20`))
  
  #downsample by grouping date and 2 hr bins
  downsampled_data <- deg_data %>%
    mutate(bin = floor_date(datetime, "2 hours")) %>% #bin by 2hrs
    group_by(bin) %>%
    
    summarize(
      wets = round(mean(`wets0-20`, na.rm=TRUE), 3), #take the mean for binning
      .groups = "drop") %>%
    mutate(
      date = as.Date(bin),
      start_time = format(bin, "%H:%M:%S"),
      end_time = format(bin + hours(2) - seconds(1), "%H:%M:%S")) %>%
    
    select(date, start_time, end_time, wets)
  
  file_name <- basename(file_path)
  output_path <- file.path(downsampled.dir, file_name)
  
  write_csv(downsampled_data, output_path)
}