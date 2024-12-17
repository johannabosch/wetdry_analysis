downsample_avg_welcome <- function() {
  cat("\n")
  cat("===================================================\n")
  cat("           YOU LOADED downsample_deg_avg          \n")
  cat("===================================================\n")
  cat("\n")
  cat("this function subsamples DEG files by an allotted time period\n")
  cat("Combines each 10 minute period into X hr periods\n")
  cat("(define X using bin_time), and takes the mean of\n")
  cat("wets0-20 for each bin_time period.\n\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")
  cat("- Define downsampled.dir in project_settings.R\n")
  
  
  cat("=============================================\n\n")}

downsample_avg_welcome()

#function to downsample by 2hrs - move to utils once ready
downsample_deg_avg <- function(file_path, bin_time = bin_time) {
  deg_data <- read_csv(file_path, show_col_types = FALSE)
  
  #bring back a datetime column so R can read dates in POSIXct format
  deg_data <- deg_data %>%
    mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    filter(!is.na(datetime) & !is.na(`wets0-20`))
  
  #downsample by grouping date and 2 hr bins
  downsampled_data <- deg_data %>%
    mutate(bin = floor_date(datetime, paste(bin_time, "hours"))) %>% #this assigns each timestamp in datetime to a bin by X hrs
    group_by(bin) %>%
    
    summarize(
      wets = round(mean(`wets0-20`, na.rm=TRUE), 3), #take the mean for binning
      .groups = "drop") %>%
    mutate(
      date = as.Date(bin),
      start_time = format(bin, "%H:%M:%S"),
      end_time = format(bin + hours(bin_time) - seconds(1), "%H:%M:%S")) %>%
    
    select(date, start_time, end_time, wets)
  
  file_name <- basename(file_path)
  
  #assign output directory name based on bin time
  output_dir <- file.path(data.dir, paste0("downsampled_", bin_time, "hrs"))
  
  #create output dir if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)}
  
  output_path <- file.path(output_dir, file_name)
  
  write_csv(downsampled_data, output_path)
}