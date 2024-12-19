downsample_avg_welcome <- function() {
  cat("\n")
  cat("=================================================== \n")
  cat("           YOU LOADED downsample_deg            \n")
  cat("   view this function in wey-dry_analysis/R/utils   \n")
  cat("=================================================== \n")
  cat("\n")
  cat("this function subsamples DEG files by an allotted time period\n")
  cat("Combines each 10 minute period into X hr periods\n")
  cat("(define X using bin_time), and takes the sum of\n")
  cat("wets0-20 for each bin_time period.\n\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")
  cat("- Define downsampled.dir in project_settings.R\n")
  
  
  cat("=============================================\n\n")}

downsample_avg_welcome()

#function to downsample by 2hrs - move to utils once ready
downsample_deg <- function(file_path, bin_time = bin_time) {
  bird_id <- str_extract(basename(file_path), "(?<=filtered_)[A-Z-0-9]{5}")

  deg_data <- read_csv(file_path, show_col_types = FALSE) %>%
    
    #bring back a datetime column so R can read dates in POSIXct format
    mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    
    filter(!is.na(datetime) & !is.na(`wets0-20`))
  
  #downsample by grouping date in 2 hr bins
  downsampled_data <- deg_data %>%
    mutate(bin = floor_date(datetime, paste(bin_time, "hours"))) %>% #assigns each timestamp in datetime to a bin by X hrs
    group_by(bin) %>%
    
    summarize(
      
      #take the sum OR mean for binning
      wets = round(sum(`wets0-20`, na.rm=TRUE),5), 
      #wets = round(mean(`wets0-20`, na.rm=TRUE), 5),
      .groups = "drop") %>%
    
    mutate(
      bird_id = bird_id,
      date = as.Date(bin),
      start_time = format(bin, "%H:%M:%S"),
      end_time = format(bin + hours(bin_time) - seconds(1), "%H:%M:%S")) %>%
    
    select(bird_id, date, start_time, end_time, wets)
  
  return(downsampled_data)
}