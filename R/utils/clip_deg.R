clip_degs_welcome <- function() {
  cat("\n")
  cat("===================================================\n")
  cat("             YOU LOADED clip_degs                  \n")
  cat("===================================================\n")
  cat("\n")
  cat("this function clips DEG files based on deployment periods\n")
  cat("for combined estimated and observed dates, where depDate_comb\n")
  cat("is the deployment date and retDate_comb is the retrieval date\n")
  
  cat("\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")

  
  cat("=============================================\n\n")}

clip_degs_welcome()

clip_deg <- function(md_row, deg.dir, output.dir) {
  worker_log_file <- file.path(output.dir, paste0("clip_summary_worker_", Sys.getpid(), ".txt"))
  sink(worker_log_file, append = TRUE)
  on.exit(sink(), add = TRUE)
  
  # Extract DEG file path and metadata dates
  deg_file <- file.path(deg.dir, md_row$wetdry_filename)
  cat("\nProcessing FILE: ", basename(deg_file))
  
  dep_date <- as.Date(md_row$depDate_comb, format = "%d/%m/%Y")
  ret_date <- as.Date(md_row$retDate_comb, format = "%d/%m/%Y")
  
  # Validate dates
  if (is.na(dep_date) || is.na(ret_date)) {
    cat("\n❌ Invalid deployment/retrieval dates for", md_row$wetdry_filename, "\n")
    return(data.frame(wetdry_filename = md_row$wetdry_filename, qc_status = "INVALID DATES", rows_processed = 0))
  }
  
  # Check if the DEG file exists
  if (!file.exists(deg_file)) {
    cat("\n❌ File not found:", md_row$wetdry_filename, "\n")
    return(data.frame(wetdry_filename = md_row$wetdry_filename, qc_status = "FILE NOT FOUND", rows_processed = 0))
  }
  
  # Read the DEG file
  deg_data <- tryCatch({
    read_csv(deg_file, col_types = cols(
      date = col_date(format = "%d/%m/%Y"),
      time = col_character(),
      `wets0-20` = col_double()
    ))
  }, error = function(e) {
    cat("\n❌ Error reading file:", md_row$wetdry_filename, " - ", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(deg_data)) {
    return(data.frame(wetdry_filename = md_row$wetdry_filename, qc_status = "READ ERROR", rows_processed = 0))
  }
  
  # Filter data based on the date range
  clipped_deg <- deg_data %>%
    filter(!is.na(date) & date >= dep_date & date <= ret_date)
  
  cat("\n\nDeployment date:", dep_date, " --  Retrieval date:", ret_date)
  cat("\nFirst date in DEG: ", format(min(deg_data$date, na.rm = TRUE), "%Y-%m-%d"),
      "  --  Last date in DEG: ", format(max(deg_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("\nRows before filtering:", nrow(deg_data))
  cat("\nRows after filtering:", nrow(clipped_deg))
  
  # Define the output file path
  output_file <- file.path(output.dir, paste0("filtered_", md_row$wetdry_filename))
  
  # Write the filtered data
  tryCatch({
    write_csv(clipped_deg, output_file)
    cat("\nFiltered data written to:", output_file, "\n====================================================\n")
  }, error = function(e) {
    cat("\n❌ Error writing file:", output_file, " - ", e$message, "\n====================================================\n")
    return(data.frame(wetdry_filename = md_row$wetdry_filename, qc_status = "WRITE ERROR", rows_processed = nrow(clipped_deg)))
  })
  
  return(data.frame(wetdry_filename = md_row$wetdry_filename, qc_status = "SUCCESS", rows_processed = nrow(clipped_deg)))
}