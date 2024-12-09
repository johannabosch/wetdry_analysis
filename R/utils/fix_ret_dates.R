# Helper function to process DEG files and get both first and last dates
get_dates_from_deg <- function(file_path) {

  data <- read.table(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Check if the date column is numeric
  if (is.numeric(data$date)) {
    # Convert numeric date (days since 1970-01-01) to Date format
    data$date <- as.Date(data$date, origin = "1970-01-01")
  } else {
    # Convert string representation to Date format
    data$date <- as.Date(data$date, format = "%d/%m/%Y")
  }
  
  # Get the first and last dates in the DEG file
  first_date <- min(data$date, na.rm = TRUE)
  last_date <- max(data$date, na.rm = TRUE)

  return(list(first_date = first_date, last_date = last_date))
}
update_ret_date <- function(clipping_md, deg.dir) {
  # Create a copy of clipping_md to modify
  fixed_md <- clipping_md
  
  # Iterate through each row of clipping_md
  for (i in 1:nrow(fixed_md)) {
    deg_file <- file.path(deg.dir, fixed_md$wetdry_filename[i])
    
    if (file.exists(deg_file)) {
      # Get both the first and last dates from the DEG file
      date_info <- get_dates_from_deg(deg_file)
      first_date_in_deg <- date_info$first_date
      last_date_in_deg <- date_info$last_date
      
      # Convert depDate_comb and retDate_comb to Date type if they are not already
      if (is.character(fixed_md$retDate_comb[i])) {
        retDate_comb <- as.Date(fixed_md$retDate_comb[i], format = "%d/%m/%Y")
      } else {
        retDate_comb <- fixed_md$retDate_comb[i]  # Already a Date object
      }
      
      if (is.character(fixed_md$depDate_comb[i])) {
        depDate_comb <- as.Date(fixed_md$depDate_comb[i], format = "%d/%m/%Y")
      } else {
        depDate_comb <- fixed_md$depDate_comb[i]  # Already a Date object
      }
      
      # Check if the last date in the DEG file is greater than retDate_comb
      if (!is.na(last_date_in_deg) && last_date_in_deg < retDate_comb) {
          cat("\n=====================================================\n", deg_file, "\n")
        cat("\nDeployment date:", depDate_comb, "- Retrieval date:", retDate_comb)
        cat("\nUpdating retDate_comb for row:", i, "from", fixed_md$retDate_comb[i], "to", format(last_date_in_deg, "%d/%m/%Y"), "\n")
        # Update retDate_comb in fixed_md
        fixed_md$retDate_comb[i] <- format(last_date_in_deg, "%d/%m/%Y")
      }
    } else {
      cat("\nFile does not exist:", deg_file, "\n")
    }
  }
  
  return(fixed_md)
  cat("\nAll rows processed for ", length(fixed_md$wetdry_filename), "files in deg.dir \n")

}
