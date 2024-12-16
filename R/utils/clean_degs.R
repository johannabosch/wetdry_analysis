clean_degs_welcome <- function() {
  cat("\n")
  cat("===================================================\n")
  cat("             YOU LOADED clean_degs                 \n")
  cat("===================================================\n")
  cat("\n")
  cat("this function preps DEG files for downsteam analyses\n")
  cat("It removes the metadata rows from a DEG file and\n")
  cat("filters out any rows with errors that occurred while\n")
  cat("the device was logging data.") 
  
  cat("\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")


  cat("=============================================\n\n")}

clean_degs_welcome()

clean_degs <- function(origin.dir, deg.dir) {
  deg_files <- list.files(origin.dir, pattern = "\\.deg$", full.names = TRUE)
  
  cores <- parallel::detectCores()
  plan(multisession, workers=cores/2)
  
  future_map(deg_files, function(file) {
    lines <- readLines(file)
    lines <- lines[-(1:19)]
    
    valid_lines <- lines[!grepl('Invalid|Missing|Data recorded', lines)]
    
    data <- read.table(text=paste(valid_lines, collapse="\n"), sep="\t", header=TRUE, stringsAsFactors = FALSE, check.names = FALSE)

    #split datetime columns
    if("DD/MM/YYYY HH:MM:SS" %in% colnames(data)) {
      data <- data %>%
        tidyr::separate(`DD/MM/YYYY HH:MM:SS`, into = c("date", "time"), sep = " ")
    } else {
      stop(paste(file, " does not have datetime combined"))}
    
    #check col names
    expected_cols <-  c("date", "time", "wets0-20")
    if(!identical(colnames(data), expected_cols)) {
      stop(paste("File ", file, " does not have expected column names"))
    }
    
    output_file <- file.path(deg.dir, basename(file))
    write.table(data, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  },
  
  .progress = TRUE)
  
  
  cat("\nCleaning complete!\n\n")
  cat("\nProcessed files (", length(list.files(origin.dir, pattern = "\\.deg$", full.names = TRUE)), "/", length(list.files(deg.dir, pattern = "\\.deg$", full.names = TRUE)), ") have been saved to:", deg.dir, "\n")
}