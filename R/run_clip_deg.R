run_clip_deg_welcome <- function() {
  cat("\n")
  cat("===================================================\n")
  cat("             YOU LOADED run_clean_deg                 \n")
  cat("===================================================\n")
  cat("\n")
  cat("use this function with clip_deg for batch clipping\n")
  cat("\n")
  cat("\n")
  cat("- Ensure dependencies in project_settings.R\n")
  cat("- Define origin.dir and deg.dir in project_settings.R\n")
  
  cat("=============================================\n\n")}

run_clip_deg_welcome()

#first sink output to the log file
sink()

sink(log_file, #open sink to log
     split = TRUE) #also show console output
on.exit(sink(), add=TRUE)  #ensure sink closes

clipping_degs <- future_pmap_dfr(
  
  #set up for clip_deg
  list(
    wetdry_filename = clipping_md$wetdry_filename_cleaned, #use clean filenames,
    dep_date = clipping_md$depDate_comb,
    ret_date = clipping_md$retDate_comb
  ),
  
  function(wetdry_filename, dep_date, ret_date) { # anonymous function to process each row
    
    md_row <- tibble( # defining a row in our df for clip_deg function
      wetdry_filename = wetdry_filename,
      depDate_comb = dep_date,
      retDate_comb = ret_date
    )
    
    tryCatch(      # call clip_deg
      clip_deg(md_row, 
               deg.dir = deg.dir, 
               output.dir = output.dir, 
               zip_file = zip_file),
      error = function(e) {          #log warnings with a filename and error
        write(paste("Processing error: ", wetdry_filename, " - ", e$message, "\n"), file = log_file, append = TRUE)
        
        #return FAILED in resultign df (`clipping_degs`)
        tibble(wetdry_filename=wetdry_filename, qc_status = "FAILED", rows_processed=0)
      }
    )
  },
  .progress = TRUE
  
)


cat(" ==============CLIPPING SUMMARY================ \n")
cat("=================================================\n")

failed_files <- clipping_degs %>% filter(qc_status != "PASSED")
passed_files <- clipping_degs %>% filter(qc_status == "PASSED")

if(nrow(failed_files) > 0) {
  cat(nrow(passed_files), "/", nrow(clipping_md), " files were clipped succesfully!\n")
  cat("WARNING: ", nrow(failed_files), " files failed clipping, see `failed_files` \n")
  view(failed_files)
} else {
  cat(nrow(passed_files), "/", nrow(clipping_md), " files were clipped succesfully!")}

sink()