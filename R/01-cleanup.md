# Clean-up

## Getting started



## Metadata 

### Review

What we need from the metadata file:
`track_dataStart:` first date in raw trackfile (as downloaded from the tag)
`dep_date:` day the tag was deployed on the bird
`dep_dateEst:` my estimate of deploy date if not reported in original datasheet
`ret_date:` date the tag was removed from the bird
`ret_dateEst:`  estimate of retrieval date if not reported in original datasheet OR estimate of the last day of reliable light data if the logger battery died while still at sea
`track_dataEnd:` last date in raw trackfile (as downloaded from the tag)

The date window identified by these variables will include uninformative data before deployment and after retrieval, `track_dataEnd can be useful in identifying if/when a logger battery died at sea (track_dataEnd < ret_date).` When building the logic for cleaning up the DEG files, we can't forget to include scenarios where the device stopped collecting data before the retrieval date, so `ret_date` will be greater than the maximum date found in the DEG file.


``` r
#load the metadata
md <- read.csv(file.path(data.dir, "MasterMetadata_LHSP_Tracking_NL_NB_NS_06Dec2024.csv"))

# let's double check - every time there is an NA in retDate_comb, there should be no wet-dry file
print(md %>%
  filter(!is.na(wetdry_filename) & is.na(ret_date) & is.na(ret_dateEst))) # checks out
```

```
##  [1] colony                 tagType                tagMake               
##  [4] tagID                  tagProg                tagProgStartUTC       
##  [7] band                   sex                    dep_recap             
## [10] dep_handlerInits       dep_lat                dep_lon               
## [13] dep_burrowID           dep_plotID             dep_date              
## [16] dep_dateEst            dep_yr                 dep_mo                
## [19] dep_datetimeUTC        dep_startHandle        dep_time              
## [22] dep_durHandle          dep_brStatus           dep_burrowCont        
## [25] dep_mass               dep_wing               dep_wingType          
## [28] dep_tarsus             dep_bill               dep_tail              
## [31] dep_eggAge             dep_eggWid             dep_eggLen            
## [34] dep_chkAge             dep_notes              dep_techNotes         
## [37] ret_handlerInits       ret_birdYN             ret_tagYN             
## [40] ret_date               ret_dateEst            ret_yr                
## [43] ret_mo                 ret_datetimeUTC        ret_time              
## [46] ret_endHandle          ret_durHandle          ret_burrowID          
## [49] ret_burrowCont         ret_mass               ret_massChange        
## [52] ret_eggAge             ret_eggWid             ret_eggLen            
## [55] ret_chkAge             ret_spotCardYN         ret_bloodCapAmt       
## [58] ret_feathType          ret_feathNum           ret_notes             
## [61] ret_notesTissue        ret_tagStatus          ret_trackType         
## [64] wetdry_filename        track_filename         track_dataStart       
## [67] track_dataEnd          IPorig_track_filename  sourceFile            
## [70] gtDep_site             gtDep_lat              gtDep_lon             
## [73] gtDep_startDate        gtDep_startTime        gtDep_endDate         
## [76] gtDep_endTime          gtDep_startDatetimeUTC gtDep_endDatetimeUTC  
## [79] gtDep_notes            gtRet_site             gtRet_lat             
## [82] gtRet_lon              gtRet_startDate        gtRet_startTime       
## [85] gtRet_endDate          gtRet_endTime          gtRet_startDatetimeUTC
## [88] gtRet_endDatetimeUTC   gtRet_notes            gtRet_filename        
## [91] ESRF                   ret_brStatus           simulMateTrack        
## [94] repGLS                 repGPS                 repGLSGPS             
## <0 rows> (or 0-length row.names)
```

``` r
#double check that Migrate Tech are the only devices with wetdry data
print(md %>%
  group_by(tagMake) %>%
  summarise(filename_count = sum(!is.na(wetdry_filename))))
```

```
## # A tibble: 12 × 2
##    tagMake                            filename_count
##    <chr>                                       <int>
##  1 Biotrack: MK5040                                0
##  2 Biotrack: MK5440                                0
##  3 Biotrack: MK5540                                0
##  4 Biotrack: MK5540 C                              0
##  5 Biotrack: MK5740                                0
##  6 Biotrack: MK5740?                               0
##  7 Migrate Technology: W30A9-SEA                 200
##  8 Migrate Technology: W30A9-SEA-COOL             98
##  9 Migrate Technology: W30A9-SEA-NOT               0
## 10 Pathtrack: nanoFix GEO-Mini                     0
## 11 Pathtrack: nanoFix miniR3_12                    0
## 12 <NA>                                            0
```

``` r
#how many Migrate Tech (MT) devices total?
MT_devices <- md %>%  filter(tagMake %in% c("Migrate Technology: W30A9-SEA", "Migrate Technology: W30A9-SEA-COOL"))

#how many MT devices with missing wetdry files?
MT_devices_missing <- (MT_devices %>%
  filter(tagMake %in% c("Migrate Technology: W30A9-SEA", "Migrate Technology: W30A9-SEA-COOL"), is.na(wetdry_filename)) %>%
    select(wetdry_filename, tagID))

#how many MT devices have wetdry-data?
MT_devices_retrieved <- (MT_devices %>%
  filter(tagMake %in% c("Migrate Technology: W30A9-SEA", "Migrate Technology: W30A9-SEA-COOL"), !is.na(wetdry_filename)) %>%
    select(wetdry_filename, tagID))
```

### Housekeeping

To clean up the metadata for clipping, we need to:
  * filter out the Migrate Technology devices (they are the only devices with wetdry data)
  * remove any rows that have NA for `wetdry_filename`
  * merge cols for estimates and non-est for deployment and return dates, 


``` r
#clean up the metadata so that we only include the necessary columns for clipping
clipping_md <- md %>%
  
  filter(
    #filter for MT devices
    tagMake %in% c("Migrate Technology: W30A9-SEA", "Migrate Technology: W30A9-SEA-COOL"),
    #remove any NA rows in wetdry_filename 
    !is.na(wetdry_filename)) %>% 
  
  # merge estimated and observed columns 
  mutate(
    depDate_comb = as.Date(ifelse(!is.na(dep_dateEst), dep_dateEst, dep_date), format = "%Y-%m-%d"),
    retDate_comb = as.Date(
      ifelse(!is.na(ret_dateEst), substr(ret_dateEst, nchar(ret_dateEst) - 9, nchar(ret_dateEst)), 
             ret_date), format = "%Y-%m-%d")) %>%
  
  mutate(depDate_comb = format(depDate_comb, "%d/%m/%Y"), #keep dates in this format to match files
         retDate_comb = format(retDate_comb, "%d/%m/%Y")) %>%
  
  #only keep necessary columns 
  select(wetdry_filename, depDate_comb, retDate_comb, colony)

# checked that number of observations in `MT_devices_retrieved` is equal to `clipping_md`
```

Make a similar `metrics_md` file that we can use later on for HMM 


``` r
#clean up the metadata so that we only include the necessary columns for clipping
#clean up the metadata so that we only include the necessary columns for clipping
metrics_md <- md %>%
  

  filter(
    #filter for MT devices
    tagMake %in% c("Migrate Technology: W30A9-SEA", "Migrate Technology: W30A9-SEA-COOL"),
    #remove any NA rows in wetdry_filename 
    !is.na(wetdry_filename)) %>% 
  
  # merge estimated and observed columns 
  mutate(
    depDate_comb = as.Date(ifelse(!is.na(dep_dateEst), dep_dateEst, dep_date), format = "%Y-%m-%d"),
    retDate_comb = as.Date(
      ifelse(!is.na(ret_dateEst), substr(ret_dateEst, nchar(ret_dateEst) - 9, nchar(ret_dateEst)), 
             ret_date), format = "%Y-%m-%d")) %>%
  
  mutate(depDate_comb = as.Date(depDate_comb), #leave dates in standard format for when DEGS match
         retDate_comb = as.Date(retDate_comb)) %>% 
  
  mutate(bird_id = str_extract(basename(wetdry_filename), "[A-Z-0-9]{5}")) %>%

  #only keep necessary columns 
  select(bird_id, colony, wetdry_filename, depDate_comb, retDate_comb) %>%

  #categorize deployment periods in the dtaaframe
  mutate(
    deployment_period = case_when(
    depDate_comb >= as.Date("2017-01-01") & retDate_comb <= as.Date("2018-12-31") ~ "2017-2018",
    depDate_comb >= as.Date("2018-01-01") & retDate_comb <= as.Date("2019-12-31") ~ "2018-2019",
    depDate_comb >= as.Date("2019-01-01") & retDate_comb <= as.Date("2020-12-31") ~ "2019-2020",
    depDate_comb >= as.Date("2020-01-01") & retDate_comb <= as.Date("2021-12-31") ~ "2020-2021",
    depDate_comb >= as.Date("2021-01-01") & retDate_comb <= as.Date("2022-12-31") ~ "2021-2022",
    depDate_comb >= as.Date("2022-01-01") & retDate_comb <= as.Date("2023-12-31") ~ "2022-2023",
    depDate_comb >= as.Date("2023-01-01") & retDate_comb <= as.Date("2024-12-31") ~ "2023-2024",
    TRUE ~ "Other"   # any records outside of these ranges
    )
  )
```

and then a tibble to review how many files we have per deployment period per colony


``` r
deployment_summary <- metrics_md %>%
  
  group_by(deployment_period, colony) %>%
  
  summarise(file_count=n(), .groups = "drop") %>%
  
  pivot_wider(names_from = colony,  #make tibble with colony columns
              values_from = file_count, 
              values_fill = 0 # fill missing vals with 0
              ) %>%
  mutate(TOTAL = rowSums(select(., -deployment_period))) %>%
  
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      deployment_period = "TOTAL"))

deployment_summary
```

```
## # A tibble: 8 × 8
##   deployment_period `Baccalieu - Ned Walsh` `Bon Portage` Country  Gull  Kent
##   <chr>                               <int>         <int>   <int> <int> <int>
## 1 2017-2018                               6             0       0     0     0
## 2 2018-2019                               0             5       6    10    10
## 3 2019-2020                              10            17      17    23    16
## 4 2021-2022                              20             0      16    16    27
## 5 2022-2023                               7            13       0    11     0
## 6 2023-2024                              12             0       0    12     0
## 7 Other                                   0             0       0     2     0
## 8 TOTAL                                  55            35      39    74    53
## # ℹ 2 more variables: `Middle Lawn` <int>, TOTAL <dbl>
```

Looks like there were 3 devices that stayed out for longer than a year.
Lets handle these later on*


``` r
print(metrics_md[metrics_md$deployment_period =="Other", ])
```

```
##     bird_id      colony                  wetdry_filename depDate_comb
## 154   BU971        Gull         BU971_25Nov23_194250.deg   2019-09-22
## 177   CD493        Gull CD493_22Jun23_004919driftadj.deg   2021-09-16
## 258   CD501 Middle Lawn CD501_25Nov23_184453driftadj.deg   2021-09-22
##     retDate_comb deployment_period
## 154   2023-08-22             Other
## 177   2023-01-05             Other
## 258   2023-01-01             Other
```

Save the metadata file for when we run HMM

``` r
write.csv(metrics_md, file.path(data.dir, "metrics_md.csv"))
```


## DEG files

The number of DEG files we have in that original folder should equal `MT_devices_retrieved`

So we want to clip this for our known deployment times from the metadata
For this bird we want to filter the DEG file to keep any dates from 2017-09-20 to 2018-06-29.

``` r
#access the deg file
deg_file <- file.path(origin.dir, "BH584_04Jul18_125355driftadj.deg")

#here's what a raw deg looks like
print(readLines(deg_file, n = 25)) #our data starts at line 20
```

```
##  [1] "Migrate Technology Ltd logger"                                                                                                                
##  [2] "Type: 10.13.4, current settings: Approx 1'C resolution. Conductivity >63 for 'wets' count. Light range 4, clipped OFF, max light regime. XT. "
##  [3] "Logger number: BH584"                                                                                                                         
##  [4] ""                                                                                                                                             
##  [5] "MODE: 6B (light, wet/dry recorded) - FOR OLD MODE 6, CLIPPED MUST BE ON"                                                                      
##  [6] "LIGHT: Sampled every minute with max light recorded every 5mins."                                                                             
##  [7] "TEMPERATURE: NOT RECORDED."                                                                                                                   
##  [8] "WET/DRY: Sampled every 30secs with number of samples wet recorded every 10mins."                                                              
##  [9] "Max record length = 26 months. Total battery life approx 9 months. Logger is currently 11 months old."                                        
## [10] ""                                                                                                                                             
## [11] "Programmed: 13/09/2017 17:18:06. Start of logging (DD/MM/YYYY HH:MM:SS): 13/09/2017 17:18:06"                                                 
## [12] "Age at start of logging (secs): 3372472, approx 1 months"                                                                                     
## [13] "End of logging (DD/MM/YYYY HH:MM:SS): 04/07/2018 12:51:58"                                                                                    
## [14] "Age at end of logging (secs): 28757996, approx 11 months"                                                                                     
## [15] "Timer (DDDDD:HH:MM:SS): 00293:19:32:04"                                                                                                       
## [16] "Drift (secs): 108. Memory not full. "                                                                                                         
## [17] "Pointers: 47511,105444,15000804,15000804,0,0,59% light mem used,50% wet/dry/temp mem used"                                                    
## [18] "Tcals (Ax^3+Bx^2+Cx+D): 12065.555,-25651.861,17238.887,-3643.479"                                                                             
## [19] "Approx 1'C resolution. Conductivity >63 for 'wets' count. Light range 4, clipped OFF, max light regime. XT. "                                 
## [20] "DD/MM/YYYY HH:MM:SS\twets0-20"                                                                                                                
## [21] "13/09/2017 17:28:06\t0"                                                                                                                       
## [22] "13/09/2017 17:38:06\t0"                                                                                                                       
## [23] "13/09/2017 17:48:06\t0"                                                                                                                       
## [24] "13/09/2017 17:58:06\t0"                                                                                                                       
## [25] "13/09/2017 18:08:06\t0"
```

Right now the DEG files contain a bunch of metadata (the first 20 lines), and data from the entire track time, from `track_dataStart` to `track_dataEnd` in the `migrate_md` dataframe. On line 20, you can see the two columns (`datetime` and `wets0-20`) that log data for the device. The rows after line 20 sometimes contain random errors that occur while the device is logging data, so we have clean those out too and also isolate the date from the `datetime` column.

## Cleaning DEGs

I made a `clean_deg function` to remove the noise from the DEG files. 


``` r
source(here::here("utils/clean_degs.R"), echo=F)


if (FALSE) {
clean_degs(origin.dir, deg.dir)
}
```

Use this function to fix the retrieval dates in clipping_md when retrieval date is greater than the last day listed in the DEG. This preps metadata for clipping the DEG files based on deployment periods.

``` r
source(here("utils/fix_ret_dates.R"), echo = F)
```

```
## 
## =================================================== 
##              YOU LOADED fix_ret_dates                  
##    view this function in wey-dry_analysis/R/utils   
## =================================================== 
## 
## this function fixes the retrieval dates in a metadata file
## In cases where retDate_comb is greater than the last date 
## listed in the DEG file, the last date in the DEG file 
## replaces the retrieval date. This preps the metadata for clipping the DEG files based on deployment periods.
## 
## - Ensure dependencies in project_settings.R
## =============================================
```

``` r
#this just changes the structure of clipping_md, no files are changed
fixed_md <- update_ret_date(clipping_md, deg.dir)
```

```
## 
## =====================================================
##  C:/Users/BoschJ/Desktop/wet-dry_analysis/data/DEG_clean/CD491_2461_13483_GLS411_NED_Walsh.deg 
## 
## Deployment date: 18856 - Retrieval date: 19177
## Updating retDate_comb for row: 25 from 04/07/2022 to 10/03/2022 
## 
## =====================================================
##  C:/Users/BoschJ/Desktop/wet-dry_analysis/data/DEG_clean/BU971_25Nov23_194250.deg 
## 
## Deployment date: 18161 - Retrieval date: 19591
## Updating retDate_comb for row: 154 from 22/08/2023 to 12/03/2021 
## 
## =====================================================
##  C:/Users/BoschJ/Desktop/wet-dry_analysis/data/DEG_clean/BP496_09Aug19_171436driftadj.deg 
## 
## Deployment date: 17787 - Retrieval date: 18071
## Updating retDate_comb for row: 211 from 24/06/2019 to 22/06/2019 
## 
## =====================================================
##  C:/Users/BoschJ/Desktop/wet-dry_analysis/data/DEG_clean/BU784_14Aug20_141554driftadj.deg 
## 
## Deployment date: 18139 - Retrieval date: 18273
## Updating retDate_comb for row: 221 from 12/01/2020 to 06/09/2019
```

``` r
#looks like retrieval dates for 5 files had to be fixed
```


When the function prints this output, the dates are numerical but I double checked and it is doing it properly so dates are just being printed kind of wonky in the output.

4 files had to be changed because the last date in the DEG file was earlier than the retrieval date:
  * BU784_14Aug20_141554driftadj.deg
  * BP496_09Aug19_171436driftadj.deg
  * BU971_25Nov23_194250.deg
  * CD491_2461_13483_GLS411_NED_Walsh.deg

## Clipping DEGS

I'm using `Furrr` to speed up the clipping task. [Purrr](purrr.tidyverse.org/) basically replaces R's `apply` family, and [Furrr](furrr.futureverse.org/) extends Purrr to support parallel processing. Purrr's `pmap()` family  applies a function row-wise over multiple inputs and bins the results into a single data frame (`pmap_dfr` - data frame row-bind). 

This works for our big clip job because we can use `Furrr` in combination with `dplyr` functions like `mutate()` `summarize()` or `filter()` to optimize clipping, we can still process a single file at a time to avoid overloading memory, but we can distribute the work across multiple cores for faster execution so that one core deals with 50 files 

I clipped the data sequentially with Purrr for 10 files first to get the logic down, and then switched to Furrr to build a bigger function (`clip_degs.R`).


``` r
source(here("utils/clip_deg.R"), echo = F)
```

```
## 
## =================================================== 
##              YOU LOADED clip_degs                   
##    view this function in wey-dry_analysis/R/utils   
## =================================================== 
## 
## this function clips DEG files based on deployment periods
## for combined estimated and observed dates, where depDate_comb
## is the deployment date and retDate_comb is the retrieval date
## 
## 
## - Ensure dependencies in project_settings.R
## =============================================
```


``` r
#run on a subset of 10 files to start
subset <- fixed_md[1:10, ]
str(subset)
```

```
## 'data.frame':	10 obs. of  4 variables:
##  $ wetdry_filename: chr  "BH594_04Jul18_135310driftadj.deg" "BH596_04Jul18_134429driftadj.deg" "BH602_04Jul18_132708driftadj.deg" "BH584_04Jul18_125355driftadj.deg" ...
##  $ depDate_comb   : chr  "20/09/2017" "20/09/2017" "20/09/2017" "20/09/2017" ...
##  $ retDate_comb   : chr  "29/06/2018" "29/06/2018" "29/06/2018" "01/07/2018" ...
##  $ colony         : chr  "Baccalieu - Ned Walsh" "Baccalieu - Ned Walsh" "Baccalieu - Ned Walsh" "Baccalieu - Ned Walsh" ...
```



``` r
# Loop through each row in the 'subset' data frame and call clip_deg
if (FALSE) {
profvis({
for (i in 1:nrow(subset)) {
  print(paste("Processing row:", i))
  print(subset[i, , drop = FALSE])
  
  result <- clip_deg(subset[i, , drop = FALSE], deg.dir, output.dir, zip_file, log_file)
}})

# using write_csv() the Date column (now of type Date) is automatically 
# converted to the standard YYYY-MM-DD format -  default behavior of the 
# readr package so I'll just keep it that way
}
```

Without using Furrr process took :
* 85.6MB of memory on my machine 
* 5.27 seconds for 10 files

## Parallel Batch-clipping

I have 8 cores on my machine


``` r
#define the number of cores on your machine
cores <- parallel::detectCores()
```

### Testing 

``` r
# define function for a single row with clip deg
process_row <- function(row, deg.dir, output.dir, log_file) {
  clip_deg(row, deg.dir, output.dir, log_file)
  }
```

I'm testing Furrr with 7 and then 4 cores to process 10 files to start, then scale it up to 50 files


``` r
if (FALSE) {
# first I'll try using half of my cores
# and then try using one less than what I have available
plan(multisession, workers = (cores/2))

plan()


profvis({
results <- future_map_dfr(
  .x = split(subset, seq_len(nrow(subset))),
  .f = ~ process_row(.x, deg.dir, output.dir, log_file),
  .progress = TRUE)
})

print(results)
}
```

Only took up 10.9MB of memory and around 4.64 seconds to run 10 files on 7 cores
On 4 cores it took 5.1MB and 3.74 seconds to run (using 4 cores)

Try scaling up to 50 files with 4 cores to see how it performs 


``` r
if (FALSE) {
  
subset <- fixed_md[1:50, ]
str(subset)

plan()

profvis({
results <- future_map_dfr(
  .x = split(subset, seq_len(nrow(subset))),
  .f = ~ process_row(.x, deg.dir, output.dir, zip_file, log_file),
  .progress = TRUE)
})
}
```

With 4 cores for 50 files, Furrr::future_map_dfr used 6.4MB of memory and took 7.62 seconds.

### Final clip

Let's run it on all files in deg.dir
created a sink in clip_degs to log output to a file instead of console


``` r
if (FALSE) {
subset <- fixed_md[1:10, ]
str(subset)

aggregate_logs <- function(output.dir, log_file) {
  worker_logs <- list.files(output.dir, pattern = "clip_summary_worker_.*\\.txt", full.names = TRUE)
  file.append(log_file, worker_logs)
  file.remove(worker_logs)
}

log_file <- file.path(output.dir, "clip_summary.txt")
if (file.exists(log_file)) file.remove(log_file)

#restart R and clear env, then try with all files on 4 cores
plan(sequential)  
# use sequential for testing
#switch to `multisession` for parallel

results <- future_map_dfr(
  .x = split(subset, seq_len(nrow(subset))),
  .f = ~ clip_deg(.x, deg.dir, output.dir),
  .progress = TRUE
)

print(results)

}
```


``` r
if (FALSE) {
# Switch to parallel processing for the full dataset
plan(multisession, workers = cores/2)

if (file.exists(log_file)) file.remove(log_file)
  
worker_logs <- list.files(output.dir, pattern = "^clip_summary_worker_.*\\.txt$", full.names = TRUE)

if (length(worker_logs) > 0) file.remove(worker_logs)

# Process the full dataset
results_full <- future_map_dfr(
  .x = split(fixed_md, seq_len(nrow(fixed_md))),
  .f = ~ clip_deg(.x, deg.dir, output.dir),
  .progress = TRUE
)

# Consolidate logs
aggregate_logs(output.dir, log_file)

print(results_full)
}
```


274 files on 4 cores required 9.2MB of memory allocation and around 58 seconds.



## DEG File Issues

Make a new metadata file that reviews the days deployed 


``` r
deployment_md <- clipping_md %>%
  mutate(
    depDate_comb = as.Date(depDate_comb, format = "%d/%m/%Y"),
    retDate_comb = as.Date(retDate_comb, format = "%d/%m/%Y"),
    days_deployed = as.numeric(retDate_comb - depDate_comb) +1
  ) %>%
  rowwise() %>%
  mutate(
    last_active_day = {
      
      deg_file <- file.path(deg.dir, wetdry_filename)
      
      if (file.exists(deg_file)) {
        deg_data <- tryCatch(
          read_csv(deg_file, col_types = cols( #define column types
            date = col_date(format = "%d/%m/%Y"),
            time = col_character(),
            `wets0-20` = col_double()
          )),
          error = function(e) NULL
        )
        if (!is.null(deg_data)) {
          max(deg_data$date, na.rm = TRUE) #use the max deg date 
        } else {
          NA
        }
      } else {
        NA
      }
    },
    days_inactive_before_ret = if (retDate_comb > last_active_day) {
      as.numeric(retDate_comb - last_active_day)+1
    } else {
      NA
    },
    days_active_before_ret = as.numeric(last_active_day-depDate_comb)+1,
    
    days_active_after_ret = if (last_active_day > retDate_comb) {
      as.numeric(last_active_day - retDate_comb)+1
    } else {
      NA
    },
    ) %>%
  ungroup() %>%
  
  select(wetdry_filename, 
         depDate_comb, retDate_comb, 
         last_active_day, 
         days_deployed, 
         days_active_before_ret, days_inactive_before_ret, 
         days_active_after_ret)

deployment_md
```

```
## # A tibble: 298 × 8
##    wetdry_filename       depDate_comb retDate_comb last_active_day days_deployed
##    <chr>                 <date>       <date>       <date>                  <dbl>
##  1 BH594_04Jul18_135310… 2017-09-20   2018-06-29   2018-07-04                283
##  2 BH596_04Jul18_134429… 2017-09-20   2018-06-29   2018-07-04                283
##  3 BH602_04Jul18_132708… 2017-09-20   2018-06-29   2018-07-04                283
##  4 BH584_04Jul18_125355… 2017-09-20   2018-07-01   2018-07-04                285
##  5 BH603_03Jul18_194628… 2017-09-19   2018-07-01   2018-07-03                286
##  6 BH595_04Jul18_131651… 2017-09-19   2018-07-01   2018-07-04                286
##  7 BU853_69W_4261-13034… 2019-08-27   2020-10-26   2021-04-19                427
##  8 BU869_77W_2461-13037… 2019-08-27   2020-11-06   2021-02-09                438
##  9 BU832_24Jul20_165309… 2019-08-27   2020-07-22   2020-07-24                331
## 10 BU847_84W_2461-13042… 2019-08-27   2020-11-05   2021-02-19                437
## # ℹ 288 more rows
## # ℹ 3 more variables: days_active_before_ret <dbl>,
## #   days_inactive_before_ret <dbl>, days_active_after_ret <dbl>
```

There should only be 4 files with values for days_active_after_ret, showing how many days the device was dead before retrieval

We can also see from this table how many days devices were deployed, how long some devices were on after retrieval, etc.

#### File: CD480_06Aug22_140316driftadj.deg

Noticed file `CD480_06Aug22_140316driftadj.deg` has DEG some dates listed as `2201-10-06`

``` r
file <- deployment_md[deployment_md$wetdry_filename == "CD480_06Aug22_140316driftadj.deg", ]
```

The DEG file has a weird invalid byte error message and then date formatting messed up.

On May 2022, `01/05/2022`, the device starts logging invalid bytes
then says it starts logging again in January of the next year? listed as `18/01/2201`, 
It can't be logging for 2022 anymore because the day `18/01` already passed in 2022
and metadata says retrieval day was in August 2022,  `2022-07-31`

So it says it started logging in January again but lists the date as `2201`

    ```
    01/05/2022 07:09:47	20
    01/05/2022 07:19:47	20
    01/05/2022 07:29:47	20
    01/05/2022 07:39:47	20
    01/05/2022 07:49:47	20
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    01/05/2022 07:59:47	20
    01/05/2022 08:09:47	20
    01/05/2022 08:19:47	20
    01/05/2022 08:29:47	20
    01/05/2022 08:39:47	20
    ...
    01/05/2022 21:49:47	20
    01/05/2022 21:59:47	20
    01/05/2022 22:09:47	20
    01/05/2022 22:19:47	20
    Missing data until 18/01/2201 08:39:17
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    Invalid byte: FF
    ...
    Invalid byte: FF
    Invalid byte: FF
    18/01/2201 08:40:40	20
    18/01/2201 08:50:40	20
    18/01/2201 09:00:40	20
    ...
    ```

When we applied clip_deg it filtered out all those messy dates leaving any dates before `01/05/2022`, so we're good to just keep using this file but it is bad quality.

The wets values from April 30 at 8AM to May 1 at 7AM were consistently equal to 20, so maybe something went wrong with the device there (`30/04/2022 08:19:47` to `01/05/2022 07:49:47`).

So really the last active day for this device was 01/05/2022 


#### File: BU784_14Aug20_141554driftadj.deg

Also the file `BU784_14Aug20_141554driftadj.deg` only has 937 lines after filtering
Looks like it was deployed for 135 days and died 7 days after it was deployed 


``` r
file <- deployment_md[deployment_md$wetdry_filename == "BU784_14Aug20_141554driftadj.deg", ]
print(file)
```

```
## # A tibble: 1 × 8
##   wetdry_filename        depDate_comb retDate_comb last_active_day days_deployed
##   <chr>                  <date>       <date>       <date>                  <dbl>
## 1 BU784_14Aug20_141554d… 2019-08-31   2020-01-12   2019-09-06                135
## # ℹ 3 more variables: days_active_before_ret <dbl>,
## #   days_inactive_before_ret <dbl>, days_active_after_ret <dbl>
```


Do we want to add a filtering step here for devices that didn't last over X number of days? Like a quality filtering step?
