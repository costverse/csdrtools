library(magrittr)

## Build up functions import native flex file, 1 or multiple
# rio import as tibble
# ff - flexfiles
## Import one FlexFile

# ======================================================================================================
import_ff_json_native <- function(file_path) {
  ## One argument, file_path reads the zipped flexfile
  json <- base::list.files(path = file_path, full.names = TRUE, recursive = TRUE)
  ## initializes list
  ff <- base::list()
  ## assigns the Flex File into the initialized list
  ff[[1]] <- rio::import_list(json[1], setclass = "tibble")
  
  ff <- purrr::flatten(ff)
  
  ff
}
## Example =============================================================================================

ffiles_one <- import_ff_json_native("O:/Personal/Ben Berkman/costverse project/csdrtools/R/Fake Data/One_Submission/")

########################################################################################################

## Import multiple FlexFiles from desired path
# ======================================================================================================

import_ffiles_json_native <- function(file_path) {
  
  json <- base::list.files(path = file_path, full.names = TRUE, recursive = TRUE)
  
  ff <- base::list()
  
  for(i in 1:length(json)){
    
    ff[[i]] <- rio::import_list(json[i], setclass = "tibble")
    
  }
  
  ff
}

## Examle =============================================================================================

ffiles_mult <- import_ffiles_json_native("O:/Personal/Ben Berkman/costverse project/csdrtools/R/Fake Data/Two_Submission/")

# ======================================================================================================

## Assings UID to each FlexFile List, using Submission event number, flattens lists and binds by dataframe
# ======================================================================================================
# purr condensed
ffiles_stack <- function(ffiles_list) {
  
  for(i in 1:length(ffiles_list)){
    
    ffiles_list[[i]] <- lapply(ffiles_list[[i]], function(df){
      if (nrow(df) > 0)
        cbind(df, ff_id = ffiles_list[[i]][["ReportMetadata"]][["SubmissionEvent_Number"]])
      else
        cbind(df, ff_id = character())
    })
  }
  
  unpack <- purrr::flatten(ffiles_list)
  
  stacked <- split(unpack, names(unpack)) %>% 
    purrr::map(plyr::rbind.fill)
  # mapdfr map to fill
  stacked
}

## Examle =============================================================================================

ffiles_uid <- ffiles_stack(ffiles_mult)


# ======================================================================================================

## Cleans dataframe names in a list as well as columns within each data frame
# ======================================================================================================
clean_ff_names <- function(ff_list) {
  
  names(ff_list) <- janitor::make_clean_names(names(ff_list))
  
  names(ff_list)[names(ff_list)=="cli_ns"] <- "clins"
  
  ff <- ff_list
  for(i in 1:length(ff)){
    ff[[i]] <- janitor::clean_names(ff[[i]])
    
  }
  ff
}
## Examle =============================================================================================

ff_cleansed_one <- clean_ff_names(ffiles_one)
ff_cleansed_two <- clean_ff_names(ffiles_uid)

