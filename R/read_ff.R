library(magrittr)

## Import one or multiple FlexFiles from desired path ----

import_ffiles_json_native <- function(file_path) {
  
  json <- base::list.files(path = file_path, full.names = TRUE, recursive = TRUE)
  
  ff <- base::list()
  
  for(i in 1:length(json)){
    
    ff[[i]] <- rio::import_list(json[i], setclass = "tibble")
    
  }
  
  for(i in 1:length(ff)){
    
    ff[[i]] <- lapply(ff[[i]], function(df){
      if (nrow(df) > 0)
        cbind(df, ff_id = ff[[i]][["ReportMetadata"]][["SubmissionEvent_Number"]])
      else
        cbind(df, ff_id = character())
    })
  }
  
  ff <- purrr::flatten(ff)
  
  ff
}

## Example

ffiles_mult <- import_ffiles_json_native("O:/Personal/Ben Berkman/costverse project/csdrtools/R/Fake Data/Two_Submission/")
ffiles_one <- import_ffiles_json_native("O:/Personal/Ben Berkman/costverse project/csdrtools/R/Fake Data/One_Submission/")


## Assings UID to each FlexFile List, using Submission event number, flattens lists and binds by dataframe ----
ffiles_stack <- function(ffiles_list) {

  stacked <- split(ffiles_list, names(ffiles_list)) %>% 
    purrr::map(plyr::rbind.fill)
  
  stacked
}

## Example

ffiles_mult <- ffiles_stack(ffiles_mult)


## Cleans dataframe names in a list as well as columns within each data frame ----
clean_ff_names <- function(ff_list) {
  
  names(ff_list) <- janitor::make_clean_names(names(ff_list))
  
  names(ff_list)[names(ff_list)=="cli_ns"] <- "clins"
  
  ff <- ff_list
  for(i in 1:length(ff)){
    ff[[i]] <- janitor::clean_names(ff[[i]])
    
  }
  ff
}
## Example

ff_cleansed_one <- clean_ff_names(ffiles_one)
ff_cleansed_two <- clean_ff_names(ffiles_mult)

