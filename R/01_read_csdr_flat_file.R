
# Provide the file name (with extension) of the CSDR submission Flat File.
flat_file_source_name <- "Flat_File_fake.xlsx"

# Provide the path to the CSDR submission Flat File (do not include the 
# file name or extension).
flat_file_source_path <- here::here("inst", "extdata")

# Were hours reported in 'thousands of hours'?
hours_in_k <- TRUE

# Were dollars reported in 'thousands of dollars'?
dollars_in_k <- TRUE

# Import the CSDR Flat File.  Keep an unmodified source copy in `ff_source`.
ff_source <- read_flat_file(path =  paste0(flat_file_source_path, "/", flat_file_source_name))
ff <- ff_source

# Convert 'thousands of hours' to 'hours', if required.
if (hours_in_k == TRUE) {
  ff$Data <- ff$Data %>% 
    dplyr::mutate(Value = dplyr::case_when(
      UnitOfMeasure == "Hours" ~ Value * 1000,
      TRUE ~ Value
    ))
}
rm(hours_in_k)

# Convert 'thousands of dollars' to 'dollars', if required.
if (dollars_in_k == TRUE) {
  ff$Data <- ff$Data %>% 
    dplyr::mutate(Value = dplyr::case_when(
      UnitOfMeasure == "Dollars" ~ Value * 1000,
      TRUE                       ~ Value
    ))
}
rm(dollars_in_k)

# Convert WBSElementID into a `wbs` class object.

# TODO: Need to modify read_flat_file() to import WBSElementID as a wbs (not fct) then we won't need to convert it back to char here.  Also have read_flat_file convert "1.0" to "1".
ff$Data$WBSElementID <- as.character(ff$Data$WBSElementID)

ff$Data <- ff$Data %>%
  dplyr::mutate(WBSElementID = dplyr::case_when(
    WBSElementID == "1.0" ~ "1",
    TRUE ~ WBSElementID
  ))

ff$Data$WBSElementID <- as_wbs(ff$Data$WBSElementID)

################### REVIEW THIS CODE FOR EACH SUBMISSION ####################
# Remove the WBS Element remark(s) about hours or dollars being reported in
# 'thousands' since that data has been replaced/cleaned.

###############################################################################