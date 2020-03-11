
# Provide the file name (with extension) of the CSDR WBS Dictionary.
dictionary_source_name <- "fake_dictionary.xlsx"

# Provide the title of the worksheet the CSDR WBS Dictionary is found on.
dictionary_worksheet_title <- "Sheet1"

# Provide the path to the CSDR WBS Dictionary (do not include the file name
# or extension).
dictionary_source_path <- here::here("inst", "extdata")

# Import the CSDR WBS Dictionary.  Keep an unmodified copy in
# `dictionary_source`,
dictionary_source <- tidyxl::xlsx_cells(
  path = paste0(dictionary_source_path, "/", dictionary_source_name),
  sheets = dictionary_worksheet_title
)
dictionary <- dictionary_source

#################### REVIEW THIS CODE FOR EACH SUBMISSION ####################
# Clean and munge the CSDR WBS Dictionary.
# TODO: Provide more description on how to munge a dictionary so that it can be joined with the Flat File.

dictionary <- dictionary %>% 
  rectify() %>% 
  select(-`row/col`) %>%
  rlang::set_names(., nm = .[1, ]) %>%
  slice(-1) %>%
  janitor::clean_names()

# Rename the source data columns with what the rest of the script is expecting.
dictionary <- dictionary %>% 
  rename("WBSElementID" = wbs_code,
         "WBSElementName" = wbs_reporting_elements,
         "WBSDictionaryDefinitions" = cwbs_definition)

dictionary$WBSElementName <- dictionary$WBSElementName %>% str_trim()

# Convert the WBSElementID to the `wbs` class object.
dictionary$WBSElementID <- as_wbs(dictionary$WBSElementID)