
#' Read Flat File CSDR Submissions into R
#' 
#' Read legacy Flat File CSDR Submissions into R.
#'
#' @param path Required. Path to the file file CSDR Submission (NOTE: The flat
#'   file submission must be in xlsx format. Open the file in Excel and "Save as
#'   type: Excel Workbook (*.xlsx)").
#'   
#' @return Returns a flat_file list object. The first list object, `MetaData` is
#'   the submission's metadata, the second list object, `Data` includes the
#'   cost, hours, and unit data by WBS, To Date and At Completion, by Recurring
#'   and Nonrecurring, and by Functional Element and Functional Category. The
#'   third list object, `Definitions` comes from the cPet created "Definitions"
#'   worksheet (the source column names are covered to camel case to behave
#'   nicely in R).
#' @export
#'
#' @examples
#' foo <- read_flat_file(here::here("data, "Flat_File.xlsx"))
read_flat_file <- function(path){
  
  # TODO: write or find an xls to xlsx converter and convert the xls file to xlsx on the fly if needed.
  # TODO: write a helper to ensure the provided path is an Excel file and has a "Data" worksheet with in the "flat file" format.
  # TODO: add the ability to import and convert from $k or hours_k to $ or hours on import.
  # TODO: add the ability to convert `MetaData` into FlexFile format.

  flat_file <- new_flat_file()
  
  submission <- tidyxl::xlsx_cells(path)
  
  if (dplyr::filter(submission, sheet == "Data", address == "B1") %>% pull(character) == "1921/1921-1 Input") {
  
  flat_file$Data <- submission %>% 
    dplyr::filter(sheet == "Data",
                  col >= 4) %>% 
    unpivotr::behead(direction = "N", name = "ColumnLabel") %>% 
    unpivotr::behead(direction = "W", name = "WBSElementID") %>% 
    unpivotr::behead(direction = "W", name = "WBSElementName") %>% 
    unpivotr::behead(direction = "E", name = "WBSElementRemarks") %>% 
    dplyr::select(address, 
                  WBSElementID, 
                  WBSElementName, 
                  ColumnLabel,  
                  Value = numeric, 
                  WBSElementRemarks)
  
  flat_file$Data$WBSElementID <- flat_file$Data$WBSElementID %>% forcats::fct_inorder()
  flat_file$Data$WBSElementName <- flat_file$Data$WBSElementName %>% forcats::fct_inorder()
  
  flat_file$Data <-
    dplyr::left_join(flat_file$Data, flat_file$Definitions,
                     by = c("ColumnLabel" = "ColumnLabel")) %>%
    dplyr::select( # Explicit column removal
      -ColumnLetter, 
      -CDSRLocation, 
      -FCHRLocation
    ) %>%
    dplyr::select( # Column reorder
      address,
      WBSElementID,
      WBSElementName,
      UnitOfMeasure,
      Recurring_Nonrecurring,
      ToDate_AtCompletion,
      FunctionalCategory,
      FunctionalElement,
      Value,
      WBSElementRemarks,
      FullName
    ) %>% 
    dplyr::mutate(
      ToDate_AtCompletion = dplyr::case_when(
        ToDate_AtCompletion == "To Date" ~ "ToDate",
        ToDate_AtCompletion == "At Completion" ~ "AtCompletion",
        TRUE ~ NA_character_
      )
    ) 
  
  flat_file$Data$UnitOfMeasure <- flat_file$Data$UnitOfMeasure %>% factor(levels = c("Dollars", "Hours", "Units"))
  flat_file$Data$Recurring_Nonrecurring <- flat_file$Data$Recurring_Nonrecurring %>% factor(levels = c("Nonrecurring", "Recurring"))
  flat_file$Data$ToDate_AtCompletion <- flat_file$Data$ToDate_AtCompletion %>% factor(levels = c("ToDate", "AtCompletion"))
  flat_file$Data$FullName <- flat_file$Data$FullName %>% factor()
  
  # MetaData
  
  flat_file$MetaData <- submission %>% dplyr::filter(sheet == "Data",
                               col %in% c(1, 2)) %>%
    unpivotr::behead(direction = "W", name = "Field") %>%
    dplyr::select(col,
                  data_type,
                  error,
                  logical,
                  numeric,
                  date,
                  character,
                  Field) %>%
    unpivotr::spatter(Field)
  
  flat_file$MetaData <- flat_file$MetaData %>%
    dplyr::select(-col,
                  -error,
                  -logical
    ) %>% 
    dplyr::mutate(
      `Data Type` = as.character(`Data Type`))
  
  flat_file$MetaData <- flat_file$MetaData %>%
    mutate(
      `Data Version` = if (exists("Data Version", where = flat_file$MetaData)) `Data Version` else NA_character_,
      `Security Classification` = if (exists("Security Classification", where = flat_file$MetaData)) `Security Classification` else NA_character_, 
      `1a Program Name` = if (exists("1a Program Name", where = flat_file$MetaData)) `1a Program Name` else NA_character_,
      `1b Program Phase/Milestone` = if (exists("1b Program Phase/Milestone", where = flat_file$MetaData)) `1b Program Phase/Milestone` else NA_character_,
      `2 Prime Mission Product` = if (exists("2 Prime Mission Product", where = flat_file$MetaData)) `2 Prime Mission Product` else NA_character_,
      `3 Reporting Organization Type` = if (exists("3 Reporting Organization Type", where = flat_file$MetaData)) `3 Reporting Organization Type` else NA_character_,
      `4a Organization Address City` = if (exists("4a Organization Address City", where = flat_file$MetaData)) `4a Organization Address City` else NA_character_,
      `4a Organization Address Line 1` = if (exists("4a Organization Address Line 1", where = flat_file$MetaData)) `4a Organization Address Line 1` else NA_character_,
      `4a Organization Address Line 2` = if (exists("4a Organization Address Line 2", where = flat_file$MetaData)) `4a Organization Address Line 2` else NA_character_,
      `4a Organization Address State` = if (exists("4a Organization Address State", where = flat_file$MetaData)) `4a Organization Address State` else NA_character_,
      `4a Organization Address Zip` = if (exists("4a Organization Address Zip", where = flat_file$MetaData)) `4a Organization Address Zip` else NA_character_,
      `4a Organization Name` = if (exists("4a Organization Name", where = flat_file$MetaData)) `4a Organization Name` else NA_character_,
      `4b Division Address City` = if (exists("4b Division Address City", where = flat_file$MetaData)) `4b Division Address City` else NA_character_,
      `4b Division Address Line 1` = if (exists("4b Division Address Line 1", where = flat_file$MetaData)) `4b Division Address Line 1` else NA_character_,
      `4b Division Address Line 2` = if (exists("4b Division Address Line 2", where = flat_file$MetaData)) `4b Division Address Line 2` else NA_character_,
      `4b Division Address State` = if (exists("4b Division Address State", where = flat_file$MetaData)) `4b Division Address State` else NA_character_,
      `4b Division Address Zip` = if (exists("4b Division Address Zip", where = flat_file$MetaData)) `4b Division Address Zip` else NA_character_,
      `4b Division Name` = if (exists("4b Division Name", where = flat_file$MetaData)) `4b Division Name` else NA_character_,
      `5 Approved Plan Number` = if (exists("5 Approved Plan Number", where = flat_file$MetaData)) `5 Approved Plan Number` else NA_character_,
      `6 Customer` = if (exists("6 Customer", where = flat_file$MetaData)) `6 Customer` else NA_character_,
      `7 Contract Type` = if (exists("7 Contract Type", where = flat_file$MetaData)) `7 Contract Type` else NA_character_,
      `8 Contract Price` = if (exists("8 Contract Price", where = flat_file$MetaData)) `8 Contract Price` else NA_real_,
      `9 Contract Ceiling` = if (exists("9 Contract Ceiling", where = flat_file$MetaData)) `9 Contract Ceiling` else NA_real_,
      `10a Contract No` = if (exists("10a Contract No", where = flat_file$MetaData)) `10a Contract No` else NA_character_,
      `10b Latest Modification` =  if (exists("10b Latest Modification", where = flat_file$MetaData)) `10b Latest Modification` else NA_character_,
      `10c Solicitation No` = if (exists("10c Solicitation No", where = flat_file$MetaData)) `10c Solicitation No` else NA_character_,
      `10d Name` = if (exists("10d Name", where = flat_file$MetaData)) `10d Name` else NA_character_,
      `10e Order/Lot No` = if (exists("10e Order/Lot No", where = flat_file$MetaData)) `10e Order/Lot No` else NA_character_,
      `11a PoP Start Date` =  if (exists("11a PoP Start Date", where = flat_file)) lubridate::ymd(`11a PoP Start Date`) else NA_real_,
      `11b PoP End Date` = if (exists("11b PoP End Date", where = flat_file$MetaData)) lubridate::ymd(`11b PoP End Date`)else NA_real_,
      `12 Appropriation` = if (exists("12 Appropriation", where = flat_file$MetaData)) `12 Appropriation` else NA_character_,
      `13 Report Cycle` = if (exists("13 Report Cycle", where = flat_file$MetaData)) `13 Report Cycle` else NA_character_,
      `14 Submission Number` = if (exists("14 Submission Number", where = flat_file$MetaData)) `14 Submission Number` else NA_real_,
      `15 Resubmission Number` = if (exists("15 Resubmission Number", where = flat_file$MetaData)) `15 Resubmission Number` else NA_real_,
      `16 Report As Of` = if (exists("16 Report As Of", where = flat_file$MetaData)) lubridate::ymd(`16 Report As Of`) else NA_real_,
      `17 Name` = if (exists("17 Name", where = flat_file$MetaData)) `17 Name` else NA_character_,
      `18 Department` = if (exists("18 Department", where = flat_file$MetaData)) `18 Department` else NA_character_,
      `19 Telephone Number` = if (exists("19 Telephone Number", where = flat_file$MetaData)) `19 Telephone Number` else NA_character_,
      `20 Email Address` = if (exists("20 Email Address", where = flat_file$MetaData)) `20 Email Address` else NA_character_,
      `21 Date Prepared` = if (exists("21 Date Prepared", where = flat_file$MetaData)) lubridate::ymd(`21 Date Prepared`) else NA_real_,
      `Subtotal TD` = if (exists("Subtotal TD", where = flat_file$MetaData)) `Subtotal TD` else NA_real_,
      `Subtotal AC` = if (exists("Subtotal AC", where = flat_file$MetaData)) `Subtotal AC` else NA_real_,
      `G&A TD` = if (exists("G&A TD", where = flat_file$MetaData)) `G&A TD` else NA_real_,
      `G&A AC` = if (exists("G&A AC", where = flat_file$MetaData)) `G&A AC` else NA_real_,
      `UB AC` = if (exists("UB AC", where = flat_file$MetaData)) `UB AC` else NA_real_,
      `MR AC` = if (exists("MR AC", where = flat_file$MetaData)) `MR AC` else NA_real_,
      `FCCM TD` = if (exists("FCCM TD", where = flat_file$MetaData)) `FCCM TD` else NA_real_,
      `FCCM AC` = if (exists("FCCM AC", where = flat_file$MetaData)) `FCCM AC` else NA_real_,
      `Fee TD` = if (exists("Fee TD", where = flat_file$MetaData)) `Fee TD` else NA_real_,
      `Fee AC` = if (exists("Fee AC", where = flat_file$MetaData)) `Fee AC` else NA_real_,
      `Price TD` = if (exists("Price TD", where = flat_file$MetaData)) `Price TD` else NA_real_,
      `Price AC` = if (exists("Price AC", where = flat_file$MetaData)) `Price AC` else NA_real_,
      `DD 1921 Remarks` = if (exists("DD 1921 Remarks", where = flat_file$MetaData)) `DD 1921 Remarks` else NA_character_
    ) %>% 
    dplyr::select(
      `Data Type`,
      `Data Version`,
      `Security Classification`,
      `1a Program Name`,
      `1b Program Phase/Milestone`,
      `2 Prime Mission Product`,
      `3 Reporting Organization Type`,
      `4a Organization Address City`,
      `4a Organization Address Line 1`,
      `4a Organization Address Line 2`,
      `4a Organization Address State`,
      `4a Organization Address Zip`,
      `4a Organization Name`,
      `4b Division Address City`,
      `4b Division Address Line 1`,
      `4b Division Address Line 2`,
      `4b Division Address State`,
      `4b Division Address Zip`,
      `4b Division Name`,
      `5 Approved Plan Number`,
      `6 Customer`,
      `7 Contract Type`,
      `8 Contract Price`,
      `9 Contract Ceiling`,
      `10a Contract No`,
      `10b Latest Modification`,
      `10c Solicitation No`,
      `10d Name`,
      `10e Order/Lot No`,
      `11a PoP Start Date`,
      `11b PoP End Date`,
      `12 Appropriation`,
      `13 Report Cycle`,
      `14 Submission Number`,
      `15 Resubmission Number`,
      `16 Report As Of`,
      `17 Name`,
      `18 Department`,
      `19 Telephone Number`,
      `20 Email Address`,
      `21 Date Prepared`,
      `Subtotal TD`,
      `Subtotal AC`,
      `G&A TD`,
      `G&A AC`,
      `UB AC`,
      `MR AC`,
      `FCCM TD`,
      `FCCM AC`,
      `Fee TD`,
      `Fee AC`,
      `Price TD`,
      `Price AC`,
      `DD 1921 Remarks`
    )

  return(flat_file)
  }

}
