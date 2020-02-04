
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
#'   and Nonrecurring, and by Functional Element and Functional Category.
#' @export
#'
#' @examples
#' foo <- read_flat_file(here::here("data, "Flat_File.xlsx"))
read_flat_file <- function(path){

  # TODO: write or find an xls to xlsx converter and convert the xls file to xlsx on the fly if needed.
  
  flat_file <- new_flat_file()
  
  submission <- tidyxl::xlsx_cells(path)

  flat_file$Data <- submission %>% 
    dplyr::filter(sheet == "Data",
                  col >= 4) %>% 
    unpivotr::behead(direction = "N", name = "ColumnLabel") %>% 
    unpivotr::behead(direction = "W", name = "WBSElementID") %>% 
    unpivotr::behead(direction = "W", name = "WBSElementName") %>% 
    unpivotr::behead(direction = "E", name = "WBSElementRemarks") %>% 
    dplyr::filter(is_blank != TRUE) %>% 
    dplyr::select(address, 
                  WBSElementID, 
                  WBSElementName, 
                  ColumnLabel,  
                  Value = numeric, 
                  WBSElementRemarks)
  
  flat_file$Data$WBSElementID <- flat_file$Data$WBSElementID %>% forcats::fct_inorder()
  flat_file$Data$WBSElementName <- flat_file$Data$WBSElementName %>% forcats::fct_inorder()
  
  flat_file$Definitions <- submission %>% 
    dplyr::filter(sheet == "Definitions") %>% 
    unpivotr::rectify() %>% 
    dplyr::select(-`row/col`) %>% 
    rlang::set_names(., nm = .[1,]) %>% 
    dplyr::slice(-1) %>%
    dplyr::mutate_all(str_replace_all, "N/A", NA_character_) %>%
    dplyr::rename(
      ColumnLetter = `Column Letter`,
      ColumnLabel = `Column Label`,
      FullName = `Full Name`,
      CDSRLocation = `1921 Location`,
      FCHRLocation = `1921-1 Location`,
      UnitOfMeasure = `Unit of Measure`,
      ToDate_AtCompletion = `To Date / \r\nAt Completion`,
      Recurring_Nonrecurring = `Recurring / Nonrecurring`,
      FunctionalCategory = `Functional Category`,
      FunctionalElement = `Functional Element`
    )
  
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
  
  flat_file$MetaData <- submission %>% 
    dplyr::filter(sheet == "Data",
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
    unpivotr::spatter(Field) %>%
    dplyr::select(-col,
           -error,
           -logical
           ) %>% 
    dplyr::mutate(
      `Data Type` = as.character(`Data Type`),
      `Data Version` = as.character(`Data Version`),
      `Security Classification` = as.character(`Security Classification`),
      `1a Program Name` = as.character(`1a Program Name`),
      `1b Program Phase/Milestone` = as.character(`1b Program Phase/Milestone`),
      `2 Prime Mission Product` = as.character(`2 Prime Mission Product`),
      `3 Reporting Organization Type` = as.character(`3 Reporting Organization Type`),
      `4a Organization Address City` = as.character(`4a Organization Address City`),
      `4a Organization Address Line 1` = as.character(`4a Organization Address Line 1`),
      `4a Organization Address Line 2` = as.character(`4a Organization Address Line 2`),
      `4a Organization Address State` = as.character(`4a Organization Address State`),
      `4a Organization Address Zip` = as.character(`4a Organization Address Zip`),
      `4a Organization Name` = as.character(`4a Organization Name`),
      `4b Division Address City` = as.character(`4b Division Address City`),
      `4b Division Address Line 1` = as.character(`4b Division Address Line 1`),
      `4b Division Address Line 2` = as.character(`4b Division Address Line 2`),
      `4b Division Address State` = as.character(`4b Division Address State`),
      `4b Division Address Zip` = as.character(`4b Division Address Zip`),
      `4b Division Name` = as.character(`4b Division Name`),
      `5 Approved Plan Number` = as.character(`5 Approved Plan Number`),
      `6 Customer` = as.character(`6 Customer`),
      `7 Contract Type` = as.character(`7 Contract Type`),
      `8 Contract Price` = as.numeric(`8 Contract Price`),
      `9 Contract Ceiling` = as.numeric(`9 Contract Ceiling`),
      `10a Contract No` = as.character(`10a Contract No`),
      `10b Latest Modification` = as.character(`10b Latest Modification`),
      `10c Solicitation No` = as.character(`10c Solicitation No`),
      `10d Name` = as.character(`10d Name`),
      `10e Order/Lot No` = as.character(`10e Order/Lot No`),
      `11a PoP Start Date` =  lubridate::ymd(`11a PoP Start Date`),
      `11b PoP End Date` = lubridate::ymd(`11b PoP End Date`),
      `12 Appropriation` = as.character(`12 Appropriation`),
      `13 Report Cycle` = as.character(`13 Report Cycle`),
      `14 Submission Number` = as.integer(`14 Submission Number`),
      `15 Resubmission Number` = as.integer(`15 Resubmission Number`),
      `16 Report As Of` = lubridate::ymd(`16 Report As Of`),
      `17 Name` = as.character(`17 Name`),
      `18 Department` = as.character(`18 Department`),
      `19 Telephone Number` = as.character(`19 Telephone Number`),
      `20 Email Address` = as.character(`20 Email Address`),
      `21 Date Prepared` = lubridate::ymd(`21 Date Prepared`),
      `Subtotal TD` = as.numeric(`Subtotal TD`),
      `Subtotal AC` = as.numeric(`Subtotal AC`),
      `G&A TD` = as.numeric(`G&A TD`),
      `G&A AC` = as.numeric(`G&A AC`),
      `UB AC` = as.numeric(`UB AC`),
      `MR AC` = as.numeric(`MR AC`),
      `FCCM TD` = as.numeric(`FCCM TD`),
      `FCCM AC` = as.numeric(`FCCM AC`),
      `Fee TD` = as.numeric(`Fee TD`),
      `Fee AC` = as.numeric(`Fee AC`),
      `Price TD` = as.numeric(`Price TD`),
      `Price AC` = as.numeric(`Price AC`),
      `DD 1921 Remarks` = as.character(`DD 1921 Remarks`)
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