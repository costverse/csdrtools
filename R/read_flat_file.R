
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
  
  # flat_file$Definitions <- submission %>% 
  #   dplyr::filter(sheet == "Definitions") %>% 
  #   unpivotr::rectify() %>% 
  #   dplyr::select(-`row/col`) %>% 
  #   rlang::set_names(., nm = .[1,]) %>% 
  #   dplyr::slice(-1) %>%
  #   dplyr::mutate_all(str_replace_all, "N/A", NA_character_) %>%
  #   dplyr::rename(
  #     ColumnLetter = `Column Letter`,
  #     ColumnLabel = `Column Label`,
  #     FullName = `Full Name`,
  #     CDSRLocation = `1921 Location`,
  #     FCHRLocation = `1921-1 Location`,
  #     UnitOfMeasure = `Unit of Measure`,
  #     ToDate_AtCompletion = `To Date / \r\nAt Completion`,
  #     Recurring_Nonrecurring = `Recurring / Nonrecurring`,
  #     FunctionalCategory = `Functional Category`,
  #     FunctionalElement = `Functional Element`
  #   )
  
  flat_file$Definitions <-  tibble::tribble(
    ~`Column Letter`,             ~`Column Label`,                                                                      ~`Full Name`, ~`1921 Location`,  ~`1921-1 Location`, ~`Unit of Measure`, ~`To Date / \r\nAt Completion`, ~`Recurring / Nonrecurring`,       ~`Functional Category`,                 ~`Functional Element`,
               "D",                "WBS Code",                                                                      "WBS Code",       "Column A",           "Item 18",            "N/A",                      "N/A",                       "N/A",                      "N/A",                               "N/A",
               "E",  "WBS Reporting Elements",                                                        "WBS Reporting Elements",       "Column B",           "Item 19",            "N/A",                      "N/A",                       "N/A",                      "N/A",                               "N/A",
               "F",                "Units TD",                                                       "Number of Units To Date",       "Column C",          "Item 20a",          "Units",                  "To Date",                 "Recurring",                      "N/A",                               "N/A",
               "G",                 "NR $ TD",                                           "Nonrecurring Costs Incurred To Date",       "Column D", "Line 21, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                      "N/A",                               "N/A",
               "H",                "Rec $ TD",                                              "Recurring Costs Incurred To Date",       "Column E", "Line 21, Column B",        "Dollars",                  "To Date",                 "Recurring",                      "N/A",                               "N/A",
               "I",                "Units AC",                                                 "Number of Units At Completion",       "Column G",          "Item 20b",          "Units",            "At Completion",                 "Recurring",                      "N/A",                               "N/A",
               "J",                 "NR $ AC",                                     "Nonrecurring Costs Incurred At Completion",       "Column H", "Line 21, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                      "N/A",                               "N/A",
               "K",                "Rec $ AC",                                        "Recurring Costs Incurred At Completion",       "Column I", "Line 21, Column E",        "Dollars",            "At Completion",                 "Recurring",                      "N/A",                               "N/A",
               "L",           "NR Eng Hrs TD",                  "Nonrecurring Direct Engineering Labor Hours Incurred To Date",            "N/A",  "Line 1, Column A",          "Hours",                  "To Date",              "Nonrecurring",              "Engineering",          "Direct Engineering Labor",
               "M",          "Rec Eng Hrs TD",                     "Recurring Direct Engineering Labor Hours Incurred To Date",            "N/A",  "Line 1, Column B",          "Hours",                  "To Date",                 "Recurring",              "Engineering",          "Direct Engineering Labor",
               "N",           "NR Eng Hrs AC",            "Nonrecurring Direct Engineering Labor Hours Incurred At Completion",            "N/A",  "Line 1, Column D",          "Hours",            "At Completion",              "Nonrecurring",              "Engineering",          "Direct Engineering Labor",
               "O",          "Rec Eng Hrs AC",               "Recurring Direct Engineering Labor Hours Incurred At Completion",            "N/A",  "Line 1, Column E",          "Hours",            "At Completion",                 "Recurring",              "Engineering",          "Direct Engineering Labor",
               "P",      "NR Direct Eng $ TD",                "Nonrecurring Direct Engineering Labor Dollars Incurred To Date",            "N/A",  "Line 2, Column A",        "Dollars",                  "To Date",              "Nonrecurring",              "Engineering",          "Direct Engineering Labor",
               "Q",     "Rec Direct Eng $ TD",                   "Recurring Direct Engineering Labor Dollars Incurred To Date",            "N/A",  "Line 2, Column B",        "Dollars",                  "To Date",                 "Recurring",              "Engineering",          "Direct Engineering Labor",
               "R",      "NR Direct Eng $ AC",          "Nonrecurring Direct Engineering Labor Dollars Incurred At Completion",            "N/A",  "Line 2, Column D",        "Dollars",            "At Completion",              "Nonrecurring",              "Engineering",          "Direct Engineering Labor",
               "S",     "Rec Direct Eng $ AC",             "Recurring Direct Engineering Labor Dollars Incurred At Completion",            "N/A",  "Line 2, Column E",        "Dollars",            "At Completion",                 "Recurring",              "Engineering",          "Direct Engineering Labor",
               "T",          "NR Eng OH $ TD",                    "Nonrecurring Engineering Overhead Dollars Incurred To Date",            "N/A",  "Line 3, Column A",        "Dollars",                  "To Date",              "Nonrecurring",              "Engineering",              "Engineering Overhead",
               "U",         "Rec Eng OH $ TD",                       "Recurring Engineering Overhead Dollars Incurred To Date",            "N/A",  "Line 3, Column B",        "Dollars",                  "To Date",                 "Recurring",              "Engineering",              "Engineering Overhead",
               "V",          "NR Eng OH $ AC",              "Nonrecurring Engineering Overhead Dollars Incurred At Completion",            "N/A",  "Line 3, Column D",        "Dollars",            "At Completion",              "Nonrecurring",              "Engineering",              "Engineering Overhead",
               "W",         "Rec Eng OH $ AC",                 "Recurring Engineering Overhead Dollars Incurred At Completion",            "N/A",  "Line 3, Column E",        "Dollars",            "At Completion",                 "Recurring",              "Engineering",              "Engineering Overhead",
               "X",          "NR Tool Hrs TD",                      "Nonrecurring Direct Tooling Labor Hours Incurred To Date",            "N/A",  "Line 5, Column A",          "Hours",                  "To Date",              "Nonrecurring",                  "Tooling",              "Direct Tooling Labor",
               "Y",         "Rec Tool Hrs TD",                         "Recurring Direct Tooling Labor Hours Incurred To Date",            "N/A",  "Line 5, Column B",          "Hours",                  "To Date",                 "Recurring",                  "Tooling",              "Direct Tooling Labor",
               "Z",          "NR Tool Hrs AC",                "Nonrecurring Direct Tooling Labor Hours Incurred At Completion",            "N/A",  "Line 5, Column D",          "Hours",            "At Completion",              "Nonrecurring",                  "Tooling",              "Direct Tooling Labor",
              "AA",         "Rec Tool Hrs AC",                   "Recurring Direct Tooling Labor Hours Incurred At Completion",            "N/A",  "Line 5, Column E",          "Hours",            "At Completion",                 "Recurring",                  "Tooling",              "Direct Tooling Labor",
              "AB",     "NR Tool Direct $ TD",                    "Nonrecurring Direct Tooling Labor Dollars Incurred To Date",            "N/A",  "Line 6, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                  "Tooling",              "Direct Tooling Labor",
              "AC",    "Rec Tool Direct $ TD",                       "Recurring Direct Tooling Labor Dollars Incurred To Date",            "N/A",  "Line 6, Column B",        "Dollars",                  "To Date",                 "Recurring",                  "Tooling",              "Direct Tooling Labor",
              "AD",     "NR Tool Direct $ AC",              "Nonrecurring Direct Tooling Labor Dollars Incurred At Completion",            "N/A",  "Line 6, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                  "Tooling",              "Direct Tooling Labor",
              "AE",    "Rec Tool Direct $ AC",                 "Recurring Direct Tooling Labor Dollars Incurred At Completion",            "N/A",  "Line 6, Column E",        "Dollars",            "At Completion",                 "Recurring",                  "Tooling",              "Direct Tooling Labor",
              "AF",      "NR Tool/Equip $ TD",              "Nonrecurring Direct Tooling & Equipment Dollars Incurred To Date",            "N/A",  "Line 7, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                  "Tooling",        "Direct Tooling & Equipment",
              "AG",     "Rec Tool/Equip $ TD",                 "Recurring Direct Tooling & Equipment Dollars Incurred To Date",            "N/A",  "Line 7, Column B",        "Dollars",                  "To Date",                 "Recurring",                  "Tooling",        "Direct Tooling & Equipment",
              "AH",      "NR Tool/Equip $ AC",        "Nonrecurring Direct Tooling & Equipment Dollars Incurred At Completion",            "N/A",  "Line 7, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                  "Tooling",        "Direct Tooling & Equipment",
              "AI",     "Rec Tool/Equip $ AC",           "Recurring Direct Tooling & Equipment Dollars Incurred At Completion",            "N/A",  "Line 7, Column E",        "Dollars",            "At Completion",                 "Recurring",                  "Tooling",        "Direct Tooling & Equipment",
              "AJ",            "NR QC Hrs TD",              "Nonrecurring Direct Quality Control Labor Hours Incurred To Date",            "N/A",  "Line 8, Column A",          "Hours",                  "To Date",              "Nonrecurring",          "Quality Control",      "Direct Quality Control Labor",
              "AK",           "Rec QC Hrs TD",                 "Recurring Direct Quality Control Labor Hours Incurred To Date",            "N/A",  "Line 8, Column B",          "Hours",                  "To Date",                 "Recurring",          "Quality Control",      "Direct Quality Control Labor",
              "AL",            "NR QC Hrs AC",        "Nonrecurring Direct Quality Control Labor Hours Incurred At Completion",            "N/A",  "Line 8, Column D",          "Hours",            "At Completion",              "Nonrecurring",          "Quality Control",      "Direct Quality Control Labor",
              "AM",           "Rec QC Hrs AC",           "Recurring Direct Quality Control Labor Hours Incurred At Completion",            "N/A",  "Line 8, Column E",          "Hours",            "At Completion",                 "Recurring",          "Quality Control",      "Direct Quality Control Labor",
              "AN",       "NR QC Direct $ TD",            "Nonrecurring Direct Quality Control Labor Dollars Incurred To Date",            "N/A",  "Line 9, Column A",        "Dollars",                  "To Date",              "Nonrecurring",          "Quality Control",      "Direct Quality Control Labor",
              "AO",      "Rec QC Direct $ TD",               "Recurring Direct Quality Control Labor Dollars Incurred To Date",            "N/A",  "Line 9, Column B",        "Dollars",                  "To Date",                 "Recurring",          "Quality Control",      "Direct Quality Control Labor",
              "AP",       "NR QC Direct $ AC",      "Nonrecurring Direct Quality Control Labor Dollars Incurred At Completion",            "N/A",  "Line 9, Column D",        "Dollars",            "At Completion",              "Nonrecurring",          "Quality Control",      "Direct Quality Control Labor",
              "AQ",      "Rec QC Direct $ AC",         "Recurring Direct Quality Control Labor Dollars Incurred At Completion",            "N/A",  "Line 9, Column E",        "Dollars",            "At Completion",                 "Recurring",          "Quality Control",      "Direct Quality Control Labor",
              "AR",           "NR MFG Hrs TD",                "Nonrecurring Direct Manufacturing Labor Hours Incurred To Date",            "N/A", "Line 10, Column A",          "Hours",                  "To Date",              "Nonrecurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AS",          "Rec MFG Hrs TD",                   "Recurring Direct Manufacturing Labor Hours Incurred To Date",            "N/A", "Line 10, Column B",          "Hours",                  "To Date",                 "Recurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AT",           "NR MFG Hrs AC",          "Nonrecurring Direct Manufacturing Labor Hours Incurred At Completion",            "N/A", "Line 10, Column D",          "Hours",            "At Completion",              "Nonrecurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AU",          "Rec MFG Hrs AC",             "Recurring Direct Manufacturing Labor Hours Incurred At Completion",            "N/A", "Line 10, Column E",          "Hours",            "At Completion",                 "Recurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AV",      "NR MFG Direct $ TD",              "Nonrecurring Direct Manufacturing Labor Dollars Incurred To Date",            "N/A", "Line 11, Column A",        "Dollars",                  "To Date",              "Nonrecurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AW",     "Rec MFG Direct $ TD",                 "Recurring Direct Manufacturing Labor Dollars Incurred To Date",            "N/A", "Line 11, Column B",        "Dollars",                  "To Date",                 "Recurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AX",      "NR MFG Direct $ AC",        "Nonrecurring Direct Manufacturing Labor Dollars Incurred At Completion",            "N/A", "Line 11, Column D",        "Dollars",            "At Completion",              "Nonrecurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AY",     "Rec MFG Direct $ AC",           "Recurring Direct Manufacturing Labor Dollars Incurred At Completion",            "N/A", "Line 11, Column E",        "Dollars",            "At Completion",                 "Recurring",            "Manufacturing",        "Direct Manufacturing Labor",
              "AZ",      "NR MFG Ops OH $ TD",       "Nonrecurring Manufacturing Operations Overhead Dollars Incurred To Date",            "N/A", "Line 12, Column A",        "Dollars",                  "To Date",              "Nonrecurring", "Manufacturing Operations", "Manufacturing Operations Overhead",
              "BA",     "Rec MFG Ops OH $ TD",          "Recurring Manufacturing Operations Overhead Dollars Incurred To Date",            "N/A", "Line 12, Column B",        "Dollars",                  "To Date",                 "Recurring", "Manufacturing Operations", "Manufacturing Operations Overhead",
              "BB",      "NR MFG Ops OH $ AC", "Nonrecurring Manufacturing Operations Overhead Dollars Incurred At Completion",            "N/A", "Line 12, Column D",        "Dollars",            "At Completion",              "Nonrecurring", "Manufacturing Operations", "Manufacturing Operations Overhead",
              "BC",     "Rec MFG Ops OH $ AC",    "Recurring Manufacturing Operations Overhead Dollars Incurred At Completion",            "N/A", "Line 12, Column E",        "Dollars",            "At Completion",                 "Recurring", "Manufacturing Operations", "Manufacturing Operations Overhead",
              "BD",         "NR Raw Mat $ TD",                            "Nonrecurring Raw Material Dollars Incurred To Date",            "N/A", "Line 14, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                 "Material",                      "Raw Material",
              "BE",        "Rec Raw Mat $ TD",                               "Recurring Raw Material Dollars Incurred To Date",            "N/A", "Line 14, Column B",        "Dollars",                  "To Date",                 "Recurring",                 "Material",                      "Raw Material",
              "BF",         "NR Raw Mat $ AC",                      "Nonrecurring Raw Material Dollars Incurred At Completion",            "N/A", "Line 14, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                 "Material",                      "Raw Material",
              "BG",        "Rec Raw Mat $ AC",                         "Recurring Raw Material Dollars Incurred At Completion",            "N/A", "Line 14, Column E",        "Dollars",            "At Completion",                 "Recurring",                 "Material",                      "Raw Material",
              "BH",     "NR Purch Parts $ TD",                         "Nonrecurring Purchased Parts Dollars Incurred To Date",            "N/A", "Line 15, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                 "Material",                   "Purchased Parts",
              "BI",    "Rec Purch Parts $ TD",                            "Recurring Purchased Parts Dollars Incurred To Date",            "N/A", "Line 15, Column B",        "Dollars",                  "To Date",                 "Recurring",                 "Material",                   "Purchased Parts",
              "BJ",     "NR Purch Parts $ AC",                   "Nonrecurring Purchased Parts Dollars Incurred At Completion",            "N/A", "Line 15, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                 "Material",                   "Purchased Parts",
              "BK",    "Rec Purch Parts $ AC",                      "Recurring Purchased Parts Dollars Incurred At Completion",            "N/A", "Line 15, Column E",        "Dollars",            "At Completion",                 "Recurring",                 "Material",                   "Purchased Parts",
              "BL",     "NR Purch Equip $ TD",                     "Nonrecurring Purchased Equipment Dollars Incurred To Date",            "N/A", "Line 16, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                 "Material",               "Purchased Equipment",
              "BM",    "Rec Purch Equip $ TD",                        "Recurring Purchased Equipment Dollars Incurred To Date",            "N/A", "Line 16, Column B",        "Dollars",                  "To Date",                 "Recurring",                 "Material",               "Purchased Equipment",
              "BN",     "NR Purch Equip $ AC",               "Nonrecurring Purchased Equipment Dollars Incurred At Completion",            "N/A", "Line 16, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                 "Material",               "Purchased Equipment",
              "BO",    "Rec Purch Equip $ AC",                  "Recurring Purchased Equipment Dollars Incurred At Completion",            "N/A", "Line 16, Column E",        "Dollars",            "At Completion",                 "Recurring",                 "Material",               "Purchased Equipment",
              "BP",          "NR Mat OH $ TD",              "Nonrecurring Material Handling/Overhead Dollars Incurred To Date",            "N/A", "Line 17, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                 "Material",        "Material Handling/Overhead",
              "BQ",         "Rec Mat OH $ TD",                 "Recurring Material Handling/Overhead Dollars Incurred To Date",            "N/A", "Line 17, Column B",        "Dollars",                  "To Date",                 "Recurring",                 "Material",        "Material Handling/Overhead",
              "BR",          "NR Mat OH $ AC",        "Nonrecurring Material Handling/Overhead Dollars Incurred At Completion",            "N/A", "Line 17, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                 "Material",        "Material Handling/Overhead",
              "BS",         "Rec Mat OH $ AC",           "Recurring Material Handling/Overhead Dollars Incurred At Completion",            "N/A", "Line 17, Column E",        "Dollars",            "At Completion",                 "Recurring",                 "Material",        "Material Handling/Overhead",
              "BT",  "NR Direct Rep Sub $ TD",          "Nonrecurring Direct-Reporting Subcontractor Dollars Incurred To Date",            "N/A", "Line 18, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                 "Material",    "Direct-Reporting Subcontractor",
              "BU", "Rec Direct Rep Sub $ TD",             "Recurring Direct-Reporting Subcontractor Dollars Incurred To Date",            "N/A", "Line 18, Column B",        "Dollars",                  "To Date",                 "Recurring",                 "Material",    "Direct-Reporting Subcontractor",
              "BV",  "NR Direct Rep Sub $ AC",    "Nonrecurring Direct-Reporting Subcontractor Dollars Incurred At Completion",            "N/A", "Line 18, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                 "Material",    "Direct-Reporting Subcontractor",
              "BW", "Rec Direct Rep Sub $ AC",       "Recurring Direct-Reporting Subcontractor Dollars Incurred At Completion",            "N/A", "Line 18, Column E",        "Dollars",            "At Completion",                 "Recurring",                 "Material",    "Direct-Reporting Subcontractor",
              "BX",           "NR Other $ TD",                 "Nonrecurring Other Costs Not Shown Elsewhere Incurred To Date",            "N/A", "Line 20, Column A",        "Dollars",                  "To Date",              "Nonrecurring",                    "Other",   "Other Costs Not Shown Elsewhere",
              "BY",          "Rec Other $ TD",                    "Recurring Other Costs Not Shown Elsewhere Incurred To Date",            "N/A", "Line 20, Column B",        "Dollars",                  "To Date",                 "Recurring",                    "Other",   "Other Costs Not Shown Elsewhere",
              "BZ",           "NR Other $ AC",           "Nonrecurring Other Costs Not Shown Elsewhere Incurred At Completion",            "N/A", "Line 20, Column D",        "Dollars",            "At Completion",              "Nonrecurring",                    "Other",   "Other Costs Not Shown Elsewhere",
              "CA",          "Rec Other $ AC",              "Recurring Other Costs Not Shown Elsewhere Incurred At Completion",            "N/A", "Line 20, Column E",        "Dollars",            "At Completion",                 "Recurring",                    "Other",   "Other Costs Not Shown Elsewhere",
              "CB",                 "Remarks",                                                                       "Remarks",            "N/A",           "Item 22",            "N/A",                      "N/A",                       "N/A",                      "N/A",                               "N/A"
    ) %>% dplyr::mutate_all(str_replace_all, "N/A", NA_character_) %>% 
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