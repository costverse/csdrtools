
library(tidyverse)
library(gt)

foo <- read_flat_file(here::here("inst", "extdata", "Flat_File.xlsx"))

foo <- read_flat_file(here::here("inst", "extdata", "Flat_File_mostly_blank.xlsx"))

foo <- read_flat_file(here::here("inst", "extdata", "Flat_File_empty.xlsx"))

pivot_flat_file <- function(flat_file, UnitOfMeasure, names_from) {
  
  if (!(UnitOfMeasure %in% c("Dollars", "Hours", "Units"))) {
    stop(paste0("Error: `UnitOfMeasure` value provided is either blank or is a non-allowed value.
         Use one of the following:
         * UnitOfMeasure = \"Dollars\"
         * UnitOfMeasure = \"Hours\"
         * UnitOfMeasure = \"Units\""))
  }
 
  # if (names_from == "Recurring_Nonrecurring")
  # { 
  #   flat_file$Data %>% 
  #     filter(UnitOfMeasure == UnitOfMeasure) %>%
  #     pivot_wider(
  #       names_from = {{ names_from }}, 
  #       values_from = Value,
  #       values_fn = list(Value = sum),
  #       id_cols = everything()) %>%
  #     group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  #     summarise(Recurring = sum(Recurring), Nonrecurring = sum(Nonrecurring)) %>% 
  #     mutate(Total = Recurring + Nonrecurring) %>% return(.)
  # }
  # 
  # if (names_from == "ToDate_AtCompletion")
  # {
  #   flat_file$Data %>% 
  #     filter(UnitOfMeasure == {{ UnitOfMeasure }}) %>%
  #     select(-FullName, -address) %>% 
  #     pivot_wider(
  #       names_from = {{ names_from }}, 
  #       values_from = Value,
  #       values_fn = list(Value = sum),
  #       id_cols = everything()) %>% 
  #     group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  #     summarise(ToDate = sum(ToDate), AtCompletion = sum(AtCompletion))
  # }
  # 
  # if (names_from == "FunctionalCategory")
  # {
  #   flat_file$Data <- flat_file$Data %>% select(-FunctionalElement) %>% filter(!is.na(FunctionalCategory))
  # }
  # 
  # if (names_from == "FunctionalElement")
  # {
  #   flat_file$Data <- flat_file$Data %>% select(-FunctionalCategory) %>% filter(!is.na(FunctionalElement))
  # }
  
  flat_file$Data %>%
    filter(UnitOfMeasure == {{ UnitOfMeasure }}) %>%
    select(-FullName, -address) %>%
    pivot_wider(
      names_from = {{ names_from }},
      values_from = Value,
      values_fn = list(Value = sum),
      id_cols = everything())
}

foo$Data %>%  
  filter(UnitOfMeasure == "Dollars") %>% 
  pivot_wider(
    names_from = "Recurring_Nonrecurring", 
    values_from = Value,
    values_fn = list(Value = sum),
    id_cols = c(WBSElementID, WBSElementName))

foo %>% pivot_flat_file("Dollars", "ToDate_AtCompletion")

foo %>% pivot_flat_file("Dollars", "Recurring_Nonrecurring")

foo %>% pivot_flat_file("Dollars", "FunctionalElement")


foo %>% pivot_flat_file("Dollars", "ToDate_AtCompletion") %>% 
  filter(!is.na(FunctionalElement)) %>% 
  group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  summarise(ToDate = sum(ToDate), AtCompletion = sum(AtCompletion))

foo %>% pivot_flat_file("Dollars", "Recurring_Nonrecurring") %>% 
  filter(!is.na(FunctionalElement)) %>% 
  group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  summarise(Recurring = sum(Recurring), Nonrecurring = sum(Nonrecurring)) %>% 
  mutate(Total = Recurring + Nonrecurring)

foo %>% pivot_flat_file("Dollars", "FunctionalElement") %>% 
  group_by(WBSElementID, WBSElementName) %>% 
  summarise(
    `Direct Engineering Labor` = sum(`Direct Engineering Labor`),
    `Engineering Overhead` = sum(`Engineering Overhead`),
    `Direct Tooling Labor` = sum(`Direct Tooling Labor`),
    `Direct Tooling & Equipment` = sum(`Direct Tooling & Equipment`),
    `Direct Quality Control Labor` = sum(`Direct Quality Control Labor`),
    `Direct Manufacturing Labor` = sum(`Direct Manufacturing Labor`),
    `Manufacturing Operations Overhead` = sum(`Manufacturing Operations Overhead`),
    `Raw Material` = sum(`Raw Material`),
    `Purchased Parts` = sum(`Purchased Parts`),
    `Purchased Equipment` = sum(`Material Handling/Overhead`),
    `Material Handling/Overhead` = sum(`Material Handling/Overhead`),
    `Direct-Reporting Subcontractor` = sum(`Direct-Reporting Subcontractor`),
    `Other Costs Not Shown Elsewhere` = sum(`Other Costs Not Shown Elsewhere`))

foo %>% pivot_flat_file("Dollars", "FunctionalCategory") %>%
  group_by(WBSElementID, WBSElementName) %>% 
  summarise_if(is.numeric, sum, rm.na = TRUE)

# Recurring_Nonrecurring Dollars
pivot_rnr <- foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>% 
  pivot_wider(id_cols = c(WBSElementID, WBSElementName, WBSElementRemarks),
              values_from = Value, 
              names_from = Recurring_Nonrecurring, 
              values_fn = list(Value = sum),
              values_fill = list(Nonrecurring = 0, Recurring = 0)) %>%
  group_by(WBSElementID) %>% 
  mutate(Total = sum(Nonrecurring, Recurring))

pivot_rnr %>% 
  ungroup() %>% 
  gt() %>% 
  gt::fmt_currency(columns = vars(Nonrecurring, Recurring, Total), 
                   currency = "USD", 
                   suffixing = TRUE) %>% 
  cols_merge(col_1 = vars(WBSElementID),
             col_2 = vars(WBSElementName),
             pattern = "{1} - {2}") %>% 
  cols_label(WBSElementID = "WBS Element") %>% 
  cols_align(align = "left",
             columns = vars(WBSElementID)) %>% 
  cols_align(align = "right",
             columns = vars(Nonrecurring, Recurring, Total))

# ToDate_AtCompletion Dollars
pivot_tdac <- foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>% 
  pivot_wider(id_cols = c(WBSElementID, WBSElementName, WBSElementRemarks),
              values_from = Value, 
              names_from = ToDate_AtCompletion, 
              values_fn = list(Value = sum),
              values_fill = list(ToDate = 0, AtCompletion = 0))

# FunctionalCategory Dollars
pivot_fc <- foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>%
  select(-FunctionalElement) %>% 
  pivot_wider(id_cols = c(WBSElementID, WBSElementName, WBSElementRemarks),
              values_from = Value, 
              names_from = FunctionalCategory, 
              values_fn = list(Value = sum),
              values_fill = list(Engineering = 0,
                                 Tooling = 0,
                                 `Quality Control` = 0,
                                 Manufacturing = 0,
                                 `Manufacturing Operations` = 0,
                                 Material = 0,
                                 Other = 0)) %>% 
  group_by(WBSElementID) %>% 
  mutate(Total = sum(Engineering, Tooling, `Quality Control`, Manufacturing, `Manufacturing Operations`, Material, Other))

# FunctionalElement Dollars
pivot_fe <- foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>%
  select(-FunctionalCategory) %>% 
  pivot_wider(id_cols = c(WBSElementID, WBSElementName, WBSElementRemarks),
              values_from = Value, 
              names_from = FunctionalElement, 
              values_fn = list(Value = sum),
              values_fill = list(
                `Direct Engineering Labor` = 0,
                `Engineering Overhead` = 0,
                `Direct Tooling Labor` = 0,
                `Direct Tooling & Equipment` = 0,
                `Direct Quality Control Labor` = 0,
                `Direct Manufacturing Labor` = 0,
                `Manufacturing Operations Overhead` = 0,
                `Raw Material` = 0,
                `Purchased Parts` = 0,
                `Purchased Equipment` = 0,
                `Material Handling/Overhead` = 0,
                `Direct-Reporting Subcontractor` = 0,
                `Other Costs Not Shown Elsewhere` = 0
              )) %>% 
  group_by(WBSElementID) %>% 
  mutate(Total = sum(
    `Direct Engineering Labor`,
    `Engineering Overhead`,
    `Direct Tooling Labor`,
    `Direct Tooling & Equipment`,
    `Direct Quality Control Labor`,
    `Direct Manufacturing Labor`,
    `Manufacturing Operations Overhead`,
    `Raw Material`,
    `Purchased Parts`,
    `Purchased Equipment`,
    `Material Handling/Overhead`,
    `Direct-Reporting Subcontractor`,
    `Other Costs Not Shown Elsewhere`
  ))

foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>% 
  pivot_wider(id_cols = c(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement), names_from = c(Recurring_Nonrecurring, ToDate_AtCompletion, ),
              values_from = Value,
              values_fn = list(Value = sum),
              values_fill = list(
                Nonrecurring_ToDate = 0,
                Recurring_ToDate = 0,
                Nonrecurring_AtCompletion = 0,
                Recurring_AtCompletion = 0
              )) %>% 
  group_by(WBSElementID, FunctionalElement) %>% 
  mutate(Total_ToDate = sum(Nonrecurring_ToDate, Recurring_ToDate),
         Total_AtCompletion = sum(Nonrecurring_AtCompletion, Recurring_AtCompletion)) %>% 
  select(WBSElementID,
         WBSElementName,
         FunctionalCategory,
         FunctionalElement,
         Nonrecurring_ToDate,
         Recurring_ToDate,
         Total_ToDate,
         Nonrecurring_AtCompletion,
         Recurring_AtCompletion,
         Total_AtCompletion) %>% 
  ungroup() %>% 
  mutate(col_name = paste0(WBSElementID, " - ", WBSElementName)) %>% 
  select(col_name, everything(), -c(WBSElementID, WBSElementName)) %>% 
  #group_by(WBSElementID, WBSElementName) %>%
  select(-FunctionalCategory) %>% 
  gt(groupname_col = "col_name", rowname_col = "FunctionalElement") %>% 
  fmt_currency(columns = vars(Nonrecurring_ToDate,
                              Recurring_ToDate,
                              Total_ToDate,
                              Nonrecurring_AtCompletion,
                              Recurring_AtCompletion,
                              Total_AtCompletion), 
               currency = "USD", 
               suffixing = TRUE) %>% 
  # cols_merge(col_1 = vars(WBSElementID),
  #            col_2 = vars(WBSElementName),
  #            pattern = "{1} - {2}") %>% 
  gt::tab_spanner(label = "To Date",
                  columns = vars(Nonrecurring_ToDate,
                                 Recurring_ToDate,
                                 Total_ToDate)) %>% 
  gt::tab_spanner(label = "At Completion",
                  columns = vars(Nonrecurring_AtCompletion,
                                 Recurring_AtCompletion,
                                 Total_AtCompletion)) %>% 
  tab_style(locations = cells_data(columns = vars(Total_ToDate, Total_AtCompletion)),
            style = cell_text(weight = "bold")) %>% 
  cols_label(
    # WBSElementID = "WBS Element",
    # FunctionalCategory = "Functional Category",
    # FunctionalElement = "Functional Element",
    Nonrecurring_ToDate = "Nonrecurring",
    Recurring_ToDate = "Recurring",
    Total_ToDate = "Total",
    Nonrecurring_AtCompletion = "Nonrecurring",
    Recurring_AtCompletion = "Recurring",
    Total_AtCompletion = "Total")

