
library(tidyverse)

foo <- read_flat_file(here::here("inst", "extdata", "Flat_File.xlsx"))

pivot_flat_file <- function(flat_file, UnitOfMeasure, names_from) {
  
  flat_file$Data %>% 
    filter(!is.na(FunctionalCategory)) %>% 
    filter(UnitOfMeasure == {{ UnitOfMeasure }}) %>%
    select(-FullName, -address) %>% 
    pivot_wider(
      names_from = {{ names_from }}, 
      values_from = Value,
      values_fn = list(Value = sum),
      id_cols = everything())
}


foo$Data %>% glimpse()

foo %>% pivot_flat_file("Dollars", ToDate_AtCompletion) %>% 
  filter(!is.na(FunctionalElement)) %>% 
  group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  summarise(ToDate = sum(ToDate), AtCompletion = sum(AtCompletion))

foo %>% pivot_flat_file("Dollars", Recurring_Nonrecurring) %>% 
  filter(!is.na(FunctionalElement)) %>% 
  group_by(WBSElementID, WBSElementName, FunctionalCategory, FunctionalElement) %>% 
  summarise(Recurring = sum(Recurring), Nonrecurring = sum(Nonrecurring)) %>% 
  mutate(Total = Recurring + Nonrecurring)

foo %>% pivot_flat_file("Dollars", FunctionalElement) %>% 
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


foo %>% pivot_flat_file("Dollars", FunctionalElement) %>%
  select(-FunctionalCategory) %>% 
  group_by(WBSElementID, WBSElementName) %>% 
  summarise_if(is.numeric, sum, rm.na = TRUE)



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


expand.grid(
  c("Nonrecurring", "Recurring", "Total"),
  c("ToDate", "AtCompletion"),
  c("Total",
    "Direct Engineering Labor", 
    "Engineering Overhead",
    "Direct Tooling Labor",
    "Direct Tooling & Equipment",
    "Direct Quality Control Labor",
    "Direct Manufacturing Labor",
    "Manufacturing Operations Overhead",
    "Raw Material",
    "Purchased Parts",
    "Purchased Equipment",
    "Material Handling/Overhead",
    "Direct-Reporting Subcontractor",
    "Other Costs Not Shown Elsewhere"
  ))

foo$Definitions %>% filter(!is.na(FunctionalElement)) %>% distinct(FunctionalCategory, FunctionalElement)
