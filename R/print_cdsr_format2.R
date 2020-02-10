
library(magrittr)
library(tidyverse)

foo$Data %>% 
  filter(WBSElementID == "1.0") %>% 
  filter(complete.cases(.)) %>%
  filter(UnitOfMeasure %in% c("Dollars", "Hours")) %>% 
  tidyr::pivot_wider(
    id_cols = c(
      WBSElementID, 
      WBSElementName, 
      #FunctionalCategory, 
      FunctionalElement), 
    names_from = c(
      UnitOfMeasure,
      Recurring_Nonrecurring,
      ToDate_AtCompletion
      ),
    values_from = Value,
    values_fill = list(Value = 0)
  ) %>% left_join(., foo$Data %>% 
                    filter(WBSElementID == "1.0") %>% 
                    filter(UnitOfMeasure %in% c("Units")) %>% 
                    pivot_wider(
                      id_cols = c(
                        WBSElementID, 
                        WBSElementName
                      ), 
                      names_from = c(
                        UnitOfMeasure,
                        ToDate_AtCompletion
                      ),
                      values_from = Value,
                      values_fill = list(Value = 0)
                    ), by = c("WBSElementID", "WBSElementName")) %>% 
  group_by(WBSElementID, FunctionalElement) %>% 
  mutate(Hours_Total_ToDate = sum(Hours_Nonrecurring_ToDate, Hours_Recurring_ToDate),
         Hours_Total_AtCompletion = sum(Hours_Nonrecurring_AtCompletion, Hours_Recurring_AtCompletion),
         Dollars_Total_ToDate = sum(Dollars_Nonrecurring_ToDate, Dollars_Recurring_ToDate),
         Dollars_Total_AtCompletion = sum(Dollars_Nonrecurring_AtCompletion, Dollars_Recurring_AtCompletion)) %>% 
  select(WBSElementID,
         WBSElementName,
         #FunctionalCategory,
         FunctionalElement,
         Units_ToDate,
         Units_AtCompletion,
         Hours_Nonrecurring_ToDate,
         Hours_Recurring_ToDate,
         Hours_Total_ToDate,
         Hours_Nonrecurring_AtCompletion,
         Hours_Recurring_AtCompletion,
         Hours_Total_AtCompletion,
         Dollars_Nonrecurring_ToDate,
         Dollars_Recurring_ToDate,
         Dollars_Total_ToDate,
         Dollars_Nonrecurring_AtCompletion,
         Dollars_Recurring_AtCompletion,
         Dollars_Total_AtCompletion
         ) %>% 
  ungroup() %>% 
  # group_by(WBSElementID, WBSElementName) %>%
  # select(-WBSElementID, -WBSElementName) %>% 
  gt::gt(#groupname_col = "FunctionalCategory",
         rowname_col = "FunctionalElement") %>%
  gt::tab_header(title = paste0(.$WBSElementID[1], " - ", .$WBSElementName[1])) %>% 
  gt::tab_stubhead(label = gt::md("*Dollars in TY$  
                                  Hours in Unit Hours*")) %>% 
  gt::cols_hide(columns = dplyr::vars(WBSElementID, WBSElementName)) %>% 
  gt::fmt_currency(columns = dplyr::vars(Dollars_Nonrecurring_ToDate,
                                         Dollars_Recurring_ToDate,
                                         Dollars_Total_ToDate,
                                         Dollars_Nonrecurring_AtCompletion,
                                         Dollars_Recurring_AtCompletion,
                                         Dollars_Total_AtCompletion),
                   currency = "USD", 
                   suffixing = TRUE) %>% 
  gt::fmt_number(columns = dplyr::vars(Hours_Nonrecurring_ToDate,
                                       Hours_Recurring_ToDate,
                                       Hours_Total_ToDate,
                                       Hours_Nonrecurring_AtCompletion,
                                       Hours_Recurring_AtCompletion,
                                       Hours_Total_AtCompletion),
                 decimals = 1,
                 drop_trailing_zeros = TRUE,
                 suffixing = TRUE) %>% 
  gt::tab_spanner(label = "To Date",
                  columns = vars(
                                 Units_ToDate,
                                 Dollars_Nonrecurring_ToDate,
                                 Dollars_Recurring_ToDate,
                                 Dollars_Total_ToDate,
                                 Hours_Nonrecurring_ToDate,
                                 Hours_Recurring_ToDate,
                                 Hours_Total_ToDate)) %>% 
  gt::tab_spanner(label = "At Completion",
                  columns = vars(
                    Units_AtCompletion,
                    Dollars_Nonrecurring_AtCompletion,
                    Dollars_Recurring_AtCompletion,
                    Dollars_Total_AtCompletion,
                    Hours_Nonrecurring_AtCompletion,
                    Hours_Recurring_AtCompletion,
                    Hours_Total_AtCompletion)) %>% 
  gt::cols_label(
    # WBSElementID = "WBS Element",
    # FunctionalCategory = "Functional Category",
    # FunctionalElement = "Functional Element",
    Units_ToDate = "Units",
    Dollars_Nonrecurring_ToDate = "Nonrecurring",
    Dollars_Recurring_ToDate = "Recurring",
    Dollars_Total_ToDate = "Total",
    Hours_Nonrecurring_ToDate = "Nonrecurring",
    Hours_Recurring_ToDate = "Recurring",
    Hours_Total_ToDate = "Total",
    Units_AtCompletion = "Units",
    Dollars_Nonrecurring_AtCompletion = "Nonrecurring",
    Dollars_Recurring_AtCompletion = "Recurring",
    Dollars_Total_AtCompletion = "Total",
    Hours_Nonrecurring_AtCompletion = "Nonrecurring",
    Hours_Recurring_AtCompletion = "Recurring",
    Hours_Total_AtCompletion = "Total"
    ) %>% 
  # gt::summary_rows(
  #   groups = TRUE,
  #   columns = vars(Dollars_Nonrecurring_ToDate,
  #                  Dollars_Recurring_ToDate,
  #                  Dollars_Total_ToDate,
  #                  Dollars_Nonrecurring_AtCompletion,
  #                  Dollars_Recurring_AtCompletion,
  #                  Dollars_Total_AtCompletion
  #                  ),
  #   fns = list(
  #     "Subtotal" = ~sum(.)),
  #   formatter = gt::fmt_currency,
  #   suffixing = TRUE,
  #   use_seps = TRUE
  # ) %>% 
  # gt::summary_rows(
  #   groups = TRUE,
  #   columns = vars(
  #                  Hours_Nonrecurring_ToDate,
  #                  Hours_Recurring_ToDate,
  #                  Hours_Total_ToDate,
  #                  Hours_Nonrecurring_AtCompletion,
  #                  Hours_Recurring_AtCompletion,
  #                  Hours_Total_AtCompletion
  #                  ),
  #   fns = list(
  #     "Subtotal" = ~sum(.)),
  #   decimals = 1,
  #   drop_trailing_zeros = TRUE,
  #   suffixing = TRUE,
  #   use_seps = TRUE
  # ) %>%
  # gt::summary_rows(
  #   groups = TRUE,
  #   columns = vars(
  #     Units_ToDate,
  #     Units_AtCompletion
  #   ),
  #   fns = list(
  #     "Subtotal" = ~mean(.)),
  #   decimals = 1,
  #   drop_trailing_zeros = TRUE,
  #   suffixing = TRUE,
  #   use_seps = TRUE
  # ) %>% 
  gt::grand_summary_rows(
    columns = vars(Dollars_Nonrecurring_ToDate,
                   Dollars_Recurring_ToDate,
                   Dollars_Total_ToDate,
                   Dollars_Nonrecurring_AtCompletion,
                   Dollars_Recurring_AtCompletion,
                   Dollars_Total_AtCompletion
    ),
    fns = list(
      "WBS Total" = ~sum(.)),
    formatter = gt::fmt_currency,
    suffixing = TRUE,
    use_seps = TRUE
  ) %>% 
  gt::grand_summary_rows(
    columns = vars(
      Hours_Nonrecurring_ToDate,
      Hours_Recurring_ToDate,
      Hours_Total_ToDate,
      Hours_Nonrecurring_AtCompletion,
      Hours_Recurring_AtCompletion,
      Hours_Total_AtCompletion
    ),
    fns = list(
      "WBS Total" = ~sum(.)),
    decimals = 1,
    drop_trailing_zeros = TRUE,
    suffixing = TRUE,
    use_seps = TRUE
  ) %>% 
  gt::grand_summary_rows(
    columns = vars(
      Units_ToDate,
      Units_AtCompletion
    ),
    fns = list(
      "WBS Total" = ~mean(.)),
    decimals = 1,
    drop_trailing_zeros = TRUE,
    suffixing = TRUE,
    use_seps = TRUE
  ) %>% 
  #gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_summary(groups = TRUE)) %>%
  gt::tab_style(style = list(gt::cell_text(size = "x-small", align = "center")), locations = gt::cells_stubhead()) %>% 
  gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_grand_summary()) %>%
  gt::tab_style(style = list(gt::cell_text(style = "italic")), locations = gt::cells_data(columns = vars(Units_ToDate, Units_AtCompletion)))

