foo$Data %>% 
  filter(complete.cases(.),
         UnitOfMeasure == "Dollars") %>%
  filter(WBSElementID == "1.1.1.7") %>% 
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
  # group_by(WBSElementID, WBSElementName) %>%
  # select(-WBSElementID, -WBSElementName) %>% 
  gt(groupname_col = "FunctionalCategory",
     rowname_col = "FunctionalElement") %>%
  cols_hide(columns = vars(WBSElementID, WBSElementName)) %>% 
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
  cols_label(
    # WBSElementID = "WBS Element",
    # FunctionalCategory = "Functional Category",
    # FunctionalElement = "Functional Element",
    Nonrecurring_ToDate = "Nonrecurring",
    Recurring_ToDate = "Recurring",
    Total_ToDate = "Total",
    Nonrecurring_AtCompletion = "Nonrecurring",
    Recurring_AtCompletion = "Recurring",
    Total_AtCompletion = "Total") %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(Nonrecurring_ToDate, 
                   Recurring_ToDate, 
                   Total_ToDate,
                   Nonrecurring_AtCompletion,
                   Recurring_AtCompletion,
                   Total_AtCompletion),
    fns = list(
      "Subtotal" = ~sum(.)),
    formatter = fmt_currency,
    suffixing = TRUE,
    use_seps = TRUE
  ) %>% 
  grand_summary_rows(
    columns = vars(Nonrecurring_ToDate, 
                   Recurring_ToDate, 
                   Total_ToDate,
                   Nonrecurring_AtCompletion,
                   Recurring_AtCompletion,
                   Total_AtCompletion),
    fns = list(
      "Total" = ~sum(.)),
    formatter = fmt_currency, suffixing = TRUE) %>%
  tab_header(
    title = md(paste0("**WBS:** ",.$WBSElementID[1], " - *", .$WBSElementName[1],"*  
                      test")))

# %>% 
#   tab_style(locations = cells_data(columns = vars(Total_ToDate, Total_AtCompletion)),
#             style = cell_text(weight = "bold"))

