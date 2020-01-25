library(magrittr)

#creates a master flatfile from the flexfile tables
#joins ids to names (Account, Functional Category, OrderOrLot, etc.)
#stacks forecasts and actuals


## Function ----
flatten_ff <- function(flexfile) {
  
  actuals <- flexfile$actual_cost_hour_data %>%
    dplyr::left_join(dplyr::select(flexfile$accounts, c(id, name, ff_id)),
                     by = c("account_id" = "id",
                            "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$clins, c(id, name, ff_id)),
                     by = c("clin_id" = "id",
                            "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$wbs, c(level, id, name, parent_id, ff_id)),
              by = c("wbs_element_id" = "id",
                     "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$functional_categories, c(id, name, ff_id)),
                     by = c("functional_category_id" = "id",
                            "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$functional_overhead_categories, c(id, name, ff_id)),
                     by = c("functional_overhead_category_id" = "id",
                            "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$reporting_calendar,c(id, start_date, end_date, ff_id)),
                     by = c("reporting_period_id" = "id",
                            "ff_id" = "ff_id")) %>%
    dplyr::mutate(start_date = as.Date(start_date)) %>%
    dplyr::mutate(end_date = as.Date(end_date)) %>%
    dplyr::mutate(account_id = NULL) %>%
    dplyr::mutate(clin_id = NULL) %>%
    dplyr::mutate(functional_category_id = NULL) %>%
    dplyr::mutate(functional_overhead_category_id = NULL) %>%
    dplyr::mutate(reporting_period_id = NULL) %>%
    dplyr::mutate(atd_or_fac = "ATD") %>%
    dplyr::rename(account_name = name.x) %>%
    dplyr::rename(clin_name = name.y) %>%
    dplyr::rename(parent_wbs = parent_id) %>%
    dplyr::rename(wbs_name = name.x.x) %>%
    dplyr::rename(wbs_level = level) %>%
    dplyr::rename(functional_category_name = name.y.y) %>%
    dplyr::rename(functional_overhead_category_name = name)
  
  forecasts <- flexfile$forecast_at_completion_cost_hour_data %>%
    dplyr::left_join(dplyr::select(flexfile$wbs, c(level, id, name, parent_id, ff_id)),
                     by = c("wbs_element_id" = "id", "ff_id" = "ff_id")) %>%
    dplyr::left_join(dplyr::select(flexfile$orders_or_lots, c(id, name, phase_or_milestone_id, contract_type_id, ff_id)),
                     by = c("order_or_lot_id" = "id", "ff_id" = "ff_id")) %>%
    dplyr::rename(parent_wbs = parent_id) %>%
    dplyr::rename(wbs_name = name.x) %>%
    dplyr::rename(wbs_level = level) %>%
    dplyr::rename(order_or_lot_name = name.y) %>%
    dplyr::mutate(atd_or_fac = "FAC") %>%
    dplyr::mutate(order_or_lot_id = NULL)
  
  actuals_forecasts <- dplyr::bind_rows(actuals, forecasts)
  
  actuals_forecasts <- actuals_forecasts %>%
    dplyr::select(value_dollars, value_hours, dplyr::everything()) %>%
    dplyr::select(3:ncol(actuals_forecasts), value_dollars, value_hours)

  
}

#Examples
flattened_ff <- flatten_ff(ff_cleansed_one)
flattened_ffs <- flatten_ff(ff_cleansed_two)
