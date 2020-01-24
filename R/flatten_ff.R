library(magrittr)

#creates a master flatfile from the flexfile tables
#joins ids to names (Account, Functional Category, OrderOrLot, etc.)
#stacks forecasts and actuals

flatten_ff <- function(flexfile) {
  
  #if number of FlexFiles > 1, must also join on ff_id
  if ("ff_id" %in% colnames(flexfile$actual_cost_hour_data)) {
    
    #Aligning ids from ActualCostHourData table to names from lookup tables
    flat_ff <- dplyr::left_join(flexfile$actual_cost_hour_data, 
                                dplyr::select(flexfile$accounts, c(id, name, ff_id)), 
                                by = c("account_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(account_name = name)
    
    flat_ff$account_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$clins, c(id, name, ff_id)),
                                by = c("clin_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(clin_name = name)
    
    flat_ff$clin_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$wbs, c(level, id, name, parent_id, ff_id)),
                                by = c("wbs_element_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(parent_wbs = parent_id) %>%
      dplyr::rename(wbs_name = name) %>%
      dplyr::rename(wbs_level = level)
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$functional_categories, c(id, name, ff_id)),
                                by = c("functional_category_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(functional_category_name = name)
    
    flat_ff$functional_category_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$functional_overhead_categories, c(id, name, ff_id)),
                                by = c("functional_overhead_category_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(functional_overhead_category_name = name)
    
    flat_ff$functional_overhead_category_name <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$reporting_calendar, c(id, start_date, end_date, ff_id)),
                                by = c("reporting_period_id" = "id", "ff_id" = "ff_id"))
    
    flat_ff$start_date <- as.Date(flat_ff$start_date)
    flat_ff$end_date <- as.Date(flat_ff$end_date)
    flat_ff$reporting_period_id <- NULL
    flat_ff$ATD_or_FAC <- "ATD"
    
    #Aligning ids from ForecastAtCompletion table to names from lookup tables
    
    flat_ff_forecast <- dplyr::left_join(flexfile$forecast_at_completion_cost_hour_data,
                                         dplyr::select(flexfile$wbs, c(level, id, name, parent_id, ff_id)),
                                         by = c("wbs_element_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(parent_wbs = parent_id) %>%
      dplyr::rename(wbs_name = name) %>%
      dplyr::rename(wbs_level = level)
    
    flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                                         dplyr::select(flexfile$orders_or_lots, c(id, name, phase_or_milestone_id, contract_type_id, ff_id)),
                                         by = c("order_or_lot_id" = "id", "ff_id" = "ff_id")) %>%
      dplyr::rename(order_or_lot_name = name)
    
    flat_ff_forecast$ATD_or_FAC <- "FAC"
    flat_ff_forecast$order_or_lot_id <- NULL
    
    #bind actuals and forecasts
    flat_ff_2 <- dplyr::bind_rows(flat_ff, flat_ff_forecast)
    
    #if number of FlexFiles == 1, only join on one column
  } else {
    
    
    flat_ff <- dplyr::left_join(flexfile$actual_cost_hour_data, 
                                dplyr::select(flexfile$accounts, c(id, name)), 
                                by = c("account_id" = "id")) %>%
      dplyr::rename(account_name = name)
    
    flat_ff$account_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$clins, c(id, name)),
                                by = c("clin_id" = "id")) %>%
      dplyr::rename(clin_name = name)
    
    flat_ff$clin_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$wbs, c(level, id, name, parent_id)),
                                by = c("wbs_element_id" = "id")) %>%
      dplyr::rename(parent_wbs = parent_id) %>%
      dplyr::rename(wbs_name = name) %>%
      dplyr::rename(wbs_level = level)
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$functional_categories, c(id, name)),
                                by = c("functional_category_id" = "id")) %>%
      dplyr::rename(functional_category_name = name)
    
    flat_ff$functional_category_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$functional_overhead_categories, c(id, name)),
                                by = c("functional_overhead_category_id" = "id")) %>%
      dplyr::rename(functional_overhead_category_name = name)
    
    flat_ff$functional_overhead_category_id <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$reporting_calendar, c(id, start_date, end_date)),
                                by = c("reporting_period_id" = "id"))
    
    flat_ff$start_date <- as.Date(flat_ff$start_date)
    flat_ff$end_date <- as.Date(flat_ff$end_date)
    
    flat_ff$reporting_period_id <- NULL
    flat_ff$ff_id <- NULL
    
    flat_ff$ATD_or_FAC <- "ATD"
    
    #Forecasts ----
    
    flat_ff_forecast <- dplyr::left_join(flexfile$forecast_at_completion_cost_hour_data,
                                         dplyr::select(flexfile$wbs, c(level, id, name, parent_id)),
                                         by = c("wbs_element_id" = "id")) %>%
      dplyr::rename(parent_wbs = parent_id) %>%
      dplyr::rename(wbs_name = name) %>%
      dplyr::rename(wbs_level = level)
    
    flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                                         dplyr::select(flexfile$orders_or_lots, c(id, name, phase_or_milestone_id, contract_type_id)),
                                         by = c("order_or_lot_id" = "id")) %>%
      dplyr::rename(order_or_lot_name = name)
    
    flat_ff_forecast$order_or_lot_id <- NULL
    flat_ff_forecast$ATD_or_FAC <- "FAC"
    flat_ff_forecast$ff_id <- NULL
    
    #bind actuals and forecasts
    flat_ff_2 <- dplyr::bind_rows(flat_ff, flat_ff_forecast)
    
    
    
  }
  
}

#examples ----
flattened_ff <- flatten_ff(ffiles_cleaned_1) #one FlexFile

flattened_ffs <- flatten_ff(ffiles_cleaned_2) #more than one FlexFile

