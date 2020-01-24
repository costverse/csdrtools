#Functions to summarize FlexFile data. Includes tables, summary statistics, etc.

library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)


tbl_1921 <- function(flattened_ff, value = c("Dollars", "Hours"), formatted_table = c(TRUE, FALSE)){
  
  if(value == "Dollars") {
    
    temp_1921 <- flattened_ff %>%
      dplyr::group_by(wbs_element_id, wbs_name, atd_or_fac, nonrecurring_or_recurring_id) %>%
      dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
      tidyr::gather(key = value_type, value = value, value_dollars, value_hours) %>%
      tidyr::unite(col = temp, atd_or_fac, nonrecurring_or_recurring_id, sep = "_") %>%
      tidyr::spread(key = temp, value = value) %>%
      dplyr::filter(value_type == "value_dollars") %>%
      dplyr::select(-value_type)
      
    if(formatted_table == TRUE) {
      
      temp_1921 %>%
        knitr::kable(., col.names = c("WBS Element", "WBS Element Name", "Nonrecurring", "Recurring", "Nonrecurring", "Recurring"))  %>%
        kableExtra::kable_styling(c("striped", "bordered")) %>%
        kableExtra::add_header_above(c("", "", "Actuals To Date" = 2, "Forecasts At Completion" = 2))
      
    } else if(formatted_table == FALSE){
      
      temp_1921
      
    }  
    
    
  } else if(value == "Hours") {
    
    temp_1921 <- flattened_ff %>%
      dplyr::group_by(wbs_element_id, wbs_name, atd_or_fac, nonrecurring_or_recurring_id) %>%
      dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
      tidyr::gather(key = value_type, value = value, value_dollars, value_hours) %>%
      tidyr::unite(col = temp, atd_or_fac, nonrecurring_or_recurring_id, sep = "_") %>%
      tidyr::spread(key = temp, value = value) %>%
      dplyr::filter(value_type == "value_hours") %>%
      dplyr::select(-value_type)
      
      if(formatted_table == TRUE) {
        
        temp_1921 %>%
          knitr::kable(., col.names = c("WBS Element", "WBS Element Name", "Nonrecurring", "Recurring", "Nonrecurring", "Recurring"))  %>%
          kableExtra::kable_styling(c("striped", "bordered")) %>%
          kableExtra::add_header_above(c("", "", "Actuals To Date" = 2, "Forecasts At Completion" = 2))
        
      } else if(formatted_table == FALSE){
        
        temp_1921
        
      }    
    
  }
  
}


tbl_1921_1 <- function(flattened_ff, value = c("Dollars", "Hours")){
  
  if(value == "Dollars") {
    
    flattened_ff %>%
      dplyr::group_by(detailed_standard_category_id, atd_or_fac, nonrecurring_or_recurring_id) %>%
      dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
      tidyr::gather(key = value_type, value = value, value_dollars, value_hours) %>%
      tidyr::unite(col = temp, atd_or_fac, nonrecurring_or_recurring_id, sep = "_") %>%
      tidyr::spread(key = temp, value = value) %>%
      dplyr::filter(value_type == "value_dollars") %>%
      dplyr::select(-value_type) %>%
      
      knitr::kable(., col.names = c("Standard Functional Category", "Nonrecurring", "Recurring", "Nonrecurring", "Recurring"))  %>%
      kableExtra::kable_styling(c("striped", "bordered")) %>%
      kableExtra::add_header_above(c("", "Actuals To Date" = 2, "Forecasts At Completion" = 2))
    
  } else if(value == "Hours"){
    
    flattened_ff %>%
      dplyr::group_by(detailed_standard_category_id, atd_or_fac, nonrecurring_or_recurring_id) %>%
      dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
      tidyr::gather(key = value_type, value = value, value_dollars, value_hours) %>%
      tidyr::unite(col = temp, atd_or_fac, nonrecurring_or_recurring_id, sep = "_") %>%
      tidyr::spread(key = temp, value = value) %>%
      dplyr::filter(value_type == "value_hours") %>%
      dplyr::select(-value_type) %>%
      
      knitr::kable(., col.names = c("Standard Functional Category", "Nonrecurring", "Recurring", "Nonrecurring", "Recurring"))  %>%
      kableExtra::kable_styling(c("striped", "bordered")) %>%
      kableExtra::add_header_above(c("", "Actuals To Date" = 2, "Forecasts At Completion" = 2))
    
  }
  
}


tbl_top_10_Accounts <- function(flattened_ff){
  
  flattened_ff %>%
    dplyr::filter(atd_or_fac == "ATD") %>%
    dplyr::group_by(account_name) %>%
    dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
    dplyr::arrange(-value_dollars) %>%
    dplyr::mutate(., dollars_pct = value_dollars/sum(value_dollars)) %>%
    dplyr::top_n(10, value_dollars) %>%
    
    knitr::kable(., col.names = c("Account Name", "Actual Dollars", "Actual Hours", "% of Total Dollars"))  %>%
    kableExtra::kable_styling(c("striped", "bordered"))
  
}


tbl_top_10_CLINs <- function(flattened_ff){
  
  flattened_ff %>%
    dplyr::filter(atd_or_fac == "ATD") %>%
    dplyr::group_by(clin_name) %>%
    dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
    dplyr::arrange(-value_dollars) %>%
    dplyr::mutate(., dollars_pct = value_dollars/sum(value_dollars)) %>%
    dplyr::top_n(10, value_dollars) %>%
    
    knitr::kable(., col.names = c("CLIN Name", "Actual Dollars", "Actual Hours", "% of Total Dollars"))  %>%
    kableExtra::kable_styling(c("striped", "bordered"))
  
}


tbl_WBS_by_Account <- function(flattened_ff){
  
  flattened_ff %>%
    dplyr::filter(atd_or_fac == "ATD") %>%
    dplyr::group_by(wbs_element_id, wbs_name, account_name) %>%
    dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
    dplyr::arrange(wbs_element_id) %>%
    
    knitr::kable(., col.names = c("WBS Element", "WBS Element Name", "Account Name", "Actual DOllars", "Actual Hours")) %>%
    kableExtra::kable_styling(c("striped", "bordered")) %>%
    kableExtra::collapse_rows(columns = 1:2, valign = "top")
  
}

tbl_direct_rates <- function(flattened_ff){
  
  flattened_ff %>%
    dplyr::filter(atd_or_fac == "ATD") %>%
    dplyr::group_by(detailed_standard_category_id, functional_category_name) %>%
    dplyr::summarise(value_dollars = sum(value_dollars), value_hours = sum(value_hours)) %>%
    dplyr::filter(detailed_standard_category_id %in% c("DIRECT_MANUFACTURING_TOUCH_LABOR",
                                                       "DIRECT_MANUFACTURING_SUPPORT_LABOR",
                                                       "DIRECT_ENGINEERING_LABOR")) %>%
    dplyr::mutate(., direct_rate = value_dollars/value_hours) %>%
    
    knitr::kable(., col.names = c("Standard Functional Category", "Functional Category", "Actual DOllars", "Actual Hours", "Direct Rate")) %>%
    kableExtra::kable_styling(c("striped", "bordered")) %>%
    kableExtra::collapse_rows(columns = 1, valign = "top")
  
}


tbl_WBS_by_date <- function(flattened_ff, timeframe = c("Year", "Month")) {
  
  if(timeframe == "Year"){
    
    flattened_ff %>%
      dplyr::filter(atd_or_fac == "ATD") %>%
      dplyr::mutate(year = base::substr(end_date, 1, 4)) %>%
      dplyr::group_by(wbs_element_id, wbs_name, year) %>%
      dplyr::summarise(value_dollars = sum(value_dollars)) %>%
      tidyr::spread(key = year, value = value_dollars) %>%
    
      kable(.) %>%
      kableExtra::kable_styling(c("striped", "bordered"))
    
  }
  
  else if(timeframe == "Month"){
    
    flattened_ff %>%
      dplyr::filter(atd_or_fac == "ATD") %>%
      tidyr::unite(col = temp, start_date, end_date, sep = " to ") %>%
      dplyr::group_by(wbs_element_id, wbs_name, temp) %>%
      dplyr::summarise(value_dollars = sum(value_dollars)) %>%
      tidyr::spread(key = temp, value = value_dollars) %>%
      
      kable(.) %>%
      kableExtra::kable_styling(c("striped", "bordered"))
    
  }
  
}


# test

# tbl_1921(flattened_ff, value = "Dollars")

tbl_1921(flattened_ff, value = "Hours", formatted_table = FALSE)

tbl_1921_1(flattened_ff, value = "Hours")

tbl_top_10_Accounts(flattened_ff)

tbl_top_10_CLINs(flattened_ff)

tbl_WBS_by_Account(flattened_ff)

tbl_direct_rates(flattened_ff)

tbl_WBS_by_date(flattened_ff, timeframe = "Year")










