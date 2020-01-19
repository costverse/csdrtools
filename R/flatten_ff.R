library(magrittr)

#creates a master flatfile from the flexfile tables
#joins IDs to Names (Account, Functional Category, OrderOrLot, etc.)
#binds forecasts to actuals


flatten_ff <- function(flexfile) {
  
  if ("ff_id" %in% colnames(flexfile$ActualCostHourData)) {
    
    #Aligning IDs from ActualCostHourData table to Names from lookup tables
    flat_ff <- dplyr::left_join(flexfile$ActualCostHourData, 
                                dplyr::select(flexfile$Accounts, c(ID, Name, ff_id)), 
                                by = c("AccountID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(Account_Name = Name)
    
    flat_ff$AccountID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$CLINs, c(ID, Name, ff_id)),
                                by = c("CLIN_ID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(CLIN_Name = Name)
    
    flat_ff$CLIN_ID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$WBS, c(Level, ID, Name, ParentID, ff_id)),
                                by = c("WBSElementID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(Parent_WBS = ParentID) %>%
      dplyr::rename(WBS_Name = Name) %>%
      dplyr::rename(WBS_Level = Level)
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$FunctionalCategories, c(ID, Name, ff_id)),
                                by = c("FunctionalCategoryID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(Functional_Category_Name = Name)
    
    flat_ff$FunctionalCategoryID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$FunctionalOverheadCategories, c(ID, Name, ff_id)),
                                by = c("FunctionalOverheadCategoryID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(Functional_Overhead_Category_Name = Name)
    
    flat_ff$FunctionalOverheadCategoryID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$ReportingCalendar, c(ID, StartDate, EndDate, ff_id)),
                                by = c("ReportingPeriodID" = "ID", "ff_id" = "ff_id"))
    
    flat_ff$StartDate <- as.Date(flat_ff$StartDate)
    flat_ff$EndDate <- as.Date(flat_ff$EndDate)
    
    flat_ff$ReportingPeriodID <- NULL
    
    flat_ff$ATD_Or_FAC <- "ATD"
    
    #Aligning IDs from ForecastAtCompletion table to Names from lookup tables
    
    flat_ff_forecast <- dplyr::left_join(flexfile$ForecastAtCompletionCostHourData,
                                         dplyr::select(flexfile$WBS, c(Level, ID, Name, ParentID, ff_id)),
                                         by = c("WBSElementID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(Parent_WBS = ParentID) %>%
      dplyr::rename(WBS_Name = Name) %>%
      dplyr::rename(WBS_Level = Level)
    
    flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                                         dplyr::select(flexfile$OrdersOrLots, c(ID, Name, PhaseOrMilestoneID, ContractTypeID, ff_id)),
                                         by = c("OrderOrLotID" = "ID", "ff_id" = "ff_id")) %>%
      dplyr::rename(OrderOrLotName = Name)
    
    flat_ff_forecast$ATD_Or_FAC <- "FAC"
    
    #Bind the above two tables
    dplyr::bind_rows(flat_ff, flat_ff_forecast)
    # Cleansed ----
    flat_ff <- janitor::clean_names(flat_ff)
    
  } else {
 
    flat_ff <- dplyr::left_join(flexfile$ActualCostHourData, 
                                dplyr::select(flexfile$Accounts, c(ID, Name)), 
                                by = c("AccountID" = "ID")) %>%
      dplyr::rename(Account_Name = Name)
    
    flat_ff$AccountID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$CLINs, c(ID, Name)),
                                by = c("CLIN_ID" = "ID")) %>%
      dplyr::rename(CLIN_Name = Name)
    
    flat_ff$CLIN_ID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$WBS, c(Level, ID, Name, ParentID)),
                                by = c("WBSElementID" = "ID")) %>%
      dplyr::rename(Parent_WBS = ParentID) %>%
      dplyr::rename(WBS_Name = Name) %>%
      dplyr::rename(WBS_Level = Level)
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$FunctionalCategories, c(ID, Name)),
                                by = c("FunctionalCategoryID" = "ID")) %>%
      dplyr::rename(Functional_Category_Name = Name)
    
    flat_ff$FunctionalCategoryID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$FunctionalOverheadCategories, c(ID, Name)),
                                by = c("FunctionalOverheadCategoryID" = "ID")) %>%
      dplyr::rename(Functional_Overhead_Category_Name = Name)
    
    flat_ff$FunctionalOverheadCategoryID <- NULL
    
    flat_ff <- dplyr::left_join(flat_ff, dplyr::select(flexfile$ReportingCalendar, c(ID, StartDate, EndDate)),
                                by = c("ReportingPeriodID" = "ID"))
    
    flat_ff$StartDate <- as.Date(flat_ff$StartDate)
    flat_ff$EndDate <- as.Date(flat_ff$EndDate)
    
    flat_ff$ReportingPeriodID <- NULL
    flat_ff$ff_id <- NULL
    
    flat_ff$ATD_Or_FAC <- "ATD"
    
    #Forecasts ----
    
    flat_ff_forecast <- dplyr::left_join(flexfile$ForecastAtCompletionCostHourData,
                                         dplyr::select(flexfile$WBS, c(Level, ID, Name, ParentID)),
                                         by = c("WBSElementID" = "ID")) %>%
      dplyr::rename(Parent_WBS = ParentID) %>%
      dplyr::rename(WBS_Name = Name) %>%
      dplyr::rename(WBS_Level = Level)
    
    flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                                         dplyr::select(flexfile$OrdersOrLots, c(ID, Name, PhaseOrMilestoneID, ContractTypeID)),
                                         by = c("OrderOrLotID" = "ID")) %>%
      dplyr::rename(OrderOrLotName = Name)
    
    flat_ff_forecast$ATD_Or_FAC <- "FAC"
    flat_ff_forecast$ff_id <- NULL
    
    #Bind ----
    flat_ff_2 <- dplyr::bind_rows(flat_ff, flat_ff_forecast)
    # Cleansed ----
    flat_ff_2 <- janitor::clean_names(flat_ff_2)
    
  }
  
  }


flattened_ff <- flatten_ff(ffiles_one)

flattened_ffs <- flatten_ff(ffiles_uid)
