#Actuals ----

flat_ff <- dplyr::left_join(ffiles_uid$ActualCostHourData, 
                     dplyr::select(ffiles_uid$Accounts, c(ID, Name)), 
                     by = c("AccountID" = "ID")) %>%
  dplyr::rename(Account_Name = Name)

flat_ff$AccountID <- NULL

flat_ff <- dplyr::left_join(flat_ff, dplyr::select(ffiles_uid$CLINs, c(ID, Name)),
                     by = c("CLIN_ID" = "ID")) %>%
  dplyr::rename(CLIN_Name = Name)

flat_ff$CLIN_ID <- NULL

flat_ff <- dplyr::left_join(flat_ff, dplyr::select(ffiles_uid$WBS, c(Level, ID, Name, ParentID)),
                     by = c("WBSElementID" = "ID")) %>%
  dplyr::rename(Parent_WBS = ParentID) %>%
  dplyr::rename(WBS_Name = Name) %>%
  dplyr::rename(WBS_Level = Level)

flat_ff <- dplyr::left_join(flat_ff, dplyr::select(ffiles_uid$FunctionalCategories, c(ID, Name)),
                     by = c("FunctionalCategoryID" = "ID")) %>%
  dplyr::rename(Functional_Category_Name = Name)

flat_ff$FunctionalCategoryID <- NULL

flat_ff <- dplyr::left_join(flat_ff, dplyr::select(ffiles_uid$FunctionalOverheadCategories, c(ID, Name)),
                     by = c("FunctionalOverheadCategoryID" = "ID")) %>%
  dplyr::rename(Functional_Overhead_Category_Name = Name)

flat_ff$FunctionalOverheadCategoryID <- NULL

flat_ff <- dplyr::left_join(flat_ff, dplyr::select(ffiles_uid$ReportingCalendar, c(ID, StartDate, EndDate)),
                     by = c("ReportingPeriodID" = "ID"))
                     
flat_ff$ReportingPeriodID <- NULL

flat_ff$ATD_Or_FAC <- "ATD"

#Forecasts ----

flat_ff_forecast <- dplyr::left_join(ffiles_uid$ForecastAtCompletionCostHourData,
                              dplyr::select(ffiles_uid$WBS, c(Level, ID, Name, ParentID)),
                     by = c("WBSElementID" = "ID")) %>%
  dplyr::rename(Parent_WBS = ParentID) %>%
  dplyr::rename(WBS_Name = Name) %>%
  dplyr::rename(WBS_Level = Level)

flat_ff_forecast$ATD_Or_FAC <- "FAC"

flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                              dplyr::select(ffiles_uid$OrdersOrLots, c(ID, Name, PhaseOrMilestoneID, ContractTypeID)),
                              by = c("OrderOrLotID" = "ID")) %>%
  dplyr::rename(OrderOrLotName = Name)

#Bind ----
flat_ff_2 <- dplyr::bind_rows(flat_ff, flat_ff_forecast)


#Function----
flatten_ff <- function(flexfile) {
  
  #Aligning IDs from ActualCostHourData table to Names from lookup tables
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
  
  flat_ff$ReportingPeriodID <- NULL
  
  flat_ff$ATD_Or_FAC <- "ATD"

  #Aligning IDs from ForecastAtCompletion table to Names from lookup tables
  
  flat_ff_forecast <- dplyr::left_join(flexfile$ForecastAtCompletionCostHourData,
                                       dplyr::select(flexfile$WBS, c(Level, ID, Name, ParentID)),
                                       by = c("WBSElementID" = "ID")) %>%
    dplyr::rename(Parent_WBS = ParentID) %>%
    dplyr::rename(WBS_Name = Name) %>%
    dplyr::rename(WBS_Level = Level)
  
  flat_ff_forecast$ATD_Or_FAC <- "FAC"
  
  flat_ff_forecast <- dplyr::left_join(flat_ff_forecast,
                                       dplyr::select(flexfile$OrdersOrLots, c(ID, Name, PhaseOrMilestoneID, ContractTypeID)),
                                       by = c("OrderOrLotID" = "ID")) %>%
    dplyr::rename(OrderOrLotName = Name)
  
  #Bind the above two tables
  dplyr::bind_rows(flat_ff, flat_ff_forecast)
  
  
  }
