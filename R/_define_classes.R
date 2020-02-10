
flexfile <- structure(
  list(
    ReportConfiguration = list(
      DetailedStandardCategory = logical(),
      GA_AsStandardCategory = logical(),
      FCCM_AsStandardCategory = logical(),
      ForecastAtCompletion_ByNonrecurringOrRecurring = logical(),
      ForecastAtCompletion_ByStandardCategory = logical()
    ),
    ReportMetadata = list(
      SecurityClassification = character(),
      ProprietaryStatement = character(),
      ProgramName = character(),
      PhaseOrMilestoneID = factor(
        levels = c(
          "PRE_A",
          "A",
          "B",
          "C_LRIP",
          "C_FRP",
          "O_AND_S",
          "MULTIPLE"
        ),
        ordered = TRUE),
      PrimeMissionProduct = character(),
      CommodityType = character(),
      ReportingOrganization_OrganizationName = character(),
      ReportingOrganization_DivisionName = character(),
      ReportingOrganization_CageCode = character(),
      ReportingOrganization_Location_Street = character(),
      ReportingOrganization_Location_City = character(),
      ReportingOrganization_Location_State = character(),
      ReportingOrganization_Location_ZipCode = character(),
      ReportingOrganization_Location_Country = character(),
      ApprovedPlanNumber = character(),
      ApprovedPlanRevisionNumber = character(),
      CustomerName = character(),
      ContractTypeID = factor(
        levels = c(
          "CS",
          "CPAF",
          "CPFF",
          "CPIF",
          "CPIF_PI",
          "FFP",
          "FPIF",
          "FPIST",
          "FPIST_PI",
          "FPIFT_PI",
          "FPAF",
          "FP_EPA",
          "FP_PPR",
          "FCP_RPR",
          "FFP_LOET",
          "IDIQ",
          "LC",
          "TM",
          "OTHER",
          "MULTIPLE"
        )),
      ContractPrice = numeric(),
      ContractCeiling = numeric(),
      ContractNumber = character(),
      PeriodOfPerformance_StartDate = lubridate::ymd(),
      PeriodOfPerformance_EndDate = lubridate::ymd(),
      ReportCycleID = factor(
        levels = c(
          "INITIAL",
          "INTERIM",
          "FINAL"
        ),
        ordered = TRUE),
      SubmissionEvent_Number = integer(),
      SubmissionEvent_Name = character(),
      SubmissionEvent_IsWildcard = logical(),
      ResubmissionNumber = integer(),
      ReportAsOf = lubridate::ymd(),
      PointOfContact_Name = character(),
      PointOfContact_Department = character(),
      PointOfContact_TelephoneNumber = character(),
      PointOfContact_EmailAddress = character(),
      DatePrepared = lubridate::ymd(),
      ReportingPeriodID = integer()
    ),
    OrdersOrLots = dplyr::tibble(
      ID = character(),
      Name = character(),
      PhaseOrMilestoneID = character(),
      CustomerName = character(),
      ContractTypeID = factor(
        levels = c(
          "CS",
          "CPAF",
          "CPFF",
          "CPIF",
          "CPIF_PI",
          "FFP",
          "FPIF",
          "FPIST",
          "FPIST_PI",
          "FPIFT_PI",
          "FPAF",
          "FP_EPA",
          "FP_PPR",
          "FCP_RPR",
          "FFP_LOET",
          "IDIQ",
          "LC",
          "TM",
          "OTHER",
          "MULTIPLE"
        )),
      ContractPrice = numeric(),
      ContractCeiling = numeric(),
      PeriodOfPerformance_StartDate = lubridate::ymd(),
      PeriodOfPerformance_EndDate = lubridate::ymd(),
      AppropriationTypeID = factor(
        levels = c(
          "RDTE",
          "PROCUREMENT",
          "O_AND_M"
        ))),
    CLINs = dplyr::tibble(
      ID = factor(),
      Name = character(),
      ContractTypeID = factor(
        levels = c(
          "CS",
          "CPAF",
          "CPFF",
          "CPIF",
          "CPIF_PI",
          "FFP",
          "FPIF",
          "FPIST",
          "FPIST_PI",
          "FPIFT_PI",
          "FPAF",
          "FP_EPA",
          "FP_PPR",
          "FCP_RPR",
          "FFP_LOET",
          "IDIQ",
          "LC",
          "TM",
          "OTHER",
          "MULTIPLE"
        ))
    ),
    EndItems = dplyr::tibble(
      ID = character(),
      Name = character()
    ),
    WBS = dplyr::tibble(
      Level = integer(),
      ID = character(),
      Name = character(),
      ParentID = character()
    ),
    Accounts = dplyr::tibble(
      ID = character(),
      Name = character()
    ),
    FunctionalCategories = dplyr::tibble(
      ID = character(),
      Name = character()
    ),
    FunctionalOverheadCategories = dplyr::tibble(
      ID = character(),
      Name = character()
    ),
    UnitsOrSublots = dplyr::tibble(
      ID = character(),
      EndItemID = character(),
      FirstUnitNumber = integer(),
      LastUnitNumber = integer(),
      OrderOrLotID = character()
    ),
    ReportingCalendar = dplyr::tibble(
      ID = integer(),
      StartDate = lubridate::ymd(),
      EndDate = lubridate::ymd()
    ),
    SummaryCostData = dplyr::tibble(
      OrderOrLotID = character(),
      Subtotal_TD = numeric(),
      Subtotal_AC = numeric(),
      GA_TD = numeric(),
      GA_AC = numeric(),
      UB = numeric(),
      MR = numeric(),
      FCCM_TD = numeric(),
      FCCM_AC = numeric(),
      Fee_TD = numeric(),
      Fee_AC = numeric(),
      Price_TD = numeric(),
      Price_AC = numeric()
    ),
    ActualCostHourData = dplyr::tibble(
      OrderOrLotID = character(),
      CLIN_ID = factor(),
      EndItemID = character(),
      WBSElementID = character(),
      AccountID = character(),
      NonrecurringOrRecurringID = factor(
        levels = c(
          "NONRECURRING",
          "RECURRING"
        )
      ),
      FunctionalCategoryID = character(),
      FunctionalOverheadCategoryID = character(),
      StandardCategoryID = factor(
        levels = c(
          "DIRECT_ENGINEERING_LABOR",
          "ENGINEERING_LABOR_OVERHEAD",
          "DIRECT_MANUFACTURING_TOUCH_LABOR",
          "DIRECT_MANUFACTURING_OTHER_LABOR",
          "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD",
          "DIRECT_MAINTENANCE_TOUCH_LABOR",
          "DIRECT_MAINTENANCE_OTHER_LABOR",
          "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD",
          "OTHER_DIRECT_COSTS",
          "OTHER_OVERHEAD",
          "DIRECT_MATERIALS",
          "MATERIAL_OVERHEAD",
          "GENERAL_AND_ADMINISTRATIVE",
          "FACILITIES_CAPITAL_COST_OF_MONEY"
        )),
      DetailedStandardCategoryID = factor(
        levels = c(
          "DIRECT_ENGINEERING_LABOR",
          "ENGINEERING_LABOR_OVERHEAD",
          "DIRECT_MANUFACTURING_TOUCH_LABOR",
          "DIRECT_MANUFACTURING_SUPPORT_LABOR",
          "DIRECT_MANUFACTURING_TOOLING_LABOR",
          "DIRECT_MANUFACTURING_OTHER_LABOR",
          "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD",
          "DIRECT_MAINTENANCE_TOUCH_LABOR",
          "DIRECT_MAINTENANCE_SUPPORT_LABOR",
          "DIRECT_MAINTENANCE_OTHER_LABOR",
          "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD",
          "DIRECT_PROGRAM_MANAGEMENT_LABOR",
          "DIRECT_OTHER_LABOR",
          "DIRECT_SERVICES",
          "OTHER_DIRECT_NON_LABOR",
          "OTHER_OVERHEAD",
          "DIRECT_REPORTING_SUBCONTRACTOR",
          "INTERCOMPANY_WORK_ORDERS",
          "PURCHASED_PARTS",
          "PURCHASED_EQUIPMENT",
          "RAW_MATERIALS",
          "DIRECT_TOOLING_AND_EQUIPMENT",
          "OTHER_MATERIAL",
          "MATERIAL_OVERHEAD",
          "GENERAL_AND_ADMINISTRATIVE",
          "FACILITIES_CAPITAL_COST_OF_MONEY"
        )),
      UnitOrSublotID = character(),
      AllocationMethodID = character(),
      ReportingPeriodID = integer(),
      Tag1 = character(),
      Tag2 = character(),
      Tag3 = character(),
      Tag4 = character(),
      Tag5 = character(),
      Tag6 = character(),
      Tag7 = character(),
      Tag8 = character(),
      Tag9 = character(),
      Tag10 = character(),
      Tag11 = character(),
      Tag12 = character(),
      Tag13 = character(),
      Tag14 = character(),
      Tag15 = character(),
      Tag16 = character(),
      Tag17 = character(),
      Tag18 = character(),
      Tag19 = character(),
      Tag20 = character(),
      Tag21 = character(),
      Tag22 = character(),
      Tag23 = character(),
      Tag24 = character(),
      Tag25 = character(),
      Value_Dollars = numeric(),
      Value_Hours = numeric()
    ),
    ForecastAtCompletionCostHourData = dplyr::tibble(
      OrderOrLotID = character(),
      WBSElementID = character(),
      NonrecurringOrRecurringID = character(),
      StandardCategoryID = character(),
      DetailedStandardCategoryID = character(),
      Value_Dollars = numeric(),
      Value_Hours = numeric()
    ),
    AllocationMethods = dplyr::tibble(
      ID = character(),
      AllocationMethodTypeID = factor(
        levels = c(
          "PERCENT",
          "PRORATE"
        )),
      IsUnitOrSublotAllocationMethod = logical(),
      Name = character()
    ),
    AllocationComponents = dplyr::tibble(
      AllocationMethodID = character(),
      OrderOrLotID = character(),
      EndItemID = character(),
      WBSElementID = character(),
      UnitOrSublotID = character(),
      PercentValue = numeric()
    ),
    SummaryRemarks = dplyr::tibble(
      OrderOrLotID = character(),
      Text = character()
    ),
    WBSElementRemarks = dplyr::tibble(
      OrderOrLotID = character(),
      WBSElementID = character(),
      Text = character()
    ),
    WBSDictionaryDefinitions = dplyr::tibble(
      WBSElementID = character(),
      Text = character()
    ),
    CostHourTagDefinitions = dplyr::tibble(
      CostHourTagID = factor(
        levels = c(
          "TAG1",
          "TAG2",
          "TAG3",
          "TAG4",
          "TAG5",
          "TAG6",
          "TAG7",
          "TAG8",
          "TAG9",
          "TAG10",
          "TAG11",
          "TAG12",
          "TAG13",
          "TAG14",
          "TAG15",
          "TAG16",
          "TAG17",
          "TAG18",
          "TAG19",
          "TAG20",
          "TAG21",
          "TAG22",
          "TAG23",
          "TAG24",
          "TAG25"
        )),
      Name = character(),
      Text = character()
    )
  ),
  class = "flexfile"
)

new_flexfile <- function(...) {
  structure(flexfile, class = "flexfile")
}

# Quantity Report -------------------------------------------------------------

qty_rpt <- structure(
  list(
    ReportMetadata = list(
      SecurityClassification = character(),
      ProprietaryStatement = character(),
      ProgramName = character(),
      PhaseOrMilestoneID = factor(
        levels = c(
          "PRE_A",
          "A",
          "B",
          "C_LRIP",
          "C_FRP",
          "O_AND_S",
          "MULTIPLE"),
        ordered = TRUE),
      PrimeMissionProduct = character(),
      CommodityType = character(),
      ReportingOrganization_OrganizationName = character(),
      ReportingOrganization_DivisionName = character(),
      ReportingOrganization_CageCode = character(),
      ReportingOrganization_Location_Street = character(),
      ReportingOrganization_Location_City = character(),
      ReportingOrganization_Location_State = character(),
      ReportingOrganization_Location_ZipCode = character(),
      ReportingOrganization_Location_Country = character(),
      ApprovedPlanNumber = character(),
      ApprovedPlanRevisionNumber = character(),
      CustomerName = character(),
      ContractTypeID = factor(
        levels = c(
          "CS",
          "CPAF",
          "CPFF",
          "CPIF",
          "CPIF_PI",
          "FFP",
          "FPIF",
          "FPIST",
          "FPIST_PI",
          "FPIFT_PI",
          "FPAF",
          "FP_EPA",
          "FP_PPR",
          "FCP_RPR",
          "FFP_LOET",
          "IDIQ",
          "LC",
          "TM",
          "OTHER",
          "MULTIPLE"
        )),
      ContractPrice = numeric(),
      ContractCeiling = numeric(),
      ContractNumber = character(),
      PeriodOfPerformance_StartDate = lubridate::ymd(),
      PeriodOfPerformance_EndDate = lubridate::ymd(),
      ReportCycleID = factor(
        levels = c(
          "INITIAL",
          "INTERIM",
          "FINAL"
        ),
        ordered = TRUE),
      SubmissionEvent_Number = integer(),
      SubmissionEvent_Name = character(),
      SubmissionEvent_IsWildcard = logical(),
      ResubmissionNumber = integer(),
      ReportAsOf = lubridate::ymd(),
      PointOfContact_Name = character(),
      PointOfContact_Department = character(),
      PointOfContact_TelephoneNumber = character(),
      PointOfContact_EmailAddress = character(),
      DatePrepared = lubridate::ymd()
    ),
    OrdersOrLots = dplyr::tibble(
      ID = character(),
      Name = character(),
      PhaseOrMilestoneID = character(),
      CustomerName = character(),
      ContractTypeID = factor(
        levels = c(
          "CS",
          "CPAF",
          "CPFF",
          "CPIF",
          "CPIF_PI",
          "FFP",
          "FPIF",
          "FPIST",
          "FPIST_PI",
          "FPIFT_PI",
          "FPAF",
          "FP_EPA",
          "FP_PPR",
          "FCP_RPR",
          "FFP_LOET",
          "IDIQ",
          "LC",
          "TM",
          "OTHER",
          "MULTIPLE"
        )),
      ContractPrice = numeric(),
      ContractCeiling = numeric(),
      PeriodOfPerformance_StartDate = lubridate::ymd(),
      PeriodOfPerformance_EndDate = lubridate::ymd(),
      AppropriationTypeID = factor(
        levels = c(
          "RDTE",
          "PROCUREMENT",
          "O_AND_M"
        ))),
    EndItems = dplyr::tibble(
      ID = character(),
      Name = character()
    ),
    WBS = dplyr::tibble(
      Level = integer(),
      ID = character(),
      Name = character(),
      ParentID = character()
    ),
    QuantitiesAtCompletion = dplyr::tibble(
      OrderOrLotID = character(),
      EndItemID = character(),
      WBSElementID = character(),
      DeliveredQuantityAtCompletion = numeric(),
      InternalQuantityAtCompletion = numeric(),
      CoproductionOrConcurrentQuantityAtCompletion = numeric(),
      GFEQuantityAtCompletion = numeric()
    ),
    QuantitiesToDate = dplyr::tibble(
      OrderOrLotID = character(),
      WBSElementID = character(),
      CompletedQuantityToDate = numeric(),
      InProcessQuantity = numeric()
    ),
    ProductionSequence = dplyr::tibble(
      EndItemID = character(),
      FirstUnitNumber = integer(),
      LastUnitNumber = integer(),
      OrderOrLotID = character(),
      IsInternal = logical()
    ),
    SummaryRemarks = dplyr::tibble(
      OrderOrLotID = character(),
      Text = character()
    ),
    WBSElementRemarks = dplyr::tibble(
      OrderOrLotID = character(),
      WBSElementID = character(),
      Text = character()
    )
  ),
  class = "qty_rpt"
)

new_PhaseOrMilestoneID <- function(PhaseOrMilestoneID = factor()) {
  
  levels <- c("PRE_A",
              "A",
              "B",
              "C_LRIP",
              "C_FRP",
              "O_AND_S",
              "MULTIPLE")
  
  if (!(PhaseOrMilestoneID %in% levels))
    stop("Error: `PhaseOrMilestoneID` must be one of the following:\n",
         "\t─ \"PRE_A\"\n",
         "\t─ \"A\"\n",
         "\t─ \"B\"\n",
         "\t─ \"C_LRIP\"\n",
         "\t─ \"C_FRP\"\n",
         "\t─ \"O_AND_S\"\n",
         "\t─ \"MULTIPLE\"\n"
    )
  
  structure(PhaseOrMilestoneID,
            levels = levels,
            class = "PhaseOrMilestoneID")
}
