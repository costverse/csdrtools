
wb <- loadWorkbook(file = here::here("R", "csdr_submission_review_template.xlsx"))

removeTable(wb = wb,
            sheet = "data",
            table = "data")

writeDataTable(wb = wb,
               sheet = "data",
               tableName = "data",
               x = csdr, 
               colNames = TRUE)

writeDataTable(wb = wb,
               sheet = "data",
               tableName = "data",
               x = csdr, 
               colNames = TRUE)

saveWorkbook(wb = wb,
             file =  here::here("R", "csdr_submission_review.xlsx"),
             overwrite = TRUE)
