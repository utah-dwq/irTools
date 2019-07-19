# Download export workbook from Shinyapp

exportData <- function(AU_list, compiled_data){
  
  # Create styles for export headers
  Identifier = openxlsx::createStyle(textDecoration = "bold", bgFill = "yellow")
  IR = openxlsx::createStyle(textDecoration = "bold", bgFill = "pink")
  
  # Narrow compiled data to clicked AU's
  compiled_data_narrow = lapply(compiled_data, function(x){x = x[x$ASSESS_ID%in%AU_list,]})
  
  # Create workbook
  reviewer_export <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(reviewer_export, sheetName = "Data Summary")
  openxlsx::writeDataTable(reviewer_export, sheet = "Data Summary", compiled_data_narrow$summary_tc_assessed)
  
  openxlsx::addWorksheet(reviewer_export, sheetName = "Abbreviated Data")
  openxlsx::writeDataTable(reviewer_export, sheet = "Abbreviated Data", compiled_data_narrow$toxconv_data_asmnt)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Abbreviated Data", cols = 1:95, rows = 1, rule = "IR", type = "contains", style = IR)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Abbreviated Data", cols = 1:95, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
  
  if(!is.null(compiled_data_narrow$ecoli_data_asmnt)){
    openxlsx::addWorksheet(reviewer_export, sheetName = "E.coli Data")
    openxlsx::writeData(reviewer_export, sheet = "E.coli Data", compiled_data_narrow$ecoli_data_asmnt)
  }
  
  # Save workbook
  
  openxlsx::saveWorkbook(reviewer_export, "data_Export.xlsx", overwrite = TRUE)
  
}