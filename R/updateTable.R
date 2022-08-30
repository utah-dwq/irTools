#' Update translation workbook sheets with new combinations found in the WQP dataset.
#'
#' Joins translation workbook sheets (activityMediaNameTable, labNameActivityTable, detConditionTable, detLimitTypeTable, paramTransTable, paramFractionTable, unitConvTable) to WQP data to apply decisions from input files to data.
#' @param data A merged WQP result object. Must include both narrowresult & activity OR detquantlim files. May also be a post-fillMaskedValues() results object. Note: re-application of edited domain tables to an already screened dataset is not advised, and changes to the domain table likely will not be reflected in a re-screened dataset due to merging conventions.
#' @param trans_wb Full path and filename for Excel workbook containing screen table to be applied
#' @param sheet Name of sheet in workbook holding desired screening decisions
#' @param cols A vector of column names to be screened for new combinations of values to be placed in the translation workbook.
#' @param params Logical. If true, will screen WQ parameters for CAS ID and match to CAS IDs in the standards workbook.
#' @param standards_wb Full path and filename for Excel workbook containing WQ standards to be used to assess WQP data.
#' @return Message reporting the number of new combinations found in the data that were added to the workbook sheet. Workbook sheet is updated in translation workbook, with blank IR_FLAG column for new combinations in need of review.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @export
updateTable <- function(data,trans_wb,sheet,cols,params=FALSE,standards_wb){
  #Identify unique combinations
  combos=data.frame(unique(data[,cols]))
  names(combos)=cols
  combos[combos==""]=NA
  combos$InData="Y"
  
  #Load translation workbook
  wb=openxlsx::loadWorkbook(trans_wb)
  table = openxlsx::readWorkbook(wb, sheet=sheet)
  table$DateAdded = openxlsx::convertToDate(table$DateAdded)
  table=table[,!names(table)%in%c("InData")]
  
  combo_merge=merge(combos, table, all=T)
  
  if(params==TRUE){
    # Open CAS Lookup table from WQP
    caslookup = data.frame(openxlsx::readWorkbook(wb, sheet="CASLookup", startRow=2))[,c("Name","CAS.Number")]
    names(caslookup)[names(caslookup)=="Name"] = "CharacteristicName"
    names(caslookup)[names(caslookup)=="CAS.Number"] = "CAS_WQP"
    
    before = dim(combo_merge)[1]
    
    combo_merge = unique(combo_merge[,!names(combo_merge)%in%c("CAS_in_Stds","CAS_WQP","CAS_check")])
    combo_merge = merge(combo_merge, caslookup, all.x = TRUE)
    
    # Open standards table and extract CAS numbers
    crit= data.frame(openxlsx::readWorkbook(standards_wb,sheet = "criteria"))
    crit = unique(crit$CAS)
    ss_crit = data.frame(openxlsx::readWorkbook(standards_wb,sheet = "ss_criteria"))
    ss_crit = unique(ss_crit$CAS)
    crits = unique(c(crit, ss_crit))
    
    # Flag data records with CAS numbers represented in standards
    combo_merge$CAS_in_Stds = ifelse(combo_merge$CAS_WQP%in%crits,"Y",NA)
    
    after = dim(combo_merge)[1]
    if(!before==after){
      stop("Parameter table grew after application of CAS numbers and cross check with standards workbook. Records likely duplicated.")
    }
  }
  
  new_combos_count=dim(combo_merge)[1]-dim(table)[1]
  if(new_combos_count>0){combo_merge$DateAdded[is.na(combo_merge$DateAdded)]=Sys.Date()}
  openxlsx::writeData(wb, sheet, combo_merge)
  print(paste(sheet," updated.", new_combos_count,"new combinations identified."))
  
  ###Save translation workbook
  openxlsx::saveWorkbook(wb, trans_wb, overwrite = TRUE)
  print("Translation workbook updated & saved.")
}
