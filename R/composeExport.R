#' Create dataset for secondary reviewer export from assessment dashboard
#'
#' Pulls columns of interest from prepped data and calculates basic exceedance summaries for site-use-parameter assessments.
#' @param prepped_data A list of objects produced by dataPrep function, including toxics, conventionals, and ecoli dataframes.
#' @param toxics_assessed A dataframe object of toxic assessments from the assessExcCounts function.
#' @param conventionals_assessed A dataframe object of conventionals assessments from the assessExcCounts function.
#' @param create_workbooks A logical argument to specify whether workbooks should be made of the specified parameters. Defaults to FALSE.
#' @param path A folder path name in which to drop the exported workbooks. Only required if create_workbooks is true.
#' @param params A vector of assessment parameter names to be placed in workbooks.
#' @param include_rejected A logical argument to specify whether the export workbooks should include rejected data or not. Defaults to true.
#' @param rejected_data A dataframe object with all of the data screened out using the laboratory, media, detection-quantitation limit, parameter translation tables, and data prep screens. This is only required if include_rejected = TRUE.
#' @return A list composed of three dataframes: site-date-use-param records linked with aggregated daily values (if applicable), calculations, and exceedances, site-use-param summaries including sample and exceedance counts, and prepped E.coli data.
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom plyr rbind.fill

### TESTING ###
# See "P:\WQ\Integrated Report\Automation_Development\elise\AU_export_testing\au_export_testing_steps.R" for steps taken up to dataPrep
# load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\au_export_resultdata.RData")
# prepped_data = result

#blah = composeExport(screened_data = screened_data, prepped_data = result, toxics_assessed = toxics_assessed, conventionals_assessed = conventionals_assessed, include_rejected = FALSE, create_workbooks = FALSE)
#"Minimum Dissolved Oxygen","pH",
# blah = composeExport(prepped_data, toxics_assessed, conventionals_assessed, include_rejected = TRUE, rejected_data = all_rejected_records, create_workbooks = TRUE, params = c("Aluminum","Arsenic","Boron","Cadmium","Copper","E. coli","Max. Temperature","Iron","Lead","Nitrates as N","Selenium","Total Ammonia as N","Total Dissolved Solids","Zinc","Total Phosphorus as P","Radium 226, 228 (Combined)"), path = path)
#' @export
composeExport <- function(prepped_data, toxics_assessed, conventionals_assessed, include_rejected = TRUE, rejected_data, create_workbooks = FALSE, params=c("Mercury","pH","Max. Temperature","Arsenic","Cadmium","Chromium","Aluminum","Total Ammonia as N","Calcium","Minimum Dissolved Oxygen","Hardness","Magnesium","Nitrate as N"), path){

compiled_data = list()

# Workbook styles
if(create_workbooks){
  Identifier = openxlsx::createStyle(textDecoration = "bold", bgFill = "yellow")
  IR = openxlsx::createStyle(textDecoration = "bold", bgFill = "pink")
  Param = openxlsx::createStyle(textDecoration = "bold", bgFill = "turquoise")
}
# Function for creating Excel workbooks for specified parameters
param_exp <- function(x){
  
  alldat = subset(all_data1, all_data1$CharacteristicName==x|all_data1$R3172ParameterName==x)
  summdat = subset(summary_tc_assessed, summary_tc_assessed$R3172ParameterName==x)
  
  reviewer_export <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(reviewer_export, sheetName = "Summary")
  openxlsx::addWorksheet(reviewer_export, sheetName = "Data")
  openxlsx::writeDataTable(reviewer_export, sheet = "Data", alldat)
  openxlsx::writeDataTable(reviewer_export, sheet = "Summary", summdat)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "IR", type = "contains", style = IR)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "Param", type = "contains",style = Param)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "IR", type = "contains", style = IR)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
  openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "Param", type = "contains",style = Param)
  if(include_rejected){
    rej_data1 = subset(rejected_data, rejected_data$CharacteristicName==x)
    rej_data2 = subset(rejected_data, rejected_data$R3172ParameterName==x)
    rej_dat = merge(rej_data1, rej_data2, all = TRUE)
    openxlsx::addWorksheet(reviewer_export, sheetName = "Rejected")
    openxlsx::writeDataTable(reviewer_export, sheet = "Rejected", rej_dat)
    openxlsx::conditionalFormatting(reviewer_export, sheet = "Rejected", cols = 1:285, rows = 1, rule = "IR", type = "contains", style = IR)
    openxlsx::conditionalFormatting(reviewer_export, sheet = "Rejected", cols = 1:285, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
    openxlsx::conditionalFormatting(reviewer_export, sheet = "Rejected", cols = 1:285, rows = 1, rule = "Param", type = "contains",style = Param)
  }
  openxlsx::saveWorkbook(reviewer_export, file = paste0(x,"_IR_data_summary.xlsx"))
  rm(list = ls())
  gc()
}

### Load watershed management units
load(system.file("extdata", "wmus_aus.Rdata", package = "irTools"))
# wmus = unique(au_wmu)

### Upload export translation workbook
exp_file=system.file("extdata", "IR_export_translations.xlsx", package = "irTools")
exp_wb = openxlsx::loadWorkbook(exp_file)

# Read in columns from translation workbook
columns = openxlsx::readWorkbook(exp_wb, sheet = 1)
abbrev_cols = columns$COL_KEEP[columns$SHEET=="DA"]
summ_cols = columns$COL_KEEP[columns$SHEET=="DS"]

#### HANDLING ACCEPTED DATA ####

# With all accepted data, remove CF rows
dat_accepted = prepped_data$export_data[!prepped_data$export_data$BeneficialUse=="CF",]
dat_accepted$IR_DataPrep_FLAG = "ACCEPT"


# Retain only toxic and conventional records and add WMU
#tox_conv = subset(dat_accepted, dat_accepted$AssessmentType=="Toxic"|dat_accepted$AssessmentType=="Conventional")
tox_conv = subset(dat_accepted, dat_accepted$AssessmentType=="Toxic"|dat_accepted$AssessmentType=="Conventional"|dat_accepted$AssessmentType=="All")
before = dim(tox_conv)[1]
tox_conv = merge(tox_conv, wmus, all.x = TRUE)
after = dim(tox_conv)[1]

before==after

# Ensure criteria are numeric
tox_conv$NumericCriterion = as.numeric(tox_conv$NumericCriterion)
tox_conv$IR_Value = as.numeric(tox_conv$IR_Value)

# Create exceeds column
tox_conv$Exceeds = 0

# Determine if exceeds based on criterion type
tox_conv1=within(tox_conv, {
  Exceeds[CriterionType=="max" & IR_Value > NumericCriterion]=1
  Exceeds[CriterionType=="min" & IR_Value < NumericCriterion]=1
})
table(tox_conv1$Exceeds)

col_order = abbrev_cols[abbrev_cols%in%names(tox_conv1)]

all_data1 = tox_conv1[,col_order]

# Summary data
summary_tc_assessed = plyr::rbind.fill(conventionals_assessed, toxics_assessed)
names(summary_tc_assessed)[names(summary_tc_assessed)=="SampleCount"] = "MLIDSampleCount"
names(summary_tc_assessed)[names(summary_tc_assessed)=="ExcCount"] = "MLIDExceedanceCount"
names(summary_tc_assessed)[names(summary_tc_assessed)=="SSC_MLID"] = "siteSpecificAssessment"

# Add the following columns for clarity in summary tab of export
summary_tc_assessed$MonitoringLocationIdentifier = summary_tc_assessed$IR_MLID
summary_tc_assessed$IR_Screen_FLAG = "ACCEPT"
summary_tc_assessed$IR_DataPrep_FLAG = "ACCEPT"
summary_tc_assessed = merge(summary_tc_assessed, wmus, all.x = TRUE)

col_order2 = summ_cols[summ_cols%in%names(summary_tc_assessed)]
summary_tc_assessed = summary_tc_assessed[,col_order2]

compiled_data$toxconv_data_asmnt = all_data1
compiled_data$summary_tc_assessed = summary_tc_assessed

if(create_workbooks){
  setwd(path)
  lapply(params, FUN=param_exp)
  
  # restoparams = unique(c(as.character(all_data1$CharacteristicName[!all_data1$CharacteristicName%in%params]),as.character(all_data1$R3172ParameterName[!all_data1$R3172ParameterName%in%params])))
  # restodat = subset(all_data1, all_data1$CharacteristicName%in%restoparams|all_data1$R3172ParameterName%in%restoparams)
  # restosummdat = subset(summary_tc_assessed, summary_tc_assessed$R3172ParameterName%in%restoparams)
  # reviewer_export <- openxlsx::createWorkbook()
  # openxlsx::addWorksheet(reviewer_export, sheetName = "Summary")
  # openxlsx::addWorksheet(reviewer_export, sheetName = "Data")
  # openxlsx::writeDataTable(reviewer_export, sheet = "Data", alldat)
  # openxlsx::writeDataTable(reviewer_export, sheet = "Summary", summdat)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "IR", type = "contains", style = IR)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Data", cols = 1:120, rows = 1, rule = "Param", type = "contains",style = Param)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "IR", type = "contains", style = IR)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "Identifier", type = "contains",style = Identifier)
  # openxlsx::conditionalFormatting(reviewer_export, sheet = "Summary", cols = 1:30, rows = 1, rule = "Param", type = "contains",style = Param)
  # openxlsx::saveWorkbook(reviewer_export, file = paste0(x,"_IR_data_summary.xlsx"))
  
  
  }

return(compiled_data)
}

# Isolate lake profiles from accepted data
#lake_profs = dat_accepted[!is.na(dat_accepted$DataLoggerLine) & dat_accepted$BeneficialUse %in% c("3A","3B","3C","3D","3E"),] # EH: in test, 400 records fit this bill, but see below when remove logger line records.

# profs_assessed = irTools::assessLakeProfiles(lake_profs)
# 
# # Profile data tab
# prof_data_asmnt = merge(lake_profs, profs_assessed$profile_asmnts_individual, all = TRUE)
# 
# # columns of interest
# prof_data_asmnt = prof_data_asmnt[,names(prof_data_asmnt)%in%abbrev_cols]
# 
# openxlsx::addWorksheet(reviewer_export, sheetName = "Lake Profile Data")
# openxlsx::writeData(reviewer_export, sheet = "Lake Profile Data", prof_data_asmnt)

# Summary data for lake profiles *** missing criteria, dates, exceedance count
# prof_summ_data = profs_assessed$profile_asmnts_mlid_param

# Isolate trophic data
# lake_troph = dat_accepted[dat_accepted$AU_Type=="Reservoir/Lake" & dat_accepted$R3172ParameterName %in% c("Chlorophyll a", "Total Phosphorus as P","Depth, Secchi disk depth"),]

# columns of interest
#trophic_data_asmnt = trophic_data_asmnt[,names(trophic_data_asmnt)%in%abbrev_cols]

#openxlsx::addWorksheet(reviewer_export, sheetName = "Lake Trophic Data")
#openxlsx::writeData(reviewer_export, sheet = "Lake Trophic Data", trophic_data_asmnt)

# # Isolate ecoli data
# ecoli = dat_accepted[dat_accepted$R3172ParameterName=="E. coli",]
# 
# if(length(prepped_data$ecoli$OrganizationIdentifier)>0){
#   
#   ecoli_asmnt = irTools::assessEColi(prepped_data$ecoli) 
#   
#   # Merge to aggregated daily data
#   ecoli_data_asmnt = merge(ecoli, ecoli_asmnt$assessed_data, all.x = TRUE) # EH: could add other aspects of ecoli assessments, too.
#   
#   # Determine exceedances
#   ecoli_data_asmnt$Exceeds = ifelse(ecoli_data_asmnt$IR_Value>ecoli_data_asmnt$NumericCriterion, 1, 0)
#   
#   # columns of interest
#   ecoli_data_asmnt = ecoli_data_asmnt[,names(ecoli_data_asmnt)%in%abbrev_cols]
#   
#   compiled_data$ecoli_data_asmnt = ecoli_data_asmnt 
#   
# }else{warning("No E.coli data detected in prepped dataset.")}

### Isolate toxics and conventionals from other data in accepted dataset
# # Remove lake profile data from accepted dataset
# dat_accepted1 = dat_accepted[is.na(dat_accepted$DataLoggerLine),] # EH: removes 1000 records (likely bc associated with uses that are not part of the profiles assessment)
# dim(dat_accepted1)
# 
# # Remove lake trophic data from accepted dataset
# dat_accepted2 = suppressMessages(dplyr::anti_join(dat_accepted1, lake_troph))
# dim(dat_accepted2)
# 
# # Remove ecoli data, leaving just toxics and conventionals left - this df has more rows than result$tox and result$conventionals bc has not been aggregated to daily values yet.
# tox_conv = suppressMessages(dplyr::anti_join(dat_accepted2, ecoli))
# dim(tox_conv)

# # Remove numeric criterion from tox conv bc those are updated (or duplicated) in the aggregated datasets
# tox_conv1 = tox_conv[,!names(tox_conv)%in%"NumericCriterion"]
# table(tox_conv1$AssessmentType)
# 
# # Bind AGGREGATED tox and conv ds together and rename IR_Value column
# prepped_data$toxics$NumericCriterion = wqTools::facToNum(prepped_data$toxics$NumericCriterion)
# prepped_data$conventionals$NumericCriterion = wqTools::facToNum(prepped_data$conventionals$NumericCriterion)
# 
# dim(prepped_data$toxics) + dim(prepped_data$conventionals)
# 
# aggreg_tox_conv = plyr::rbind.fill(prepped_data$toxics, prepped_data$conventionals)
# dim(aggreg_tox_conv)
# 
# names(aggreg_tox_conv)[names(aggreg_tox_conv)=="IR_Value"] = "IR_Aggreg_Value"
# 
# # Merge aggregated to non-aggreg
# tox_conv2 = merge(tox_conv1, aggreg_tox_conv, all = TRUE)
# dim(tox_conv1)
# dim(tox_conv2) # should match
#head(tox_conv2[tox_conv2$Exceeds==1,])

# columns of interest
# toxconv_data_asmnt = tox_conv2[,names(tox_conv2)%in%abbrev_cols]

# # Remove numeric criterion from tox conv bc those are updated (or duplicated) in the aggregated datasets
# tox_conv = dat_accepted1[,!names(dat_accepted1)%in%"NumericCriterion"]
# table(tox_conv$AssessmentType)
# 
# # Bind AGGREGATED tox and conv ds together and rename IR_Value column
# prepped_data$toxics$NumericCriterion = wqTools::facToNum(prepped_data$toxics$NumericCriterion)
# prepped_data$conventionals$NumericCriterion = wqTools::facToNum(prepped_data$conventionals$NumericCriterion)
# 
# dim(prepped_data$toxics) + dim(prepped_data$conventionals)
# 
# aggreg_tox_conv = plyr::rbind.fill(prepped_data$toxics, prepped_data$conventionals)
# dim(aggreg_tox_conv)
# 
# names(aggreg_tox_conv)[names(aggreg_tox_conv)=="IR_Value"] = "IR_Aggreg_Value"
# 
# # Merge aggregated to non-aggreg
# tox_conv2 = merge(tox_conv1, aggreg_tox_conv, all = TRUE)
# dim(tox_conv1)
# dim(tox_conv2) # should match
# head(tox_conv2[tox_conv2$Exceeds==1,])

# prepped_data$toxics$NumericCriterion = wqTools::facToNum(prepped_data$toxics$NumericCriterion)
# prepped_data$conventions$NumericCriterion = wqTools::facToNum(prepped_data$conventionals$NumericCriterion)
# 
# toxics_exc = irTools::countExceedances(prepped_data$toxics, group_vars = c("IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","BEN_CLASS","R3172ParameterName","AssessmentType","CriterionLabel","SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun"))
# toxics_exc_assessed = irTools::assessExcCounts(toxics_exc,min_n = 4, max_exc_count = 2, max_exc_count_id = 1)
# 
# conv_exc = irTools::countExceedances(prepped_data$conventionals, group_vars = c("IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","BEN_CLASS","R3172ParameterName","AssessmentType","CriterionLabel","SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun"))
# conv_exc_assessed = irTools::assessExcCounts(conv_exc,min_n = 10, max_exc_pct = 10, max_exc_count_id = 2)
# 
# # Combine summary data
# summary_tc_assessed = plyr::rbind.fill(conv_exc_assessed, toxics_exc_assessed)
# 
# summary_tc_assessed = summary_tc_assessed[,!names(summary_tc_assessed)%in%c("SSC_StartMon","SSC_EndMon","AsmntAggFun")]
