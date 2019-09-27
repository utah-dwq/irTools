#' Create dataset for secondary reviewer export from assessment dashboard
#'
#' Pulls columns of interest from prepped data and calculates basic exceedance summaries for site-use-parameter assessments.
#' @param prepped_data A list of objects produced by dataPrep function, including toxics, conventionals, and ecoli dataframes.
#' @return A list composed of three dataframes: site-date-use-param records linked with aggregated daily values (if applicable), calculations, and exceedances, site-use-param summaries including sample and exceedance counts, and prepped E.coli data.
#' @importFrom dplyr anti_join
#' @importFrom plyr rbind.fill
#' @import openxlsx

### TESTING ###
# See "P:\WQ\Integrated Report\Automation_Development\elise\AU_export_testing\au_export_testing_steps.R" for steps taken up to dataPrep
# load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\au_export_resultdata.RData")
# prepped_data = result

#' @export
composeExport <- function(prepped_data){

compiled_data = list()

### Load watershed management units
# wmus = openxlsx::read.xlsx("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\au_wmu.xlsx")
# wmus = wmus[,c("ASSESS_ID","Mgmt_Unit")]
# save(wmus, file = "C:\\Users\\ehinman\\Documents\\GitHub\\irTools\\inst\\extdata\\wmus_aus.RData")

load(system.file("extdata", "wmus_aus.Rdata", package = "irTools"))
wmus = unique(wmus)

### Upload export translation workbook
exp_file=system.file("extdata", "IR_export_translations.xlsx", package = "irTools")
exp_wb = openxlsx::loadWorkbook(exp_file)

# Read in columns from translation workbook
columns = openxlsx::readWorkbook(exp_wb, sheet = 1)
abbrev_cols = columns$COL_KEEP[columns$SHEET=="DA"]
summ_cols = columns$COL_KEEP[columns$SHEET=="DS"]

# With all accepted data, remove CF rows and add WMU
dat_accepted = prepped_data$export_data[!prepped_data$export_data$BeneficialUse=="CF",]

dat_accepted = merge(dat_accepted, wmus, all.x = TRUE)

dim(dat_accepted)

# Lake profiles and trophic data have already been removed from acc_data, so just need to remove E.coli
dat_accepted1 = dat_accepted[!dat_accepted$R3172ParameterName=="E. coli",]

# Create exceeds column
dat_accepted1$Exceeds = 0

# Determine if exceeds based on criterion type
dat_accepted1=within(dat_accepted1, {
  Exceeds[CriterionType=="max" & IR_Value > NumericCriterion]=1
  Exceeds[CriterionType=="min" & IR_Value < NumericCriterion]=1
})
table(dat_accepted1$Exceeds)

compiled_data$toxconv_data_asmnt = dat_accepted1

# Summary data
toxics_exc = irTools::countExceedances(prepped_data$toxics, group_vars = c("IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","BEN_CLASS","R3172ParameterName","AssessmentType","CriterionLabel", "SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun"))
toxics_exc_assessed = irTools::assessExcCounts(toxics_exc,min_n = 4, max_exc_count = 2, max_exc_count_id = 1)

conv_exc = irTools::countExceedances(prepped_data$conventionals, group_vars = c("IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","BeneficialUse","BEN_CLASS","R3172ParameterName","AssessmentType","CriterionLabel","SSC_MLID","SSC_StartMon","SSC_EndMon","AsmntAggFun"))
conv_exc_assessed = irTools::assessExcCounts(conv_exc,min_n = 10, max_exc_pct = 10, max_exc_count_id = 2)

# Combine summary data
summary_tc_assessed = plyr::rbind.fill(conv_exc_assessed, toxics_exc_assessed)

summary_tc_assessed = summary_tc_assessed[,!names(summary_tc_assessed)%in%c("SSC_StartMon","SSC_EndMon","AsmntAggFun")]

names(summary_tc_assessed)[names(summary_tc_assessed)=="SampleCount"] = "MLIDSampleCount"
names(summary_tc_assessed)[names(summary_tc_assessed)=="ExcCount"] = "MLIDExceedanceCount"
names(summary_tc_assessed)[names(summary_tc_assessed)=="SSC_MLID"] = "siteSpecificAssessment"

compiled_data$summary_tc_assessed = summary_tc_assessed

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