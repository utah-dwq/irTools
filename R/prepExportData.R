# Read in data

### TESTING ###
# See "P:\WQ\Integrated Report\Automation_Development\elise\AU_export_testing\au_export_testing_steps.R" for steps taken up to dataPrep
load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\data\\au_export_test_data.RData")

# alldata = plyr::rbind.fill(result$acc_data, result$rej_data)
# write.csv(alldata, "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\data\\alldata_fromdataprep.csv", row.names = FALSE)

wmus = openxlsx::read.xlsx("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\au_wmu.xlsx")

# Read in export translation workbook
exp_wb = openxlsx::loadWorkbook("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\IR_export_translations.xlsx")

# Read in columns from translation workbook
columns = openxlsx::readWorkbook(exp_wb, sheet = 1)
abbrev_cols = columns$COL_KEEP[columns$SHEET=="DA"]
summ_cols = columns$COL_KEEP[columns$SHEET=="DS"]

# Find intersecting/non-intersecting columns
all_data_cols = colnames(result$acc_data)

intersecting_cols = all_data_cols[all_data_cols%in%abbrev_cols]

non_intersecting_cols = abbrev_cols[!abbrev_cols%in%intersecting_cols]

# Obtain columns of interest from pre-split prepped data
pre.split.cols_a = result$acc_data[,c(intersecting_cols)]
pre.split.cols_r = result$rej_data[,c(intersecting_cols)]
pre.split.cols_all = rbind.fill(pre.split.cols_a,pre.split.cols_r)

# Bring in result data and merge together - add in other assessments, too??
prep.dat = plyr::rbind.fill(result$lake_profiles,result$lakes_trophic, result$ecoli, result$toxics, result$agg_tds, result$conventionals)

# Merge prep.dat with pre.split.cols - appears to expand dataset
dat.all.cols = merge(pre.split.cols_all, prep.dat, all = TRUE)           

# 

# Functions to draw from:
# Data prep
# HF DO
# Ecoli
# Lake profiles

