# Read in data

### TESTING ###
# See "P:\WQ\Integrated Report\Automation_Development\elise\AU_export_testing\au_export_testing_steps.R" for steps taken up to dataPrep
load("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\data\\au_export_test_data.RData")

# Data with all columns, remove CF rows
dat_accepted = result$acc_data[!result$acc_data$BeneficialUse=="CF",]
dim(dat_accepted)

# Split off lake profiles, lake trophic, and ecoli, leaving toxics and conventionals
lake_profs = dat_accepted[!is.na(dat_accepted$DataLoggerLine) & dat_accepted$BeneficialUse %in% c("3A","3B","3C","3D","3E"),] # in test, is 400 records
dim(lake_profs)

# Remove lake profile data from accepted dataset
dat_accepted1 = dat_accepted[is.na(dat_accepted$DataLoggerLine),] # removes 1000 records (likely bc associated with uses that are not part of the profiles assessment)
dim(dat_accepted1)

# Isolate lake trophic data from original ds too
lake_troph = dat_accepted1[dat_accepted1$AU_Type=="Reservoir/Lake" & dat_accepted1$R3172ParameterName %in% c("Chlorophyll a", "Total Phosphorus as P","Depth, Secchi disk depth"),]

# Remove lake trophic data from accepted dataset
dat_accepted2 = dplyr::anti_join(dat_accepted1, lake_troph)
dim(dat_accepted2)

# Isolate E.coli
ecoli = dat_accepted2[dat_accepted2$R3172ParameterName=="E. coli",]

# Remove ecoli data, leaving just toxics and conventionals left
tox_conv = dplyr::anti_join(dat_accepted2, ecoli)
dim(tox_conv)

# Remove numeric criterion from tox conv bc those are updated (or duplicated) in the aggregated datasets
tox_conv1 = tox_conv[,!names(tox_conv)%in%"NumericCriterion"]

# Bind AGGREGATED tox and conv ds together and rename IR_Value column
result$toxics$NumericCriterion = wqTools::facToNum(result$toxics$NumericCriterion)
result$conventionals$NumericCriterion = wqTools::facToNum(result$conventionals$NumericCriterion)

dim(result$toxics) + dim(result$conventionals)

aggreg_tox_conv = plyr::rbind.fill(result$toxics, result$conventionals)
dim(aggreg_tox_conv)

names(aggreg_tox_conv)[names(aggreg_tox_conv)=="IR_Value"] = "IR_Aggreg_Value"

# Merge aggregated to non-aggreg
tox_conv2 = merge(tox_conv1, aggreg_tox_conv, all = TRUE)
dim(tox_conv1)
dim(tox_conv2) # should match

# Rbind.fill all other objects to tox_conv2
data_allcols = plyr::rbind.fill(tox_conv2, ecoli, lake_troph, lake_profs)

# Add watershed management unit
wmus = openxlsx::read.xlsx("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\au_wmu.xlsx")

wmus = wmus[,c("ASSESS_ID","Mgmt_Unit")]

data_abbrevcols1 = merge(data_abbrevcols, wmus, all.x = TRUE)


# Read in export translation workbook
exp_wb = openxlsx::loadWorkbook("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\IR_export_translations.xlsx")

# Read in columns from translation workbook
columns = openxlsx::readWorkbook(exp_wb, sheet = 1)
abbrev_cols = columns$COL_KEEP[columns$SHEET=="DA"]
summ_cols = columns$COL_KEEP[columns$SHEET=="DS"]

# Find intersecting/non-intersecting columns
all_col_names = colnames(data_allcols)

intersecting_cols = all_col_names[all_col_names%in%abbrev_cols]

non_intersecting_cols = abbrev_cols[!abbrev_cols%in%intersecting_cols]

# Obtain columns of interest from pre-split prepped data
data_abbrevcols = data_allcols[,c(intersecting_cols)]

write.csv(data_abbrevcols1, "P:\\WQ\\Integrated Report\\Automation_Development\\elise\\AU_export_testing\\data_export.csv", row.names = FALSE)


