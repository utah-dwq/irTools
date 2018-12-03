#' Scale up toxics and conventionals assessments to Assessment Units
#'
#' Compares water quality result values to standards to calculates sample and exceedance counts. This is geared towards conventional and toxic assessments.
#' 
#' @param data A prepped list of water quality portal data objects with exceedances counted and assessed for each site, use, and R3172 parameter.
#' @param subs Vector of column names on which to subset data to determine AU assessment category. Defaults to subset by ASSESS_ID, BeneficialUse, and R3172ParameterName.
#' @return Returns dataframe with assessment categories for each AU/BenUse/R3172ParameterName.
#' @importFrom plyr rbind.fill
#' @importFrom plyr ddply
#' @export


auRollUp <- function(data,subs=c("ASSESS_ID","BeneficialUse","R3172ParameterName")){

# Testing
# data=list(toxics_assessed,conv_assessed) # compile these into a list in assessIR script?
# subs=c("ASSESS_ID","BeneficialUse","R3172ParameterName")

# Combine all assessed data into one dataframe for roll up
dat_all <- plyr::rbind.fill(data)

# ****PLACE HOLDER****
# Read in list of sites with TMDLs, and redefine IR_Cat for those sites as "TA" (TMDL approved)

# Represent categories numerically so we can select the "max" category to define the AU
# Hierarchy of decision making within each subset: NS>TA>idE>idNE>FS
dat_all$AU_Cat[dat_all$IR_Cat=="NS"]<-5
dat_all$AU_Cat[dat_all$IR_Cat=="TA"]<- 4
dat_all$AU_Cat[dat_all$IR_Cat=="idE"]<-3
dat_all$AU_Cat[dat_all$IR_Cat=="idNE"]<-2
dat_all$AU_Cat[dat_all$IR_Cat=="FS"]<-1

# Turn subs into a formula argument
subs_eq <- paste(subs, collapse="+")
# Convert subsetting convention to formula for input to aggregate function
eq <- as.formula(paste("AU_Cat", subs_eq, sep="~"))

# Aggregate to AU by subs 
rollup <- aggregate(eq,dat_all,max)
rollup <- merge(rollup, unique(dat_all[,c("AU_NAME","ASSESS_ID","AU_Cat","IR_Cat")]), all.x=TRUE)
return(rollup)

}