#' Scale up toxics and conventionals assessments to Assessment Units
#'
#' Compares water quality result values to standards to calculates sample and exceedance counts. This is geared towards conventional and toxic assessments.
#'
#' @param data A prepped list of water quality portal data objects with exceedances counted and assessed for each site, use, and R3172 parameter. Will likely contain toxics assessed, conventionals assessed, lakes assessed, etc.
#' @param group_vars Vector of column names on which to group data for assessment rollups. Defaults to subset by ASSESS_ID, BeneficialUse, and R3172ParameterName.
#' @return Returns dataframe with assessment categories for each AU/BenUse/R3172ParameterName.
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 colsplit
#' @export


rollUp <- function(data, group_vars=c("ASSESS_ID","BeneficialUse","R3172ParameterName"), expand_uses=TRUE){ #(JV) I generalized function name to just rollUp. I think we may want to perform rollups at both site & AU spatial scales

# Testing
#data=list(toxics_assessed,conv_assessed) # compile these into a list in assessIR script? - (JV) I think pass as a vector of object names, then use mget to convert to list, then do.call w/ rbind.fill. See below.
data=c("toxics_assessed", "conv_assessed")
group_vars=c("ASSESS_ID","AU_NAME", "BeneficialUse")
expand_uses=TRUE

# Combine all assessed data into one dataframe for roll up
dat=mget(data)
dat_all=do.call(plyr::rbind.fill, dat)


if(expand_uses & !"BEN_CLASS" %in% group_vars){group_vars=append(group_vars, "BEN_CLASS")}


# ****PLACE HOLDER****
# Read in list of sites with TMDLs, and redefine IR_Cat for those sites as "TMDLa" (TMDL approved)

# Represent categories numerically so we can select the "max" category to define the AU
# Hierarchy of decision making within each subset: NS>TMDLa>idE>idNE>FS
dat_all$AssessCat[dat_all$IR_Cat=="NS"]<-5
#dat_all$AssessCat[dat_all$IR_Cat=="TMDLa"]<- 4 - (JV) turning off TMDL approved for now. Not sure if we want to include this here yet or as a sort of "secondary review" type step
dat_all$AssessCat[dat_all$IR_Cat=="idE"]<-3
dat_all$AssessCat[dat_all$IR_Cat=="idNE"]<-2
dat_all$AssessCat[dat_all$IR_Cat=="FS"]<-1

#(JV) Nice! I was thinking a custom function then ddply, but this is simpler.
#FYI - I think we'll want to convert the numbers back to their text equivalents

# Turn group_vars into a formula argument
subs_eq <- paste(group_vars, collapse="+") #(JV) Nice! I never thought to do it this way. I always subset down to the columns I want then use " response~., " in the formula (like in some of the data prep steps)
# Convert subsetting convention to formula for input to aggregate function
eq <- as.formula(paste("AssessCat", subs_eq, sep="~"))

# Aggregate to AU by group_vars
rollup <- aggregate(eq,dat_all,max)

#rollup <- merge(rollup, unique(dat_all[,c("AU_NAME","ASSESS_ID","AssessCat","IR_Cat")]), all.x=TRUE)
#(JV) I think skip this for now and let users define what else is included in output via argument (unless there's a reason to do this separately I'm not thinking of)


#Renaming assessment categories
rollup$AssessCat=as.character(rollup$AssessCat)
rollup=within(rollup,{
	AssessCat[AssessCat=="5"]="NS"
	AssessCat[AssessCat=="4"]="TMDLa"
	AssessCat[AssessCat=="3"]="idE"
	AssessCat[AssessCat=="2"]="idNE"
	AssessCat[AssessCat=="1"]="FS"
})



if(expand_uses){
	#Expand comma separated uses (BEN_CLASS)
	uses=rollup[,group_vars]
	uses$BEN_CLASS=as.character(uses$BEN_CLASS)
	max_use_count=max(sapply(strsplit(uses$BEN_CLASS,","),FUN="length"))
	use_colnames=paste0(rep("use",max_use_count),seq(1:max_use_count))
	uses_mat=unique(data.frame(uses$BEN_CLASS,reshape2::colsplit(uses$BEN_CLASS,",",use_colnames)))
	names(uses_mat)[names(uses_mat)=="uses.BEN_CLASS"]="BEN_CLASS"
	
	#Flatten uses
	uses_flat=reshape2::melt(uses_mat, id.vars="BEN_CLASS", value.name = "BeneficialUse")
	uses_flat=uses_flat[,!names(uses_flat)=="variable"]
	uses_flat=uses_flat[uses_flat$BeneficialUse!="" & !is.na(uses_flat$BeneficialUse),]
	
	uses=uses[,!names(uses) %in% "BeneficialUse"]
	
	uses_expanded=merge(uses,uses_flat, all.x=T, by="BEN_CLASS")
	
	rollup=merge(uses_expanded, rollup, all.x=T)
	rollup$AssessCat[is.na(rollup$AssessCat)]="NA"
}


head(rollup)


if("BeneficialUse" %in% group_vars){
	print("Beneficial use assessment category frequencies:")
	print(table(rollup$BeneficialUse, rollup$AssessCat))
}

if("R3172ParameterName" %in% group_vars){
	print("Impaired parameter frequency:")
	print(table(rollup$R3172ParameterName[rollup$AssessCat=="NS"]))
}




return(rollup)

}



