#' Run lake profile assessments
#'
#' Performs lake profile assessments per IR assessment methods. This includes identifying stratified profiles & applying appropriate assessment methods for stratified & non stratified profiles.
#'
#' @param data Lake profiles object returned by dataPrep step.
#' @param uses_assessed Vector of beneficial uses to be assessed for lake profiles. Defaults to 3A & 3B uses.
#' @param do_crit List of beneficial use classes and associated dissolved oxygen criteria to use for assessment. Defaults to list("3A"=5, "3B"=3). This excludes chronic & ELS present criteria per assessment methods. Sites with site specific criteria will be assessed regardless of criteria specified in this argument. Objects in this list should match the uses_assessed argument.
#' @param temp_crit List of beneficial use classes and associated water temperature criteria to use for assessment. Defaults to list("3A"=20, "3B"=27). This excludes chronic & ELS present criteria per assessment methods. Sites with site specific criteria will be assessed regardless of criteria specified in this argument. Objects in this list should match the uses_assessed argument.
#' @return Returns a list of lake profile assessment dataframes. profile_asmnts_mlid_param contains site/parameter level profile assessments, profile_asmnts_individual contains assessments for each individual profile,
#' 	profile_criteria contains the criteria used for the profile assessment (excluding any site-specific criteria that may have occurred in the input dataset), 
#'  profiles_long contains profile data in long format including the numeric criterion associated with each parameter, profiles_wide contains profile data cast to wide format.
#' @importFrom reshape2 dcast
#' @importFrom dplyr rename
#' @importFrom wqTools facToNum
#' @importFrom rLakeAnalyzer thermo.depth
#' @importFrom plyr ddply

#' @export
assessLakeProfiles <- function(data, do_crit=list("3A"=4, "3B"=3), temp_crit=list("3A"=20, "3B"=27), uses_assessed=c("3A","3B")){

##### Testing setup
#load("P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\prepped_data.rdata")
#data=prepped_data$lake_profiles
#uses_assessed=c("3A","3B")
#do_crit=list("3A"=4, "3B"=3)
#temp_crit=list("3A"=20, "3B"=27)
#####

# Make numeric criterion numeric
if(class(data$NumericCriterion)=="character"){data$NumericCriterion=as.numeric(data$NumericCriterion)}
if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=wqTools::facToNum(data$NumericCriterion)}

# Subset data to desired criteria & uses
do_crit=data.frame(unlist(do_crit))
do_crit$BeneficialUse=row.names(do_crit)
do_crit$R3172ParameterName="Minimum Dissolved Oxygen"
names(do_crit)[names(do_crit)=="unlist.do_crit."]="input_crit"

temp_crit=data.frame(unlist(temp_crit))
temp_crit$BeneficialUse=row.names(temp_crit)
temp_crit$R3172ParameterName="Temperature, water"
names(temp_crit)[names(temp_crit)=="unlist.temp_crit."]="input_crit"

input_crit=rbind(do_crit, temp_crit)
data_sub=data
data_sub=merge(data_sub,input_crit, all.x=T)

data_sub=subset(data_sub, (is.na(input_crit) | NumericCriterion == input_crit | !is.na(SSC_StartMon)) & BeneficialUse %in% uses_assessed)
data_sub=data_sub[,names(data_sub)!="input_crit"]

# Pull out criteria for future usage
crit=data.frame(unique(data_sub[!is.na("NumericCriterion"),c("BeneficialUse", "R3172ParameterName", "CriterionUnits", "NumericCriterion")]))

# Identify exceedances
data_exc=data_sub
data_exc$exc=0

data_exc=within(data_exc, {
	exc[CriterionType=="max" & IR_Value > NumericCriterion]=1
	exc[CriterionType=="min" & IR_Value < NumericCriterion]=1
	})

# Cast to wide (note - values averaged when more than 1 value per line recorded)
profs_long=unique(data_exc[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","IR_MLID","IR_MLNAME","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","AU_Type",
							  "BeneficialUse","BEN_CLASS","R3172ParameterName","IR_Value","IR_Unit","NumericCriterion","exc")])
profs_wide=reshape2::dcast(DataLoggerLine+ActivityIdentifier+ActivityStartDate+IR_MLID+IR_MLNAME+R317Descrp+IR_Lat+IR_Long+ASSESS_ID+AU_NAME+AU_Type+BeneficialUse+BEN_CLASS~R3172ParameterName,
					data=profs_long, value.var="IR_Value", fun.aggregate=mean, na.rm=T)
exc_wide=reshape2::dcast(DataLoggerLine+ActivityIdentifier+ActivityStartDate+IR_MLID+IR_MLNAME+R317Descrp+IR_Lat+IR_Long+ASSESS_ID+AU_NAME+AU_Type+BeneficialUse+BEN_CLASS~R3172ParameterName,
					data=profs_long, value.var="exc", fun.aggregate=max, fill=NaN)

exc_wide=exc_wide[,names(exc_wide)!="Profile depth"]
exc_wide=dplyr::rename(exc_wide,do_exc="Minimum Dissolved Oxygen", pH_exc=pH, temp_exc="Temperature, water")

profs_exc=merge(profs_wide,exc_wide,all=T)

names(profs_exc)=make.names(names(profs_exc))

# Calculate thermocline depths
calcTdepth=function(x){
	if(any(!is.na(x$Profile.depth)) & any(!is.na(x$Temperature..water))){
		x_agg=aggregate(Temperature..water~Profile.depth, x, FUN='mean')
		return(rLakeAnalyzer::thermo.depth(x_agg$Temperature..water, x_agg$Profile.depth))
	}else{return("NaN")}
}

t_profs=profs_exc[,c("BeneficialUse","ActivityIdentifier","ActivityStartDate","IR_MLID","ASSESS_ID","Profile.depth","Temperature..water")]

charToFac=function(x){
	if(class(x) == 'character'){
		y=as.factor(x)
	}else{y=x}
	return(y)
}

thermo_depths=plyr::ddply(t_profs,
						  c("BeneficialUse","ActivityIdentifier","ActivityStartDate","IR_MLID","ASSESS_ID"),
						  .fun=calcTdepth)
thermo_depths=dplyr::rename(tc_depth_m="V1", thermo_depths)
thermo_depths$stratified = 0
thermo_depths$stratified[thermo_depths$tc_depth_m!="NaN"]=1


# Merge thermocline depths back to profiles
profs_exc=merge(profs_exc,thermo_depths, all.x=T)

# Determine DO + temp exc
profs_exc$do_temp_exc=0
profs_exc$do_temp_exc[profs_exc$do_exc==1 | profs_exc$temp_exc==1]=1

#######
#x=profs_exc[profs_exc$ActivityIdentifier=="UTAHDWQ_WQX-BORFG061615-4938550-0617-Pr-F",]
#######

assessOneProfile=function(x){
	x=x[order(x$Profile.depth),]
	x$Profile.depth[x$DataLoggerLine=="1-Line 1"]=0
	samp_count=dim(x)[1]
	pct10=ceiling(dim(x)[1] *0.1)
	do_exc_cnt=sum(x$do_exc)
	temp_exc_cnt=sum(x$temp_exc)
	pH_exc_cnt=sum(x$pH_exc)
	pH_asmnt=ifelse(pH_exc_cnt<=pct10,"FS","NS")
	profile_depth=max(x$Profile.depth)
	
	rles=rle(x$do_temp_exc)
	strat=data.frame(rles$lengths, rles$values)
	strat=within(strat,{
		bottom_index=cumsum(rles$lengths)
		bottom_depth=x$Profile.depth[bottom_index]
		top_index=bottom_index-rles.lengths+1
		top_depth=x$Profile.depth[top_index]
		layer_width=bottom_depth-top_depth
	})
	
	if(any(strat$rles.values==0)){
		max_hab_width=max(strat$layer_width[strat$rles.values==0])
	}else{max_hab_width=0}
	if(x$stratified[1]==1 & max(x$Profile.depth)>3){ #stratified
		do_temp_asmnt=ifelse(max_hab_width>=3, "FS", "NS")
		do_asmnt=as.factor(NA)
		temp_asmnt=as.factor(NA)
	
	}else{ #non-stratified
		do_temp_asmnt=as.factor(NA)
		temp_asmnt=ifelse(temp_exc_cnt>pct10 & temp_exc_cnt>=2,"NS","FS")
		do_asmnt=ifelse(do_exc_cnt>pct10 & do_exc_cnt>=2,"NS","FS")
	}

	asmnt=data.frame(max_hab_width,do_temp_asmnt,do_exc_cnt,do_asmnt,temp_exc_cnt,temp_asmnt,pH_exc_cnt,pH_asmnt,samp_count,pct10,profile_depth)
	
	return(asmnt)
}

profile_asmnts=plyr::ddply(profs_exc,
						  c("BeneficialUse","BEN_CLASS","ActivityIdentifier","ActivityStartDate","IR_MLID","IR_MLNAME","ASSESS_ID","AU_NAME","R317Descrp","IR_Lat","IR_Long"),
						  .fun=assessOneProfile)

profile_asmnts=merge(profile_asmnts, thermo_depths)

profile_asmnts[c("do_temp_asmnt","do_asmnt","temp_asmnt","pH_asmnt")]=lapply(profile_asmnts[c("do_temp_asmnt","do_asmnt","temp_asmnt","pH_asmnt")], factor, 
            levels=c("NS","FS","idE","idNE"))

# Flatten assessments
profile_asmnts_flat=reshape2::melt(profile_asmnts,nar.rm=T,value.name="IR_Cat",
								   id.vars=c("BeneficialUse","BEN_CLASS","ActivityIdentifier","ActivityStartDate","IR_MLID","IR_MLNAME","ASSESS_ID","AU_NAME","R317Descrp","IR_Lat","IR_Long"),
								   measure.vars=c("do_temp_asmnt","do_asmnt","temp_asmnt","pH_asmnt"))


# Rename Variables
profile_asmnts_flat=within(profile_asmnts_flat,{
	R3172ParameterName=as.character(NA)
	R3172ParameterName[variable=="do_temp_asmnt"]="DO-temperature habitat profile width"
	R3172ParameterName[variable=="do_asmnt"]="Minimum Dissolved Oxygen"
	R3172ParameterName[variable=="temp_asmnt"]="Temperature, water"
	R3172ParameterName[variable=="pH_asmnt"]="pH"
})
profile_asmnts_flat=profile_asmnts_flat[,names(profile_asmnts_flat)!="variable"]


# Roll up to site level (MLID, use, param), removing ActivityIdentifier & date
profile_asmnts_rolledUp=irTools::rollUp(data=list(profile_asmnts_flat), group_vars=c("ASSESS_ID","AU_NAME","R317Descrp","BEN_CLASS","IR_MLID","IR_MLNAME","IR_Lat","IR_Long","BeneficialUse","R3172ParameterName"), expand_uses=F, print=F)
names(profile_asmnts_rolledUp)[names(profile_asmnts_rolledUp)=="AssessCat"]="IR_Cat"

# Gather objects to return
profile_asmnts_individual=profile_asmnts
profs_exc=dplyr::rename(profs_exc,DO_mgL="Minimum.Dissolved.Oxygen", Temp_degC="Temperature..water", Depth_m="Profile.depth",Thermocline_depth_m="tc_depth_m")
profiles_wide=profs_exc

result=list(profile_asmnts_mlid_param=profile_asmnts_rolledUp,profile_asmnts_individual=profile_asmnts_individual,profiles_wide=profiles_wide, profiles_long=profs_long, profile_criteria=crit)

return(result)

}

