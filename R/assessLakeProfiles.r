#' Run lake profile assessments
#'
#' Description description description
#'
#' @param data 
#' @return Returns dataframe with assessment categories for each AU/BenUse/R3172ParameterName.

#' @export
assessLakeProfiles <- function(data, do_crit=list("3A"=5, "3B"=3), temp_crit=list("3A"=20, "3B"=27), uses_assessed=c("3A","3B")){

#### Testing setup
load("P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\prepped_data.rdata")
data=prepped_data$lake_profiles
do_crit=list("3A"=5, "3B"=3)
temp_crit=list("3A"=20, "3B"=27)
uses_assessed=c("3A","3B")
####

# Make numeric criterion numeric
if(class(data$NumericCriterion)=="character"){data$NumericCriterion=as.numeric(data$NumericCriterion)}
if(class(data$NumericCriterion)=="factor"){data$NumericCriterion=facToNum(data$NumericCriterion)}

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

data_sub=subset(data_sub, (is.na(input_crit) | NumericCriterion == input_crit) & BeneficialUse %in% uses_assessed)
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
profs_long=unique(data_exc[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","IR_MLID","R317Descrp","IR_Lat","IR_Long","ASSESS_ID","AU_NAME","AU_Type","BeneficialUse","BEN_CLASS","R3172ParameterName","IR_Value","exc")])
profs_wide=reshape2::dcast(DataLoggerLine+ActivityIdentifier+ActivityStartDate+IR_MLID+R317Descrp+IR_Lat+IR_Long+ASSESS_ID+AU_NAME+AU_Type+BeneficialUse+BEN_CLASS~R3172ParameterName,
					data=profs_long, value.var="IR_Value", fun.aggregate=mean, na.rm=T)
exc_wide=reshape2::dcast(DataLoggerLine+ActivityIdentifier+ActivityStartDate+IR_MLID+R317Descrp+IR_Lat+IR_Long+ASSESS_ID+AU_NAME+AU_Type+BeneficialUse+BEN_CLASS~R3172ParameterName,
					data=profs_long, value.var="exc", fun.aggregate=max, fill=0)
exc_wide=exc_wide[,names(exc_wide)!="Profile depth"]
exc_wide=dplyr::rename(exc_wide,do_exc="Minimum Dissolved Oxygen", pH_exc=pH, temp_exc="Temperature, water")

profs_exc=merge(profs_wide,exc_wide,all=T)

names(profs_exc)=make.names(names(profs_exc))

# Calculate thermocline depths
test=profs_exc[profs_exc$ActivityIdentifier=="UTAHDWQ_WQX-BORFG061615-4938550-0617-Pr-F",]
thermo_depths=rLakeAnalyzer::thermo.depth(test$Temperature..water, test$Profile.depth)

calcTdepth=function(x){
	x_agg=aggregate(Temperature..water~Profile.depth, x, FUN='mean')
	return(rLakeAnalyzer::thermo.depth(x_agg$Temperature..water, x_agg$Profile.depth))
}

t_profs=profs_exc[,c("ActivityIdentifier","ActivityStartDate","IR_MLID","ASSESS_ID","Profile.depth","Temperature..water")]
dim(t_profs)
t_profs2=aggregate(Temperature..water~., data=t_profs, FUN='length')
dim(t_profs2)


thermo_depths=plyr::ddply(t_profs,
						  c("ActivityIdentifier","ActivityStartDate","IR_MLID","ASSESS_ID"),
						  .fun=calcTdepth)





#











return(profile_asmnts)

}



