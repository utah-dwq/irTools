# INPUTS: 
# datasets (default list includes toxics, conventionals, lakes)
# columns upon which to subset and aggregate data (AU, Use, Parameter--needs to be flexible)
# room for TMDL list
#
# hierarchy of decision making within each subset: NS>idE>FS>idNE


# 1. Combine toxics, conventionals, lakes.

auRollUp <- function(data=list(toxics_assessed,conv_assessed),subs=c("ASSESS_ID","BeneficialUse","R3172ParameterName")){

# Testing
data=list(toxics_assessed,conv_assessed)
subs=c("ASSESS_ID","BeneficialUse","R3172ParameterName")
# Combine all assessed data into one dataframe for roll up
dat_all <- plyr::rbind.fill(data)

# Define hierarchy function of roll up decision-making
x = test
roll <- function(x){
  x$AU_Cat[x$IR_Cat=="NS"]<-5
  x$AU_Cat[x$IR_Cat=="idE"]<-3
  x$AU_Cat[x$IR_Cat=="idNE"]<-2
  x$AU_Cat[x$IR_Cat=="FS"]<-1
  
out <- x[which.max(x$AU_Cat),names(x)%in%c(subs,"AU_Cat")]
return(out)
}

rollup <- plyr::ddply(dat_all, .(subs),roll)

  
test <- dat_all[dat_all$AU_NAME=="Evacuation Creek"&dat_all$BeneficialUse=="2B",] 

  
  
}