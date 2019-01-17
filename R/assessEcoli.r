#E Coli specific functions

#######################
#gmean
#geomean function
gmean=function(x){exp(mean(log(x)))}

#######################
#dataPreProc
#function for data preprocessing: subs 1 for <1 and 2420 for >2419.6, calculates site/date geomeans, trims to rec season, creates flat file (MLID+Date+Use=UID, repeats data as necessary)

dataPreProc=function(data_raw){
	data_raw=data_raw[data_raw$Parameter=="E.coli",]
	data_raw$Month=month(as.Date(data_raw$Date,format='%m/%d/%Y'))
	data_raw$Day=day(as.Date(data_raw$Date,format='%m/%d/%Y'))
	data_raw$Year=year(as.Date(data_raw$Date,format='%m/%d/%Y'))
	StartMonth=month(as.Date(SeasonStartDate,format='%m-%d'))
	StartDay=day(as.Date(SeasonStartDate,format='%m-%d'))
	EndMonth=month(as.Date(SeasonEndDate,format='%m-%d'))
	EndDay=day(as.Date(SeasonEndDate,format='%m-%d'))
	data_raw=data_raw[data_raw$Month>=StartMonth&data_raw$Day>=StartDay&data_raw$Month<=EndMonth&data_raw$Day<=EndDay,]
	data_raw$MPN_FINAL=gsub("<1",1,data_raw$MPN_FINAL)
	data_raw$MPN_FINAL=as.numeric(gsub(">2419.6",2420,data_raw$MPN_FINAL))
	data_processed=aggregate(MPN_FINAL~MLID+BEN_CLASS+Date,data=data_raw,FUN='gmean')
	data_processed=data_processed[data_processed$Date!="",]
	data_processed$Date=as.Date(data_processed$Date,format='%m/%d/%Y')
	data_processed$Year=year(data_processed$Date)
	uses=colsplit(data_processed$BEN_CLASS,"/",names=paste0(rep("u",15),seq(1,15,1)))
	MLID=data_processed$MLID
	uses=data.frame(MLID,uses)
	uses_flat=melt(uses,id.vars="MLID",na.rm=T,value.name="Use")
	uses_flat=unique(uses_flat[uses_flat$Use!=""&uses_flat$Use=="1C"|uses_flat$Use=="2A"|uses_flat$Use=="2B",c("MLID","Use")])
	data_processed=merge(data_processed,uses_flat,by="MLID",all=T)	
	return(data_processed)
	}


#############
#maxSamps48hr
#This function is a double loop to count max number of samples w/ 48 hr spacing from any given data set
#Should feed this function data from dataPreProc function
#Loop through each date in rec season, select samples, then apply to get max # of samples for that date.
#If max # of samples satisfies min requirements for representative data (i.e. >=5 samps), calc 30 d average

maxSamps48hr=function(x){
x$doy=yday(as.Date(x$Date,format='%m/%d/%Y'))
count=vector()
for(n in 1:dim(x)[1]){
	data_n=x
	if(dim(data_n)[1]>0){
	doy_seed=data_n$doy[n]
	data_n=data_n[data_n$doy-doy_seed>1|data_n$doy-doy_seed<(-1)|data_n$doy-doy_seed==0,]
	data_ni=data_n
	i=n+1
	if(i>dim(data_ni)[1]){i=1}
	while(doy_seed!=data_ni[i,"doy"]){	
		data_ni=data_ni[data_ni$doy-data_ni[i,"doy"]>1|data_ni$doy-data_ni[i,"doy"]<(-1)|data_ni$doy-data_ni[i,"doy"]==0|data_ni$doy==doy_seed,]
		doy_seed!=data_ni[i,"doy"]
		if(i<dim(data_ni)[1]){i=i+1}else{i=1}
		#print(data_n[n,"doy"]!=data_ni[i,"doy"])
		}
		count=append(count,dim(data_ni)[1])
		max_count=max(count)}}
	if(dim(data_n)[1]==0){max_count=0}
return(max_count)
}

##############################
#assessEColi
#performs EColi assessments for scenarios A, B, & C
#data=preprocessed standard E Coli dataframe including ben use (standard geofile format) column (from dataPreProc() function)

assessA=function(data){
	#maxSamps48hr=maxSamps48hr(data)#48 hr spacing turned off
	maxSamps48hr=dim(data)[1]#48 hr spacing turned off
	Use=data$Use[1]
	if(Use=="2A"){max_crit=std2A_max}
	if(Use=="2B"){max_crit=std2B_max}
	if(Use=="1C"){max_crit=std1C_max}
	if(maxSamps48hr<5){
		if(any(data$MPN_FINAL>max_crit)){assessed=c(paste(dim(data)[1]),"3A","A")
		}else{assessed=c(paste(dim(data)[1]),"3E","A")}
	}
	if(maxSamps48hr>=5){
		tenpct=ceiling(dim(data)[1]/10)
			if(dim(data[data$MPN_FINAL>max_crit,])[1]>tenpct){assessed=c(paste(dim(data)[1]),"5","A")
			}else{assessed=c(paste(dim(data)[1]),"BC","BC")}
	}
return(assessed)	
}

assessBC=function(data){
	Year=data$Year[1]
	Use=data$Use[1]
	rec_season=format(seq(from=as.Date(paste0(Year,"-",SeasonStartDate)),to=as.Date(paste0(Year,"-",SeasonEndDate)),by=1),format="%m-%d-%Y")
	if(Use[1]=="2A"){gmean30d_crit=std2A_30Dgmean}
	if(Use[1]=="2B"){gmean30d_crit=std2B_30Dgmean}
	if(Use[1]=="1C"){gmean30d_crit=std1C_30Dgmean}
	if(dim(data)[1]<5){assess=c(paste(dim(data)[1]),"C","C")}
	if(dim(data)[1]>=5){
		gmean=vector(length=length(rec_season))
		for(j in 1:length(rec_season)){
			dmin=as.Date(rec_season[j],format='%m-%d-%Y')
			dmax=as.Date(rec_season[j],format='%m-%d-%Y')+29
			data_j=data[data$Date>=dmin&data$Date<=dmax,]
			maxSamps48hr_j=maxSamps48hr(data_j)
			if(maxSamps48hr_j>=5){gmean_j=gmean(data_j$MPN_FINAL)}else{gmean_j=0}#Note that 0s here denote no gmean calculated, not a gmean of 0
			gmean[j]=gmean_j}
		if(any(gmean>gmean30d_crit)==TRUE){assess=c(paste(dim(data)[1]),5,"B")}else{assess=c(paste(dim(data)[1]),"C","C")
		}
	}
		if(assess[3]=="C"){
			gmean_recseason=gmean(data$MPN_FINAL)
			if(dim(data)[1]<10&gmean_recseason<=gmean30d_crit){assess=c(paste(dim(data)[1]),2,"C")}
			if(dim(data)[1]<10&gmean_recseason>gmean30d_crit){assess=c(paste(dim(data)[1]),"3A","C")}
			if(dim(data)[1]>=10&gmean_recseason<=gmean30d_crit){assess=c(paste(dim(data)[1]),2,"C")}
			if(dim(data)[1]>=10&gmean_recseason>gmean30d_crit){assess=c(paste(dim(data)[1]),5,"C")}
		}	
	return(assess)
}

assessEColi_MLIDYr=function(data){
	assessedA=ddply(.data=data_preprocessed,.(MLID,Year,Use),.fun=assessA)
	colnames(assessedA)=c("MLID","Year","Use","totalSamps","Cat","Scen")
	
	data_bc=merge(data_preprocessed,assessedA)
	data_bc=data_bc[data_bc$Scen=="BC",]
	
	assessedBC=ddply(.data=data_bc,.(MLID,Year,Use),.fun=assessBC)
	colnames(assessedBC)=c("MLID","Year","Use","totalSamps","Cat","Scen")

	assessedA=assessedA[assessedA$Scen=="A",]
	ecoli_assessments=rbind(assessedA,assessedBC)
	return(ecoli_assessments)
}


####################
######MLIDYr_rollup#
#Rolls up assessments for MLIDs w/ multiple years
#Categories: 5>3A>3E>2
#Input data is output from assessEColi_MLIDYr

MLIDYr_rollup=function(data){
ru=dcast(data,MLID+Use~Year,value.var="Cat")
ru[is.na(ru)]=0 #note, 0 here denotes no data for each year
AssessCat=vector()
for(n in 1:dim(ru)[1]){
	data_n=ru[n,]
	if(any(data_n[3:dim(data_n)[2]]==5)){assess_n=5
	}else{if(any(data_n[3:dim(data_n)[2]]=="3A")){assess_n="3A"
	}else{if(any(data_n[3:dim(data_n)[2]]=="3E")){assess_n="3E"
	}else{if(all(data_n[3:dim(data_n)[2]]==2)){assess_n=2}}}}
	AssessCat=append(AssessCat,assess_n)
	}
ru=cbind(ru,AssessCat)
}














#Old code:
#
#
#
#
#makeKey=function(data){
#	data=cbind(data,uses)
#	key=melt(data[,c("MLID","Year",paste0(rep("u",15),seq(1,15,1)))],id.vars=c("MLID","Year"),na.rm=T)
#	key=key[key$value=="1C"|key$value=="2A"|key$value=="2B",]
#	key=unique(key[,c("MLID","Year","value")])
#	colnames(key)=c("MLID","Year","Use")
#	data_preprocessed=data.frame(data_preprocessed)
#	return(data_preprocessed)
#	}
#
#
#key_BC=data.frame(unique(assess_A[assess_A[,"Scen"]=="BC",c("MLID","Year","Use")]),row.names=NULL)
#assess_A=assess_A[assess_A[,"Scen"]=="A",]
#assess_BC=matrix(ncol=7,nrow=0)
#for(n in 1:dim(key_BC)[1]){
#	MLID_n=as.vector(key_BC[n,1])
#	Year_n=as.vector(key_BC[n,2])
#	data_n=data_preprocessed[data_preprocessed$MLID==MLID_n&data_preprocessed$Year==Year_n,]
#	rec_season=format(seq(from=as.Date(paste0(Year_n,"-",SeasonStartDate)),to=as.Date(paste0(Year_n,"-",SeasonEndDate)),by=1),format="%m-%d-%Y")
#	if(key$Use[n]=="2A"){gmean30d_crit=std2A_30Dgmean}
#	if(key$Use[n]=="2B"){gmean30d_crit=std2B_30Dgmean}
#	if(key$Use[n]=="1C"){gmean30d_crit=std1C_30Dgmean}
#	if(dim(data_n)[1]<5){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,NA,"C","C")}
#	if(dim(data_n)[1]>=5){
#	gmean_n=vector()
#		for(j in 1:length(rec_season)){
#			dmin=as.Date(rec_season[j],format='%m-%d-%Y')
#			dmax=as.Date(rec_season[j],format='%m-%d-%Y')+29
#			data_nj=data_n[data_n$Date>=dmin&data_n$Date<=dmax,]
#			maxSamps48hr_nj=maxSamps48hr(data_nj)
#			if(maxSamps48hr_nj>=5){gmean_ni=gmean(data_nj$MPN_FINAL)}else{gmean_ni=0}#Note that 0s here denote no gmean calculated, not a gmean of 0
#			gmean_n=append(gmean_n,gmean_ni)}
#		if(any(gmean_n>gmean30d_crit)==TRUE){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),5,"B")}else{assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),"C","C")}}
#		if(assess_n[7]=="C"){
#			gmean_recseason=gmean(data_n$MPN_FINAL)
#			if(dim(data_n)[1]<10&gmean_recseason<=gmean30d_crit){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),2,"C")}
#			if(dim(data_n)[1]<10&gmean_recseason>gmean30d_crit){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),"3A","C")}
#			if(dim(data_n)[1]>=10&gmean_recseason<=gmean30d_crit){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),2,"C")}
#			if(dim(data_n)[1]>=10&gmean_recseason>gmean30d_crit){assess_n=c(paste(MLID_n),paste(key_BC$Use[n]),paste(Year_n),NA,paste(dim(data_n)[1]),5,"C")}
#			}			
#		assess_BC=rbind(assess_BC,assess_n)}
#
#colnames(assess_BC)=c("MLID","Use","Year","maxSamps48hr","totalSamps","Cat","Scen")
#MLID_Yr_assessed=data.frame(rbind(assess_A,assess_BC),row.names=NULL)
#return(MLID_Yr_assessed)
#}
#
#
#
#
#
#
#assessEColi_MLIDYr=function(data){
#
#
##Scenario A
#assess_A=matrix(ncol=7,nrow=0)
#for(n in 1:dim(key)[1]){
#	data_n=data[data$MLID==key$MLID[n]&data$Year==key$Year[n],]
#	#maxSamps48hr_n=maxSamps48hr(data_n)#48 hr spacing turned off
#	maxSamps48hr_n=dim(data_n)[1]#48 hr spacing turned off
#	if(key$Use[n]=="2A"){max_crit=std2A_max}
#	if(key$Use[n]=="2B"){max_crit=std2B_max}
#	if(key$Use[n]=="1C"){max_crit=std1C_max}
#		if(maxSamps48hr_n<5){
#			if(any(data_n$MPN_FINAL>max_crit)){assess_n=c(paste(key$MLID[n]),paste(key$Use[n]),paste(key$Year[n]),paste(maxSamps48hr_n),paste(dim(data_n)[1]),"3A","A")
#			}else{assess_n=c(paste(key$MLID[n]),paste(key$Use[n]),paste(key$Year[n]),paste(maxSamps48hr_n),paste(dim(data_n)[1]),"3E","A")}}
#		if(maxSamps48hr_n>=5){
#			tenpct=ceiling(dim(data_n)[1]/10)
#			if(dim(data_n[data_n$MPN_FINAL>max_crit,])[1]>tenpct){assess_n=c(paste(key$MLID[n]),paste(key$Use[n]),paste(key$Year[n]),paste(maxSamps48hr_n),paste(dim(data_n)[1]),"5","A")
#			}else{assess_n=c(paste(key$MLID[n]),paste(key$Use[n]),paste(key$Year[n]),paste(maxSamps48hr_n),paste(dim(data_n)[1]),"BC","BC")}}
#		assess_A=rbind(assess_A,assess_n)
#	}
#colnames(assess_A)=c("MLID","Use","Year","maxSamps48hr","totalSamps","Cat","Scen")
#
#}
#
#
#
##Scenarios B&C (conceptual outline)
##cycle through MLID by year
#	#cycle through calendar, for each 30d window:
#		#Weed out any MLID-years w/ <5 samples {send MLID/year to scen C}
#		#Calculate max # of samples w/ 48 hr spacing
#		#Calc 30d geomean for each window w/ sufficient data
#	#Record all 30d geomeans for each MLID/year
#		#if no 30d geomeans are calculated {send MLID/year to scen C}
#		#if >1 geomeans calculated & 0 exceed {send MLID/year to scen C}
#		#if >1 geomeans & >0 exceed {Cat 5}
#	#For data sent to scen C:
#		#Calc rec season geomean
#		#If >=10 events in MLID-year
#			#Geomean exceed: cat 5
#			#Geomean not exceeded: cat 2
#		#If <10 events in MLID-year
#			#Geomean exceed: cat 3A
#			#Geomean not exceeded: cat 2	
#		
#
#
#####################
#######MLIDYr_rollup#
##Rolls up assessments for MLIDs w/ multiple years
##Categories: 5>3A>3E>2
##Input data is output from assessEColi_MLIDYr
#
#MLIDYr_rollup=function(data){
#ru=dcast(data,MLID+Use~Year,value.var="Cat")
#ru[is.na(ru)]=0 #note, 0 here denotes no data for each year
#AssessCat=vector()
#for(n in 1:dim(ru)[1]){
#	data_n=ru[n,]
#	if(any(data_n[3:dim(data_n)[2]]==5)){assess_n=5
#	}else{if(any(data_n[3:dim(data_n)[2]]=="3A")){assess_n="3A"
#	}else{if(any(data_n[3:dim(data_n)[2]]=="3E")){assess_n="3E"
#	}else{if(all(data_n[3:dim(data_n)[2]]==2)){assess_n=2}}}}
#	AssessCat=append(AssessCat,assess_n)
#	}
#ru=cbind(ru,AssessCat)
#}
#
#
#
#
##Other rollup possibility
##MLID_list=unique(data$MLID)
##rolledup=matrix(nrow=0,ncol=6)
##colnames(rolledup)=c("MLID","AssessCat","CntCat2","CntCat3E","CntCat3A","CntCat5")
##for(n in 1:length(MLID_list)){
##	data_n=data[data$MLID==MLID_list[n],]
##	cat_counts=table(data_n$Cat)
##	if(any(data_n$Cat==5)){assess_n=5
##	}else{if(any(data_n$Cat=="3A")){assess_n="3A"
##	}else{if(any(data_n$Cat=="3E")){assess_n="3E"
##	}else{if(all(data_n$Cat==2)){assess_n=2}}}}
##	rolledup=rbind(rolledup,c(paste(MLID_list[n]),paste(assess_n),cat_counts))
##	}
##rolledup=data.frame(rolledup,row.names=NULL)
##return(rolledup)
#
#