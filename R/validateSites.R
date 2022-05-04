#' Validate monitoring locations for the IR
#'
#' Performs auto-validation on previously queried WQP stations combined with the existing master site list.
#' Checks for any new site types in new data. A warning message and a list of new site types is printed if new site types are encountered.
#' 
#' @param sites Sites object queried from WQP to be reviewed.
#' @param trans_wb Full path and filename of translation workbook containing the waterbody type table used in th screening process.
#' @param manual_path Path to workbook containing manual site changes from previous IR's
#' @param slco Logical. Default is FALSE. If TRUE, function will use the grepl function to automatically merge SLCOWS and UTAHDWQ sites containing the same two-letter, 4-number combination code used to denote SLCo monitoring locations.
#' @return Exports a new, undated master site list to the location & filename provided by the user.

#' @import sp
#' @import plyr
#' @import dplyr
#' @import tidyr

validateSites <- function(sites,trans_wb,manual_path,slco=FALSE){
  stn=sites
  stn[stn==""]=NA #Make sure all blanks are NA
  stn=unique(stn)
  
  # Create IR specific columns, all values filled w/ "REVIEW"
  stn[,c("IR_MLID","IR_MLNAME","IR_FLAG","IR_COMMENT")] = "REVIEW"
  
  ############################
  ###Attribute-based checks###
  ############################
  
  site_type_keep = readxl::read_xlsx(trans_wb,sheet="waterbodyTypeTable")
  site_type_keep = subset(site_type_keep,site_type_keep$IR_FLAG=="ACCEPT")$MonitoringLocationTypeName
  
  rej_reasons_att=data.frame(matrix(nrow=0,ncol=2))
  
  # If [MonitoringLocationDescriptionText] contains "Duplicate","Replicate","Dummy","replaced","Blank","QA", or "QC", reject as QAQC
  reason_n = stn[grepl("Duplicate|Replicate|Dummy|replaced|Blank|QA|QC",stn$MonitoringLocationDescriptionText),]%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Attributes indicate dup, rep, blank, dummy, or QAQC site")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  # If [OrganizationIdentifier] is test or demo, reject site.
  reason_n=stn%>%filter(OrganizationIdentifier%in%c("OST_SHPD_TEST","DEMOTEST"))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Organization identifier indicates test/demo")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with ConstructionDateText populated
  reason_n=stn%>%filter(!is.na(ConstructionDateText))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Construction date text populated")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with WellDepthMeasure.MeasureValue populated
  reason_n=stn%>%filter(!is.na(WellDepthMeasure.MeasureValue))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Well depth measure populated")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with WellDepthMeasure.MeasureUnitCode populated
  reason_n=stn%>%filter(!is.na(WellDepthMeasure.MeasureUnitCode))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Well depth measure unit code populated")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with WellHoleDepthMeasure.MeasureValue populated
  reason_n=stn%>%filter(!is.na(WellHoleDepthMeasure.MeasureValue))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Well hole depth measure populated")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with WellHoleDepthMeasure.MeasureUnitCode populated
  reason_n=stn%>%filter(!is.na(WellHoleDepthMeasure.MeasureUnitCode))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Well hole depth measure unit code populated")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with AquiferName populated
  reason_n=stn%>%filter(!is.na(AquiferName))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Aquifer name populated: associated with unassessed wells")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with FormationTypeText populated
  reason_n=stn%>%filter(!is.na(FormationTypeText))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Formation type populated: associated with unassessed wells")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites with AquiferTypeName populated
  reason_n=stn%>%filter(!is.na(AquiferTypeName))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Aquifer type name populated: associated with unassessed wells")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  #Reject sites where MonitoringLocationTypeName !%in% site_type_keep argument
  reason_n=stn%>%filter(!MonitoringLocationTypeName%in%site_type_keep)%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Non-assessed site type")
  if(dim(reason_n)[1]>0){rej_reasons_att=rbind(rej_reasons_att,reason_n)}
  
  names(rej_reasons_att)=c("MonitoringLocationIdentifier","Reason")
  rej_reasons_att$ReasonType="Attribute based"
  rej_reasons_att$FLAG="REJECT"
  head(rej_reasons_att)
  
  print("Attribute based site rejection reason count:")
  print(table(rej_reasons_att$Reason))
  
  #Set stn IR_FLAG and reason for attribute based site rejections
  stn$IR_FLAG[stn$MonitoringLocationIdentifier %in% rej_reasons_att$MonitoringLocationIdentifier]="REJECT"
  table(stn$IR_FLAG)
  
  ####################
  ###Spatial checks###
  ####################
  
  # Assign AUs/BUs/SS/state/WMU
  stn = wqTools::assignPolys(stn,wqTools::au_poly,lat="LatitudeMeasure", long = "LongitudeMeasure", columns = c("ASSESS_ID","AU_NAME","AU_Type","AU_DESCRIP"))
  stn = wqTools::assignPolys(stn,wqTools::bu_poly,lat="LatitudeMeasure", long = "LongitudeMeasure", columns = c("R317Descrp","bu_class"))
  stn = wqTools::assignPolys(stn,wqTools::ut_poly,lat="LatitudeMeasure", long = "LongitudeMeasure")
  stn = wqTools::assignPolys(stn,wqTools::ss_poly, lat="LatitudeMeasure", long = "LongitudeMeasure")
  # stn = wqTools::assignPolys(stn,wqTools::hnnc_poly, lat="LatitudeMeasure", long = "LongitudeMeasure")
  stn = wqTools::assignPolys(stn,wqTools::wmu_poly, lat="LatitudeMeasure", long = "LongitudeMeasure")
  
  ###Spatial rejections
  rej_reasons_spat=data.frame(matrix(nrow=0,ncol=2))
  
  #Reject by is.na(AU)
  reason_n=stn%>%filter(is.na(ASSESS_ID))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Undefined AU")
  if(dim(reason_n)[1]>0){rej_reasons_spat=rbind(rej_reasons_spat,reason_n)}
  
  #Reject by is.na(STATE_NAME)
  reason_n=stn%>%filter(is.na(jurisdict))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Non-jurisdictional: out of state or within tribal boundaries")
  if(dim(reason_n)[1]>0){rej_reasons_spat=rbind(rej_reasons_spat,reason_n)}
  
  #Reject by GSL poly
  reason_n=stn%>%filter(AU_NAME%in%c('Gilbert Bay','Gunnison Bay','Farmington Bay','Bear River Bay'))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="GSL assessed through separate program")
  if(dim(reason_n)[1]>0){rej_reasons_spat=rbind(rej_reasons_spat,reason_n)}
  
  #Remove unneeded spatial join columns
  stn=stn[,!names(stn)%in%c("jurisdict")]
  
  #Reject where MonitoringLocationTypeName is a canal type & AU_Type!="Canal"
  reason_n=stn%>%filter(MonitoringLocationTypeName%in%c("Stream: Canal","Stream: Ditch","Canal Transport","Canal Drainage","Canal Irrigation")&!AU_Type%in%c("Canal"))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Non-assessed canal or ditch")
  if(dim(reason_n)[1]>0){rej_reasons_spat=rbind(rej_reasons_spat,reason_n)}
  
  #Reject where MonitoringLocationTypeName is a stream or spring type & AU_Type!="River/Stream"
  reason_n=stn%>%filter(MonitoringLocationTypeName%in%c("Stream","River/Stream","River/Stream Intermittent","River/Stream Perennial","Spring")&!AU_Type%in%c("River/Stream"))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Stream or spring site type in non-River/Stream AU")
  if(dim(reason_n)[1]>0){rej_reasons_spat=rbind(rej_reasons_spat,reason_n)}
  
  names(rej_reasons_spat)=c("MonitoringLocationIdentifier","Reason")
  rej_reasons_spat$ReasonType="Spatial"
  rej_reasons_spat$FLAG="REJECT"
  head(rej_reasons_spat)
  
  print("Spatial site rejection reason count:")
  print(table(rej_reasons_spat$Reason))
  
  #Set stn IR_FLAG and reason for spatial site rejections
  stn$IR_FLAG[stn$MonitoringLocationIdentifier %in% rej_reasons_spat$MonitoringLocationIdentifier]="REJECT"
  table(stn$IR_FLAG)
  
  ######################
  ###Duplicate checks###
  ######################
  #Splitting off rejected sites prior to other spatial analyses (including all previously accepted/merged sites allows calc of distances including previously reviewed sites.)
  accept_review=stn[stn$IR_FLAG!="REJECT",]
  rejected=stn[stn$IR_FLAG=="REJECT",]
  class(accept_review$IR_FLAG)
  
  table(accept_review$IR_FLAG)
  sum(table(accept_review$IR_FLAG))
  
  #Count MLIDs, add as column to accept_review, MLID_Count>1 means duplicated MLID
  MLID_Count=as.vector(table(accept_review$MonitoringLocationIdentifier)[accept_review$MonitoringLocationIdentifier])
  accept_review$MLID_Count=MLID_Count
  
  #Count Latitudes, add as column to accept_review, Lat_Count>1 means duplicated lat
  Lat_Count=as.vector(table(accept_review$LatitudeMeasure))[as.factor(accept_review$LatitudeMeasure)]
  accept_review$Lat_Count=Lat_Count
  
  #Count Longitudes, add as column to accept_review, Long_Count>1 means duplicated long
  Long_Count=as.vector(table(accept_review$LongitudeMeasure))[as.factor(accept_review$LongitudeMeasure)]
  accept_review$Long_Count=Long_Count
  
  #Re-appending rejected data
  stn=plyr::rbind.fill(accept_review,rejected)
  table(stn$IR_FLAG)
  sum(table(stn$IR_FLAG))
  rm(accept_review)
  rm(rejected)
  stn$ValidationType="AUTO"
  
  
  #Spatial review flags & reasons (Apply to stn only)
  #Populate stn$MLID & lat/long for new sites w/ no duplicate MLIDS, lats, longs, and 0 other sites w/in 100m (IR_FLAG=="REVIEW" for all non-rejected new sites at this point)
  stn$IR_MLID = ifelse(stn$IR_FLAG=="REVIEW"&stn$MLID_Count==1&stn$Lat_Count==1&stn$Long_Count==1,as.vector(stn$MonitoringLocationIdentifier),"REVIEW")
  stn$IR_MLNAME = ifelse(stn$IR_FLAG=="REVIEW"&stn$MLID_Count==1&stn$Lat_Count==1&stn$Long_Count==1,as.vector(stn$MonitoringLocationName),NA)
  stn$IR_Lat = ifelse(stn$IR_FLAG=="REVIEW"&stn$MLID_Count==1&stn$Lat_Count==1&stn$Long_Count==1,stn$LatitudeMeasure,NA)
  stn$IR_Long = ifelse(stn$IR_FLAG=="REVIEW"&stn$MLID_Count==1&stn$Lat_Count==1&stn$Long_Count==1,stn$LongitudeMeasure,NA)
  
  #Populate rejected MLID, lat, and long w/ REJECT
  stn$IR_MLID = ifelse(stn$IR_FLAG=="REJECT","REJECT",as.vector(stn$IR_MLID))
  stn$IR_MLNAME = ifelse(stn$IR_FLAG=="REJECT","REJECT",as.vector(stn$IR_MLNAME))
  stn$IR_Lat = ifelse(stn$IR_FLAG=="REJECT",NA,stn$IR_Lat)
  stn$IR_Long = ifelse(stn$IR_FLAG=="REJECT",NA,stn$IR_Long)
  
  #Review reasons
  review_reasons=data.frame(matrix(nrow=0,ncol=2))
  
  #MLID, lat/long, and site 50 m counts
  reason_n=stn%>%filter(MLID_Count>1)%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Duplicated MLID")
  if(dim(reason_n)[1]>0){review_reasons=rbind(review_reasons,reason_n)}
  
  reason_n=stn%>%filter(Lat_Count>1|Long_Count>1)%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Duplicated lat or long")
  if(dim(reason_n)[1]>0){review_reasons=rbind(review_reasons,reason_n)}
  
  #MonitoringLocationTypeName is a stream or spring type & AU_Type=="Canal"
  reason_n=stn%>%filter(MonitoringLocationTypeName%in%c("Stream","River/Stream","River/Stream Intermittent","River/Stream Perennial","Spring")& stn$AU_Type%in%c("Canal"))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="Stream or spring site type in canal AU type")
  if(dim(reason_n)[1]>0){review_reasons=rbind(review_reasons,reason_n)}
  
  #MonitoringLocationTypeName is a lake type & AU_Type!="Reservoir/Lake"
  reason_n=stn%>%filter(MonitoringLocationTypeName%in%c("Lake, Reservoir, Impoundment","Lake","Reservoir")& !stn$AU_Type%in%c("Reservoir/Lake"))%>%select(MonitoringLocationIdentifier)%>%mutate(reason="MLID type is lake/reservoir, but AU_Type is not - potential new AU needed")
  if(dim(reason_n)[1]>0){review_reasons=rbind(review_reasons,reason_n)}
  
  #Rename review reason columns
  names(review_reasons)=c("MonitoringLocationIdentifier","Reason")
  review_reasons$ReasonType="Spatial"
  review_reasons$FLAG="REVIEW"
  
  print("Spatial site review reason count:")
  print(table(review_reasons$Reason))
  
  #rbind reasons together
  reasons_all=rbind(rej_reasons_att,rej_reasons_spat, review_reasons)
  
  #Populate ACCEPT for new sites w/ no duplicate MLIDS, lats, longs, and 0 other sites w/in 100m (IR_FLAG=="REVIEW" for all non-rejected new sites at this point)
  stn=within(stn,{
    IR_FLAG[!MonitoringLocationIdentifier %in% reasons_all$MonitoringLocationIdentifier &IR_FLAG!="REJECT" & MLID_Count==1 & Lat_Count==1 & Long_Count==1]<-"ACCEPT"
  })
  
  # Convert all NA AU types to Undefined, change beneficial use column name, add Review Comment Column
  stn$AU_Type[is.na(stn$AU_Type)]="Undefined"
  names(stn)[names(stn)=="bu_class"] = "BEN_CLASS"
  names(stn)[names(stn)=="ss_descrp"] = "ss_R317Descrp"
  
  print("Applying manual site rejections...")
  man_sites = readxl::read_xlsx(manual_path, sheet=1)
  stn_rej = subset(stn, stn$MonitoringLocationIdentifier%in%man_sites$MonitoringLocationIdentifier)[!names(stn)%in%c("IR_COMMENT")]
  stn_rej$IR_FLAG = "REJECT"
  stn_rej$IR_Lat = "REJECT"
  stn_rej$IR_Long = "REJECT"
  stn_rej$IR_MLID = "REJECT"
  stn_rej$IR_MLNAME = "REJECT"
  stn_rej$ValidationType = "MANUAL"
  stn_rej = merge(stn_rej, man_sites, all.x = TRUE)
  rm(man_sites)
  
  stn = subset(stn, !stn$MonitoringLocationIdentifier%in%stn_rej$MonitoringLocationIdentifier)
  stn = plyr::rbind.fill(stn, stn_rej)
  stn$ReviewDate = NA
  
  stn=within(stn,{
    IR_COMMENT[IR_FLAG=="REJECT" & ValidationType=="AUTO"]="Automatically flagged for rejection"
    IR_COMMENT[IR_FLAG=="REVIEW" & ValidationType=="AUTO"]="Automatically flagged for review"
    IR_COMMENT[IR_FLAG=="ACCEPT" & ValidationType=="AUTO"]="Automatically accepted"
  })
  
  # Merge SLCo sites
  if(slco==TRUE){
    print("Merging SLCo and DWQ sites with parsimonious site names...")
    dim(stn)
    stn1 = subset(stn, !(stn$IR_FLAG%in%c("REJECT")))
    slco_sites = subset(stn1, stn1$OrganizationIdentifier=="SLCOWS")
    slco_codes = slco_sites$MonitoringLocationName
    merged_sites = slco_sites[0,]
    for(i in 1:length(slco_codes)){
      slcname = slco_codes[i]
      dwq_site = stn1[grepl(slcname,stn1$MonitoringLocationName)&stn1$OrganizationIdentifier%in%c("UTAHDWQ_WQX"),]
      if(dim(dwq_site)[1]>0){
        slc_site = stn1[grepl(slcname,stn1$MonitoringLocationName)&stn1$OrganizationIdentifier%in%c("SLCOWS"),]
        slc_site$IR_MLID = dwq_site$IR_MLID
        slc_site$IR_MLNAME = dwq_site$IR_MLNAME
        slc_site$IR_Lat = dwq_site$IR_Lat
        slc_site$IR_Long = dwq_site$IR_Long
        dwqslc_site = plyr::rbind.fill(dwq_site,slc_site)
        dwqslc_site$IR_COMMENT = "Two or more sites merged"
        merged_sites = plyr::rbind.fill(merged_sites, dwqslc_site)
      }
    }
    stn2 = subset(stn, !stn$MonitoringLocationIdentifier%in%merged_sites$MonitoringLocationIdentifier)
    stn = plyr::rbind.fill(stn2, merged_sites)
    dim(stn)
  }
  
  # Export master site file
  writexl::write_xlsx(list(sites=stn, reasons=reasons_all),
                      "site_review_file.xlsx", format_headers=F, col_names=T)
  print("Site review file updated and review/rejection reasons file created.")
  
}
