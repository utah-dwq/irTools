#' Auto-validate USEPA WQP stations
#'
#' Performs auto-validation on previously queried WQP stations combined with the existing master site list.
#' Auto-validates any new sites (i.e. those not currently in the master site list) and appends them into existing master site list flagged for acceptance, rejection, or review.
#' Checks for any new site types in new data. A warning message and a list of new site types is printed if new site types are encountered.
#' Also re-auto-validates any sites in the master site list that have previously only undergone auto-validation (to account for any changes in auto validation process) and checks existing master site list for changes in AUs, selected site types, or property boundaries.
#' Currently requires the existence of a WQP stations file with matching start and end dates, and therefore works best when run in a paired equence with retrieveWQP().
#' Also requires existing "wqp_master_site_file.csv" in outfile_path. Will likely update to select individual files in the future.
#' 
#' @param sites_file Full path and filename of sites file queried from WQP to be reviewed (.csv).
#' @param master_site_file Full path and filename of master site list as generated by autoValidateWQPsites function and manual site review application (.csv).
#' @param polygon_path Full path to folder containing, AU, land ownership, and Utah state boundary 1000 m buffer polygon shapefiles.
#' @param outfile_path Path for file outputs.
#' @param site_type_keep Vector of site type names to be considered in assessment process. Non-specified site types will be automatically rejected. Additional site types can be excluded when running assessment tools. However, including extra site types here may necessetitate additional manual site reviews.
#' @param ownership_assess Vector of property ownership labels to be considered in assessment process. Defaults to c("Federal","Private","State") and exclude "Tribal".
#' @return Exports a new, undated master site list to the outfile_path. If one already exists in outfile_path, it is moved to the 'edit_logs' folder and renamed with the system date.

#' @import sp
#' @import sf

#' @export
#' #####
autoValidateWQPsites=function(sites_file,master_site_file,polygon_path,outfile_path,
	site_type_keep=c(
		"Lake, Reservoir, Impoundment",
		"Stream",
		"Stream: Canal",
		"Stream: Ditch",
		"Spring",
		"River/Stream",
		"Lake",
		"River/Stream Intermittent",
		"River/Stream Perennial",
		"Reservoir",
		"Canal Transport",
		"Canal Drainage",
		"Canal Irrigation")

){

##########
###TESTING SETUP
#library(sp)
#library(sf)
#sites_file="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\02data_raw\\sites101001-180930.csv"
#master_site_file="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\03site_validation\\wqp_master_site_file.csv"
#polygon_path="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\03site_validation\\polygons"
#outfile_path="P:\\WQ\\Integrated Report\\Automation_Development\\jake\\03site_validation"
#site_type_keep=c("Lake, Reservoir, Impoundment",
#				 "Stream",
#				 "Spring",
#				 "Stream: Canal",
#				 "Stream: Ditch",
#				 "River/Stream",
#				 "Lake",
#				 "River/Stream Intermittent",
#				 "River/Stream Perennial",
#				 "Reservoir",
#				 "Canal Transport",
#				 "Canal Drainage",
#				 "Canal Irrigation")
########



setwd(outfile_path)


# Read in WQP station and results data
stn = read.csv(sites_file, stringsAsFactors=FALSE)
stn[stn==""]=NA #Make sure all blanks are NA

#Read in master site file
master_site=read.csv(master_site_file, stringsAsFactors=FALSE)
names(master_site)[names(master_site)=="IR_COMMENT"]="IR_REASON"
if(dim(master_site)[1]>0){master_site[master_site==""]=NA} #Make sure all blanks are NA

suppressWarnings({class(stn$HorizontalAccuracyMeasure.MeasureValue)="numeric"}) #Non-numeric values introduced to this column were causing appearance of duplicates in master_site_file
class(master_site$HorizontalAccuracyMeasure.MeasureValue)="numeric"

#Check for new site types...
master_site_types=unique(master_site$MonitoringLocationTypeName)
stn_site_types=unique(stn$MonitoringLocationTypeName)
new_site_types=stn_site_types[!stn_site_types %in% master_site_types]
if(length(new_site_types)>0){
	print("WARNING: New site type(s) encountered")
	print(cbind(new_site_types))}


#Identify any new sites (all review columns == NA) and move to new data frame (stn_new)
stn2=stn[,!names(stn)%in%c("LatitudeMeasure","LongitudeMeasure","HorizontalCoordinateReferenceSystemDatumName")]
stn2[stn2==""]=NA #Make sure all blanks are NA
stn2=merge(stn2,master_site,all.x=TRUE)
stn_new=stn2[is.na(stn2$UID),]
dim(stn_new)
rm(stn2)


#Assign UID to new sites
if(dim(master_site)[1]>0&dim(stn_new)[1]>0){
	stn_new$UID=seq(1:dim(stn_new)[1])+max(master_site$UID)
}else{
	if(dim(stn_new)[1]>0){
		stn_new$UID=seq(1:dim(stn_new)[1])}
	}

#Delete lat, long, and datum columns from new sites data frame. Then merge back in from stn.
table(stn_new$HorizontalCoordinateReferenceSystemDatumName)
stn_new=stn_new[,!names(stn_new)%in%c("LatitudeMeasure","LongitudeMeasure","HorizontalCoordinateReferenceSystemDatumName")]
stn_new=merge(stn_new,stn,all.x=TRUE)
dim(stn_new)
table(as.factor(stn_new$HorizontalCoordinateReferenceSystemDatumName))

#Read in polygons
ut_poly=st_read(polygon_path,"UT_state_bnd_noTribal_wgs84")
ut_poly=ut_poly[,"STATE_NAME"]
suppressWarnings({ut_poly=st_buffer(ut_poly, 0)})
#lo_poly=st_read(polygon_path,"tribal_ownership_wgs84")
#lo_poly=lo_poly[,"OWNERSHIP"]
#lo_poly=st_buffer(lo_poly, 0)
au_poly=st_read(polygon_path,"AU_poly_wgs84")
au_poly=au_poly[,c("ASSESS_ID","AU_NAME","AU_Type")]
au_poly=st_read(polygon_path,"AU_poly_wgs84")
au_poly=au_poly[au_poly$Status=="ACTIVE",c("ASSESS_ID","AU_NAME","AU_Type")]
bu_poly=st_read("polygons","Beneficial_Uses_All_2020IR_wgs84")
bu_poly=bu_poly[bu_poly$Status=="ACTIVE",c("R317Descrp","BenUseClas","Water_Type")]
names(bu_poly)[names(bu_poly)=="BenUseClas"]="BEN_CLASS"
ss_poly=st_read("polygons","SiteSpecific_wgs84")
ss_poly=ss_poly[ss_poly$Status=="ACTIVE","R317Descrp"]
names(ss_poly)[names(ss_poly)=="R317Descrp"]="ss_R317Descrp"


##################################
####Master site reviews
if(dim(master_site)[1]>0){
	#Kick master sites that were previously accepted or merged, but are now at a non-assessed site type etc. to AUTO validation (convert review type to AUTO)
	#(where they will be rejected, doing this here keeps them out of later master site spatial count based checks)
	
	##Send to AUTO review by datum
	#table(master_site$ValidationType)
	#master_site$ValidationType[master_site$HorizontalCoordinateReferenceSystemDatumName!="WGS84"]="AUTO"
	#table(master_site$ValidationType)
	
	#Site type
	table(master_site$ValidationType)
	master_site$ValidationType[!master_site$MonitoringLocationTypeName %in% site_type_keep]="AUTO"
	table(master_site$ValidationType)
	
	#Remove OWNERSHIP, ASSESS_ID, & AU_NAME cols from master_site before re-assigning (this updates master list in case polygons change)
	master_site=master_site[,!names(master_site)%in%c("OWNERSHIP","ASSESS_ID","AU_NAME","AU_Type","BEN_CLASS","R317Descrp","ss_R317Descrp","Water_Type")]	
	class(master_site$IR_FLAG)
	dim(master_site)
	
	#Generate spatial sites object
	sites=master_site
	coordinates(sites)=c("LongitudeMeasure","LatitudeMeasure")
	proj4string(sites)=CRS("+init=epsg:4326")
	sites=st_as_sf(sites)
	
	dim(master_site)
	
	#Intersect sites w/ Utah poly
	isect=st_intersection(sites, ut_poly)
	st_geometry(isect)=NULL
	master_site=merge(master_site,isect,all.x=TRUE)
	dim(master_site)
	
	#Intersect sites w/ AU poly
	isect=st_intersection(sites, au_poly)
	st_geometry(isect)=NULL
	master_site=merge(master_site,isect,all.x=TRUE)
	dim(master_site)

	#Intersect sites w/ BU poly
	isect=st_intersection(sites, bu_poly)
	st_geometry(isect)=NULL
	master_site=merge(master_site,isect,all.x=TRUE)
	dim(master_site)

	#Intersect sites w/ SS poly
	isect=st_intersection(sites, ss_poly)
	st_geometry(isect)=NULL
	master_site=merge(master_site,isect,all.x=TRUE)
	dim(master_site)
	
	rm(sites)


	#Send to AUTO review by is.na(STATE_NAME)
	table(master_site$ValidationType)
	master_site$ValidationType[is.na(master_site$STATE_NAME)]="AUTO"
	table(master_site$ValidationType)
	master_site=master_site[,!names(master_site) %in% "STATE_NAME"]
	
	#Send to AUTO review by is.na(AU)
	table(master_site$ValidationType)
	master_site$ValidationType[is.na(master_site$ASSESS_ID)]="AUTO"
	table(master_site$ValidationType)

	#Send to AUTO where 	MonitoringLocationTypeName is a canal type & AU_Type!="Canal"
	master_site$ValidationType[
		(master_site$MonitoringLocationTypeName=="Stream: Canal" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="Stream: Ditch" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="Canal Transport" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="Canal Drainage" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="Canal Irrigation" & master_site$AU_Type != "Canal")
		]="AUTO"
	table(master_site$ValidationType)

	#Send to AUTO where MonitoringLocationTypeName is a stream or spring type & AU_Type!="River/Stream" or "Canal"
	master_site$ValidationType[
		(master_site$MonitoringLocationTypeName=="Stream" & master_site$AU_Type != "River/Stream")&
		(master_site$MonitoringLocationTypeName=="Stream" & master_site$AU_Type != "Canal") |
		(master_site$MonitoringLocationTypeName=="River/Stream" & master_site$AU_Type != "River/Stream")&
		(master_site$MonitoringLocationTypeName=="River/Stream" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="River/Stream Intermittent" & master_site$AU_Type != "River/Stream")&
		(master_site$MonitoringLocationTypeName=="River/Stream Intermittent" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="River/Stream Perennial" & master_site$AU_Type != "River/Stream")&
		(master_site$MonitoringLocationTypeName=="River/Stream Perennial" & master_site$AU_Type != "Canal")|
		(master_site$MonitoringLocationTypeName=="Spring" & master_site$AU_Type != "River/Stream")&
		(master_site$MonitoringLocationTypeName=="Spring" & master_site$AU_Type != "Canal")
		]="AUTO"
	table(master_site$ValidationType)

	#Send to AUTO where MonitoringLocationTypeName is a lake type & AU_Type!="Reservoir/Lake"
	master_site$ValidationType[
		(master_site$MonitoringLocationTypeName=="Lake, Reservoir, Impoundment" & master_site$AU_Type != "Reservoir/Lake")|
		(master_site$MonitoringLocationTypeName=="Lake" & master_site$AU_Type != "Reservoir/Lake")|
		(master_site$MonitoringLocationTypeName=="Reservoir" & master_site$AU_Type != "Reservoir/Lake")
		]="AUTO"
	table(master_site$ValidationType)

	##Send to AUTO review by !OWNERSHIP %in% ownership_assess
	#table(master_site$ValidationType)
	#master_site$ValidationType[!master_site$OWNERSHIP %in% ownership_assess]="AUTO"
	#table(master_site$ValidationType)
	
	#Send master sites that have only undergone AUTO validation or were flagged for AUTO review above to stn_new (i.e. re-auto review those sites to account for any changes in automated review process)
	stn_new=rbind(stn_new,master_site[master_site$ValidationType=="AUTO",])
	dim(stn_new)
	table(stn_new$HorizontalCoordinateReferenceSystemDatumName)
	
	#Remove stn_new sites from master_site
	dim(master_site)
	master_site=master_site[!master_site$UID%in%stn_new$UID,]
	dim(master_site)
	dim(stn_new)
}


######################################
######################################


#Stop execution if there are no new sites
if(dim(stn_new)[1]==0){stop("No new sites identified. Proceed to next step.",call.=FALSE)}


##Auto review new sites & master sites re-flagged to AUTO...

# Create IR specific columns, all values filled w/ "REVIEW"
stn_new[,c("IR_MLID","IR_FLAG","IR_REASON")] = "REVIEW"
stn_new[,c("IR_Lat","IR_Long")] = NA

# If [MonitoringLocationDescriptionText] contains "Duplicate","Replicate","Dummy","replaced","Blank","QA", or "QC", reject as QAQC
stn_new$IR_FLAG = ifelse(grepl("Duplicate",stn_new$MonitoringLocationDescriptionText) | grepl("Replicate",stn_new$MonitoringLocationDescriptionText) | grepl("Dummy",stn_new$MonitoringLocationDescriptionText) | 
                        grepl("replaced",stn_new$MonitoringLocationDescriptionText) | grepl("Blank",stn_new$MonitoringLocationDescriptionText) | grepl("QA",stn_new$MonitoringLocationDescriptionText) | 
                        grepl("QC",stn_new$MonitoringLocationDescriptionText),"REJECT",stn_new$IR_FLAG)
stn_new$IR_REASON = ifelse(grepl("Duplicate",stn_new$MonitoringLocationDescriptionText),"QAQC-Duplicate",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(grepl("Replicate",stn_new$MonitoringLocationDescriptionText),"QAQC-Replicate",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(grepl("Dummy",stn_new$MonitoringLocationDescriptionText),"QAQC-Dummy",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(grepl("replaced",stn_new$MonitoringLocationDescriptionText),"QAQC-Replaced",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(grepl("Blank",stn_new$MonitoringLocationDescriptionText),"QAQC-Blank",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(grepl("QA",stn_new$MonitoringLocationDescriptionText) | grepl("QC",stn_new$MonitoringLocationDescriptionText),"QAQC-Quality Control",stn_new$IR_REASON)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

# If [OrganizationIdentifier] is test or demo, reject site.
stn_new$IR_FLAG=ifelse(stn_new$OrganizationIdentifier%in%c("OST_SHPD_TEST","DEMOTEST"),"REJECT",stn_new$IR_FLAG)
stn_new$IR_REASON=ifelse(stn_new$OrganizationIdentifier%in%c("OST_SHPD_TEST","DEMOTEST"),"Organization identifier indicates test/demo",stn_new$IR_REASON)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites where horizontal datum =="UNKWN")
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&stn_new$HorizontalCoordinateReferenceSystemDatumName=="UNKWN","Horizontal datum unknown",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&stn_new$HorizontalCoordinateReferenceSystemDatumName=="UNKWN","REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites with ConstructionDateText populated
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$ConstructionDateText),"Construction date text populated",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$ConstructionDateText),"REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites with WellDepthMeasure.MeasureValue populated
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellDepthMeasure.MeasureValue),"Well depth measure populated",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellDepthMeasure.MeasureValue),"REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites with WellDepthMeasure.MeasureUnitCode populated
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellDepthMeasure.MeasureUnitCode),"Well depth measure unit code populated",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellDepthMeasure.MeasureUnitCode),"REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites with WellHoleDepthMeasure.MeasureValue populated
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellHoleDepthMeasure.MeasureValue),"Well hole depth measure populated",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellHoleDepthMeasure.MeasureValue),"REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites with WellHoleDepthMeasure.MeasureUnitCode populated
stn_new$IR_REASON=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellHoleDepthMeasure.MeasureUnitCode),"Well hole depth measure unit code populated",stn_new$IR_REASON)
stn_new$IR_FLAG=ifelse(stn_new$IR_FLAG!="REJECT"&!is.na(stn_new$WellHoleDepthMeasure.MeasureUnitCode),"REJECT",stn_new$IR_FLAG)
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)

#Reject sites where MonitoringLocationTypeName !%in% site_type_keep argument
stn_new$IR_FLAG=ifelse(stn_new$MonitoringLocationTypeName%in%site_type_keep,stn_new$IR_FLAG,"REJECT")
stn_new$IR_REASON=ifelse(stn_new$MonitoringLocationTypeName%in%site_type_keep,stn_new$IR_REASON,"Non-assessed site type")
table(stn_new$IR_FLAG)
table(stn_new$IR_REASON)


####################
###Spatial checks###
####################


#Check for datums that != NAD27, NAD83, or WGS84 before screening on lat/long
#Convert NAD27 and NAD83 lat/long to WGS84, replace old rows w/new coordinates
if(all(stn_new$HorizontalCoordinateReferenceSystemDatumName[stn_new$IR_FLAG!="REJECT"]%in%c("NAD27","NAD83","WGS84"))=="TRUE"){
	temp=stn_new[stn_new$HorizontalCoordinateReferenceSystemDatumName=="NAD27",]
	if(dim(temp)[1]>0){
		coordinates(temp)=c("LongitudeMeasure","LatitudeMeasure")
		proj4string(temp)=CRS("+init=epsg:4267")
		CRS.new=CRS("+init=epsg:4326")
		coords_NAD27toWGS84=spTransform(temp, CRS.new)
		coords_NAD27toWGS84$HorizontalCoordinateReferenceSystemDatumName="WGS84"
		coords_NAD27toWGS84=as.data.frame(coords_NAD27toWGS84)
	}else{
		coords_NAD27toWGS84=data.frame(matrix(ncol = dim(stn_new)[2], nrow = 0))
		names(coords_NAD27toWGS84)=names(stn_new)
		}
	
	temp=stn_new[stn_new$HorizontalCoordinateReferenceSystemDatumName=="NAD83",]
	if(dim(temp)[1]>0){
		coordinates(temp)=c("LongitudeMeasure","LatitudeMeasure")
		proj4string(temp)=CRS("+init=epsg:4269")
		CRS.new=CRS("+init=epsg:4326")
		coords_NAD83toWGS84=spTransform(temp, CRS.new)
		coords_NAD83toWGS84$HorizontalCoordinateReferenceSystemDatumName="WGS84"
		coords_NAD83toWGS84=as.data.frame(coords_NAD83toWGS84)
	}else{
		coords_NAD83toWGS84=data.frame(matrix(ncol = dim(stn_new)[2], nrow = 0))
		names(coords_NAD83toWGS84)=names(stn_new)
		}
	stn_new=stn_new[stn_new$HorizontalCoordinateReferenceSystemDatumName!="NAD27"&stn_new$HorizontalCoordinateReferenceSystemDatumName!="NAD83",]
	stn_new=rbind(stn_new,coords_NAD27toWGS84,coords_NAD83toWGS84)
	stn_new=as.data.frame(stn_new)
	}else{stop("ERROR: datum other than NAD27, NAD83, or WGS84 present. Need to add conversion to R code.")}
dim(stn_new)
table(stn_new$HorizontalCoordinateReferenceSystemDatumName)


########
#Remove OWNERSHIP, ASSESS_ID, AU_NAME, & AU_Type cols from stn_new before re-assigning for all sites (this updates master list in case polygons change)
stn_new=stn_new[,!names(stn_new)%in%c("OWNERSHIP","ASSESS_ID","AU_NAME","AU_Type","BEN_CLASS","R317Descrp","ss_R317Descrp","Water_Type")]
class(stn_new$IR_FLAG)
dim(stn_new)

#Create spatial sites object
sites=stn_new
coordinates(sites)=c("LongitudeMeasure","LatitudeMeasure")
proj4string(sites)=CRS("+init=epsg:4326")
sites=st_as_sf(sites)

#Intersect sites w/ Utah poly
isect=st_intersection(sites, ut_poly)
st_geometry(isect)=NULL
stn_new=merge(stn_new,isect,all.x=TRUE)
dim(stn_new)

stn_new[stn_new$MonitoringLocationIdentifier=="USGS-395040113592601",]

#Intersect sites w/ AU poly
isect=st_intersection(sites, au_poly)
st_geometry(isect)=NULL
stn_new=merge(stn_new,isect,all.x=TRUE)
dim(stn_new)

#Intersect sites w/ BU poly
isect=st_intersection(sites, bu_poly)
st_geometry(isect)=NULL
stn_new=merge(stn_new,isect,all.x=TRUE)
dim(stn_new)

#Intersect sites w/ SS poly
isect=st_intersection(sites, ss_poly)
st_geometry(isect)=NULL
stn_new=merge(stn_new,isect,all.x=TRUE)
dim(stn_new)

rm(sites)

#Reject by is.na(AU)
table(stn_new$IR_FLAG)
stn_new$IR_FLAG[is.na(stn_new$ASSESS_ID)]="REJECT"
stn_new$IR_REASON[is.na(stn_new$ASSESS_ID)]="Undefined AU"
table(stn_new$IR_FLAG)

#Reject by is.na(STATE_NAME)
table(stn_new$IR_FLAG)
stn_new$IR_FLAG[is.na(stn_new$STATE_NAME)]="REJECT"
stn_new$IR_REASON[is.na(stn_new$STATE_NAME)]="Outside UT state boundaries (including tribal)."
table(stn_new$IR_FLAG)
stn_new=stn_new[,!names(stn_new) %in% "STATE_NAME"]


#Reject where 	MonitoringLocationTypeName is a canal type & AU_Type!="Canal"
stn_new$IR_FLAG[
	(stn_new$MonitoringLocationTypeName=="Stream: Canal" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Stream: Ditch" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Transport" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Drainage" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Irrigation" & stn_new$AU_Type != "Canal")
	]="REJECT"
table(stn_new$IR_FLAG)
stn_new$IR_REASON[
	(stn_new$MonitoringLocationTypeName=="Stream: Canal" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Stream: Ditch" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Transport" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Drainage" & stn_new$AU_Type != "Canal")|
	(stn_new$MonitoringLocationTypeName=="Canal Irrigation" & stn_new$AU_Type != "Canal")
	]="Non-assessed canal or ditch"
table(stn_new$IR_REASON)


#Reject where 	MonitoringLocationTypeName is a stream or spring type & AU_Type!="River/Stream"
stn_new$IR_FLAG[
	(stn_new$MonitoringLocationTypeName=="Stream" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Intermittent" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Perennial" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="Spring" & stn_new$AU_Type != "River/Stream")
	]="REJECT"
table(stn_new$IR_FLAG)
stn_new$IR_REASON[
	(stn_new$MonitoringLocationTypeName=="Stream" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Intermittent" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Perennial" & stn_new$AU_Type != "River/Stream")|
	(stn_new$MonitoringLocationTypeName=="Spring" & stn_new$AU_Type != "River/Stream")
	]="Stream or spring site type in non-River/Stream AU"
table(stn_new$IR_REASON)


#Review where MonitoringLocationTypeName is a lake type & AU_Type!="Reservoir/Lake"
stn_new$IR_FLAG[
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Lake, Reservoir, Impoundment" & stn_new$AU_Type != "Reservoir/Lake")|
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Lake" & stn_new$AU_Type != "Reservoir/Lake")|
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Reservoir" & stn_new$AU_Type != "Reservoir/Lake")
	]="REVIEW"
table(stn_new$IR_FLAG)
stn_new$IR_REASON[
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Lake, Reservoir, Impoundment" & stn_new$AU_Type != "Reservoir/Lake")|
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Lake" & stn_new$AU_Type != "Reservoir/Lake")|
	(stn_new$IR_FLAG!="REJECT" & stn_new$MonitoringLocationTypeName=="Reservoir" & stn_new$AU_Type != "Reservoir/Lake")
	]="MLID type is lake/reservoir, but AU_Type is not - potential new AU needed"
table(stn_new$IR_REASON)



##########
##Calculate full distance matrix, lat/long, mlid, and site 100m counts.
##For new sites and master flagged for re-AUTO (stn_new), apply lat/long, 100 m site count logic to determine review status
##For previously manually reviewed master sites, check if MLID_Count, Lat_Count, Long_Count, or sites100m_count have increased, flag these for review

#rbind new auto-validated sites back to master file and calculate distances on all non-rejected sites in master file (this way if polygons change, it is automatically accounted for in master file)
spatial_check_data=rbind(master_site,stn_new)
dim(spatial_check_data)

#Splitting off rejected sites prior to other spatial analyses (including all previously accepted/merged sites allows calc of distances including previously reviewed sites.)

accept_review=spatial_check_data[spatial_check_data$IR_FLAG!="REJECT",]
rejected=spatial_check_data[spatial_check_data$IR_FLAG=="REJECT",]
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

#Identify non-rejected sites w/in 100 m (0.1 km)
distmat=spDists(cbind(accept_review$LongitudeMeasure,accept_review$LatitudeMeasure),longlat=TRUE)
row.names(distmat)=accept_review$MonitoringLocationIdentifier
colnames(distmat)=accept_review$MonitoringLocationIdentifier

sum(table(accept_review$IR_FLAG))

countSites100m=function(data){
	count=sum(data>0&data<=0.1)
	#names(data[data>0&data<=0.1])
	return(count)
	}

sites100m_count=apply(distmat,1,FUN='countSites100m')
accept_review$sites100m_count=sites100m_count

rejected$sites100m_count=NA
rejected$MLID_Count=NA
rejected$Lat_Count=NA
rejected$Long_Count=NA

#Re-appending rejected data
spatial_check_data=rbind(accept_review,rejected)
table(spatial_check_data$IR_FLAG)
sum(table(spatial_check_data$IR_FLAG))
rm(accept_review)
rm(rejected)


#Join spatial checks to stn_new (drop spatial count columns then merge)
dim(stn_new)
stn_new=stn_new[,!names(stn_new)%in%c("ASSESS_ID","AU_NAME","AU_Type","BEN_CLASS","R317Descrp","ss_R317Descrp","OWNERSHIP","ValidationType","MLID_Count","Lat_Count","Long_Count","sites100m_count")]
stn_new=merge(stn_new,spatial_check_data,all.x=T)
dim(stn_new)
stn_new$ValidationType="AUTO"

#Apply next 4 to stn_new only
#Populate stn_new$MLID & lat/long for new sites w/ no duplicate MLIDS, lats, longs, and 0 other sites w/in 100m (IR_FLAG=="REVIEW" for all non-rejected new sites at this point)
stn_new$IR_MLID = ifelse(stn_new$IR_FLAG=="REVIEW"&stn_new$MLID_Count==1&stn_new$Lat_Count==1&stn_new$Long_Count==1&stn_new$sites100m_count==0,as.vector(stn_new$MonitoringLocationIdentifier),"REVIEW")
stn_new$IR_Lat = ifelse(stn_new$IR_FLAG=="REVIEW"&stn_new$MLID_Count==1&stn_new$Lat_Count==1&stn_new$Long_Count==1&stn_new$sites100m_count==0,stn_new$LatitudeMeasure,NA)
stn_new$IR_Long = ifelse(stn_new$IR_FLAG=="REVIEW"&stn_new$MLID_Count==1&stn_new$Lat_Count==1&stn_new$Long_Count==1&stn_new$sites100m_count==0,stn_new$LongitudeMeasure,NA)

#Populate rejected MLID, lat, and long w/ REJECT
stn_new$IR_MLID = ifelse(stn_new$IR_FLAG=="REJECT","REJECT",as.vector(stn_new$IR_MLID))
stn_new$IR_Lat = ifelse(stn_new$IR_FLAG=="REJECT",NA,stn_new$IR_Lat)
stn_new$IR_Long = ifelse(stn_new$IR_FLAG=="REJECT",NA,stn_new$IR_Long)

#Populate ACCEPT for new sites w/ no duplicate MLIDS, lats, longs, and 0 other sites w/in 100m (IR_FLAG=="REVIEW" for all non-rejected new sites at this point)
stn_new$IR_REASON = ifelse(stn_new$IR_FLAG=="REVIEW"&stn_new$MLID_Count==1&stn_new$Lat_Count==1&stn_new$Long_Count==1&stn_new$sites100m_count==0,"ACCEPT",stn_new$IR_REASON)
stn_new$IR_REASON = ifelse(stn_new$IR_FLAG=="REVIEW","Manual review required",stn_new$IR_REASON)
stn_new$IR_FLAG = ifelse(stn_new$IR_FLAG=="REVIEW"&stn_new$MLID_Count==1&stn_new$Lat_Count==1&stn_new$Long_Count==1&stn_new$sites100m_count==0,"ACCEPT",stn_new$IR_FLAG)
stn_new$IR_REASON = ifelse(stn_new$IR_FLAG=="ACCEPT","ACCEPT",stn_new$IR_REASON)
sum(table(stn_new$IR_FLAG))

#Review where MonitoringLocationTypeName is a stream or spring type & AU_Type=="Canal"
stn_new$IR_FLAG[
	(stn_new$MonitoringLocationTypeName=="Stream" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Intermittent" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Perennial" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="Spring" & stn_new$AU_Type == "Canal")
	]="REVIEW"
table(stn_new$IR_FLAG)
stn_new$IR_REASON[
	(stn_new$MonitoringLocationTypeName=="Stream" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Intermittent" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="River/Stream Perennial" & stn_new$AU_Type == "Canal")|
	(stn_new$MonitoringLocationTypeName=="Spring" & stn_new$AU_Type == "Canal")
	]="Stream or spring site type in canal AU type"
table(stn_new$IR_REASON)

#Join spatial checks to master_site
spatial_check_data=spatial_check_data[,names(spatial_check_data) %in% c("UID","MLID_Count","Lat_Count","Long_Count","sites100m_count")]
names(spatial_check_data)=c("UID","MLID_Count2","Lat_Count2","Long_Count2","sites100m_count2")
master_site=merge(master_site,spatial_check_data,all.x=T)

#Check if MLID_Count, Lat_Count, Long_Count, or sites100m_count have increased, flag these for review
master_site$IR_FLAG[master_site$MLID_Count2>master_site$MLID_Count]="REVIEW"
master_site$IR_FLAG[master_site$Lat_Count2>master_site$Lat_Count]="REVIEW"
master_site$IR_FLAG[master_site$Long_Count2>master_site$Long_Count]="REVIEW"
master_site$IR_FLAG[master_site$sites100m_count2>master_site$sites100m_count]="REVIEW"

#Update master_site MLID_Count, Lat_Count, Long_Count, or sites100m
master_site$MLID_Count=master_site$MLID_Count2
master_site$Lat_Count=master_site$Lat_Count2
master_site$Long_Count=master_site$Long_Count2
master_site$sites100m_count=master_site$sites100m_count2

#Drop master_site MLID_Count2, Lat_Count2, Long_Count2, and sites100m2
master_site=master_site[,!names(master_site) %in% c("MLID_Count2","Lat_Count2","Long_Count2","sites100m_count2")]

if(dim(master_site)[1]>0){
	levels(master_site$IR_REASON)=unique(c(levels(master_site$IR_REASON),"Manual review required"))
	master_site$IR_REASON[master_site$IR_FLAG=="REVIEW"]="Manual review required"
}

#rbind master_site & stn_new to make full list of all sites (master_new)
master_new=rbind(master_site, stn_new)

#Intersect sites w/ GSL AU
sites=master_new
coordinates(sites)=c("LongitudeMeasure","LatitudeMeasure")
proj4string(sites)=CRS("+init=epsg:4326")
sites=st_as_sf(sites)
gsl_poly=st_read(paste0(outfile_path,"\\polygons"),"GSL_poly_wgs84")
accept_review_lo_int=st_intersection(sites, gsl_poly)
st_geometry(accept_review_lo_int)=NULL
master_new=merge(master_new,accept_review_lo_int,all.x=TRUE)

#Reject GSL sites
master_new$IR_REASON = ifelse(master_new$IR_FLAG!="REJECT" & !is.na(master_new$Id),"GSL assessed through separate program",as.character(master_new$IR_REASON))
master_new$IR_FLAG = ifelse(master_new$IR_FLAG!="REJECT" & !is.na(master_new$Id),"REJECT",as.character(master_new$IR_FLAG))
master_new=master_new[,!names(master_new)%in%c("Id")]
table(master_new$IR_FLAG)
sum(table(master_new$IR_FLAG))


####Sort by UID and re-order columns before writing
master_new=master_new[order(master_new$UID),]
master_new=master_new[,c("UID","OrganizationIdentifier","OrganizationFormalName","ProviderName","MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName","MonitoringLocationDescriptionText",
		   "IR_FLAG","IR_REASON","IR_MLID","ASSESS_ID","AU_NAME", "AU_Type","Water_Type", "R317Descrp", "ss_R317Descrp", "BEN_CLASS", "MLID_Count","Lat_Count","Long_Count","sites100m_count","LatitudeMeasure","LongitudeMeasure","IR_Lat","IR_Long","HUCEightDigitCode",
		   "DrainageAreaMeasure.MeasureValue","DrainageAreaMeasure.MeasureUnitCode","ContributingDrainageAreaMeasure.MeasureValue","ContributingDrainageAreaMeasure.MeasureUnitCode",
		   "SourceMapScaleNumeric","HorizontalAccuracyMeasure.MeasureValue","HorizontalAccuracyMeasure.MeasureUnitCode","HorizontalCollectionMethodName","HorizontalCoordinateReferenceSystemDatumName",
		   "VerticalMeasure.MeasureValue","VerticalMeasure.MeasureUnitCode","VerticalAccuracyMeasure.MeasureValue","VerticalAccuracyMeasure.MeasureUnitCode","VerticalCollectionMethodName",
		   "VerticalCoordinateReferenceSystemDatumName","CountryCode","StateCode","CountyCode","AquiferName","FormationTypeText","AquiferTypeName","ConstructionDateText","WellDepthMeasure.MeasureValue",
		   "WellDepthMeasure.MeasureUnitCode","WellHoleDepthMeasure.MeasureValue","WellHoleDepthMeasure.MeasureUnitCode","ValidationType")]

levels(master_new$AU_Type)=c(levels(master_new$AU_Type),"Undefined")
master_new$AU_Type[is.na(master_new$AU_Type)]="Undefined"
names(master_new)[names(master_new)=="IR_REASON"]="IR_COMMENT"

# Export the file with all FLAG, REJECT, and FINE data included as the marked-up master file			
if(file.exists("wqp_master_site_file.csv")){
	file.rename("wqp_master_site_file.csv", paste0("wqp_master_site_file_",Sys.Date(),".csv"))
	file.copy(paste0("wqp_master_site_file_",Sys.Date(),".csv"),to="edit_logs")
	file.remove(paste0("wqp_master_site_file_",Sys.Date(),".csv"))
}

write.csv(master_new, file="wqp_master_site_file.csv",row.names=F)

print("Site validation complete.")
print(paste0(outfile_path,"\\wqp_master_site_file.csv"))

}

















