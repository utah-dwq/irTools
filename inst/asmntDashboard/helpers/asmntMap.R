asmntMap=function(au_asmnt_poly, site_asmnt, na_sites, rejected_sites, ...){
	permits=read.csv(system.file("extdata", "ut_facilities.csv", package = "irTools"))
	priority_permits=subset(permits, priority=='Y')

	na_sites$IR_Lat=as.numeric(na_sites$IR_Lat)
	na_sites$IR_Long=as.numeric(na_sites$IR_Long)
	ss_poly=wqTools::ss_poly
	bu_poly=wqTools::bu_poly

	na_sites=na_sites[,names(na_sites) %in% names(site_asmnt)]
	na_sites$AssessCat=NA
	na_sites$col='grey'
	
	rejected_sites2=rejected_sites
	names(rejected_sites2)[names(rejected_sites2)=='MonitoringLocationIdentifier'] = 'IR_MLID'
	names(rejected_sites2)[names(rejected_sites2)=='MonitoringLocationName'] = 'IR_MLNAME'
	names(rejected_sites2)[names(rejected_sites2)=='LatitudeMeasure'] = 'IR_Lat'
	names(rejected_sites2)[names(rejected_sites2)=='LongitudeMeasure'] = 'IR_Long'
	
	mlocs=plyr::rbind.fill(site_asmnt, na_sites, rejected_sites2)
	mlocs=unique(mlocs[,c('IR_MLID','IR_MLNAME','IR_Lat', 'IR_Long')])
	mlocs$IR_Lat=wqTools::facToNum(mlocs$IR_Lat)
	mlocs$IR_Long=wqTools::facToNum(mlocs$IR_Long)
	site_asmnt$IR_Lat=wqTools::facToNum(site_asmnt$IR_Lat)
	site_asmnt$IR_Long=wqTools::facToNum(site_asmnt$IR_Long)
	
	
	assessment_map <- 
		buildMap(plot_polys = F, search='', ...) %>%
			addMapPane("highlight", zIndex = 413) %>%
			addMapPane("rejected_sites", zIndex = 414) %>%
			addMapPane("permits", zIndex = 417) %>%
			addCircleMarkers(lat=permits$LatitudeMeasure, lng=permits$LongitudeMeasure, options = pathOptions(pane = "permits"), group="Permits", radius=5,
				popup = paste0(
					"Permit ID: ", permits$locationID,
					"<br> Permit name: ", permits$locationName,
					"<br> Permit type: ", permits$locationType)
			) %>%
			addCircleMarkers(lat=priority_permits$LatitudeMeasure, lng=priority_permits$LongitudeMeasure, options = pathOptions(pane = "permits"), group="Priority permits", radius=5, color='purple',
				popup = paste0(
					"Permit ID: ", priority_permits$locationID,
					"<br> Permit name: ", priority_permits$locationName,
					"<br> Permit type: ", priority_permits$locationType)
			) %>%
			addCircles(data=mlocs, lat=~IR_Lat, lng=~IR_Long, group="locationID", stroke=F, fill=F, label=~IR_MLID, 
				popup = paste0(
					mlocs$IR_MLID,
					"<br>", mlocs$IR_MLNAME)) %>%
			addCircles(data=mlocs, lat=~IR_Lat, lng=~IR_Long, group="locationName", stroke=F, fill=F, label=~IR_MLNAME, 
				popup = paste0(
					mlocs$IR_MLID,
					"<br>", mlocs$IR_MLNAME)) %>%
			leaflet::addCircleMarkers(data=site_asmnt, lat=~IR_Lat, lng=~IR_Long, group="Assessed sites",
				color = ~col, opacity=0.8, layerId=~IR_MLID, options = pathOptions(pane = "markers"),
				popup = paste0(
					"IR MLID: ", site_asmnt$IR_MLID,
					"<br> IR MLNAME: ", site_asmnt$IR_MLNAME,
					"<br> ML type: ", site_asmnt$MonitoringLocationTypeName,
					"<br> Assessment: ", site_asmnt$AssessCat,
					"<br> Impaired params: ", site_asmnt$Impaired_params,
					"<br> ID w/ exceedance params: ", site_asmnt$IDEX_params,
					"<br> NS pollution indicators: ", site_asmnt$pi_params)
			) %>%
			leaflet::addCircleMarkers(data=na_sites, lat=~IR_Lat, lng=~IR_Long, group="Not-assessed sites",
				color = ~col, opacity=0.8, options = pathOptions(pane = "markers"),
				popup = paste0(
					"IR MLID: ", na_sites$IR_MLID,
					"<br> IR MLNAME: ", na_sites$IR_MLNAME)
			) %>%	
			#leaflet::addCircleMarkers(data=wqp_sites, lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="WQP sites", radius=5,
			#	color = 'blue', opacity=0.8, options = pathOptions(pane = "markers"),
			#	popup = paste0(
			#		"MLID: ", wqp_sites$MonitoringLocationIdentifier,
			#		"<br> MLNAME: ", wqp_sites$MonitoringLocationName,
			#		"<br> Site type: ", wqp_sites$MonitoringLocationTypeName)
			#) %>%	
			leaflet::addCircleMarkers(data=rejected_sites, 	lat=~LatitudeMeasure, lng=~LongitudeMeasure, group="Rejected sites",
				color = 'purple', opacity=0.8, options = pathOptions(pane = "markers"),
				popup = paste0(
					"MLID: ", rejected_sites$MonitoringLocationIdentifier,
					"<br> MLNAME: ", rejected_sites$MonitoringLocationName)
			) %>%	
			addPolygons(data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0(
					"Description: ", bu_poly$R317Descrp,
					"<br> Uses: ", bu_poly$bu_class)
			)
			
			#if(hover){
			#	assessment_map = assessment_map %>% addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~polyID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
			#		label=lapply(au_asmnt_poly$lab, HTML)
			#	)
			#}else{
			#	assessment_map = assessment_map %>% addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~polyID, weight=3,color=~col, options = pathOptions(pane = "au_poly"))
			#}
			
			assessment_map = assessment_map %>% addPolygons(data=ss_poly,group="Site-specific standards",smoothFactor=4,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0("SS std: ", ss_poly$SiteSpecif)
			) %>%
			leaflet::addLayersControl(position ="topleft",
				baseGroups = c("Topo","Satellite"),overlayGroups = c("Assessed sites", "Not-assessed sites", "Rejected sites", "Assessment units","Beneficial uses", "Site-specific standards", "Permits", "Priority permits"),
				options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
			#hideGroup("Assessment units") %>%
			hideGroup("Not-assessed sites") %>%
			hideGroup("Rejected sites") %>%
			hideGroup("Site-specific standards") %>%
			hideGroup("Beneficial uses") %>%
			hideGroup("Assessed sites") %>%
			hideGroup("WQP sites") %>%
			hideGroup("Permits") %>%
			hideGroup("Priority permits") %>%
			leaflet::addLegend(position = 'topright',
						colors = c('green','yellow','orange','red','grey'), 
						labels = c('Fully supporting', 'Insufficient data, no exceedances', 'Insufficient data, exceedances', 'Not supporting', 'Not assessed')) %>% 
			fitBounds(-114.0187, 37.02012, -109.0555, 41.99088) %>%
			leaflet.extras::removeSearchFeatures() %>% leaflet.extras::addSearchFeatures(
				targetGroups = c('au_ids','au_names', 'locationID'),#, 'locationName'),
				options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
	
return(assessment_map)	
	
}
	