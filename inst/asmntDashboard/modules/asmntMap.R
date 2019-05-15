asmntMap=function(au_asmnt_poly, site_asmnt, na_sites, rejected_sites){
	ss_poly=wqTools::ss_poly
	bu_poly=wqTools::bu_poly
	au_asmnt_poly=within(au_asmnt_poly, {
		lab=paste0(
					'<p>', 
					"AU name: ", AU_NAME,
					'<br />', "AU ID: ", ASSESS_ID,
					'<br />', "Assessment: ", AssessCat,
					'<br />', "Impaired params: ", Impaired_params,
					'<br />', "ID w/ exceedance params: ", idE_params)
	
	})

	na_sites=na_sites[,names(na_sites) %in% names(site_asmnt)]
	na_sites$AssessCat=NA
	na_sites$col='grey'
	
	#site_asmnt=plyr::rbind.fill(site_asmnt,na_sites)
	
	assessment_map <- 
		buildMap(plot_polys = F) %>%
			addMapPane("permits", zIndex = 412) %>%
			addMapPane("highlight", zIndex = 413) %>%
			addMapPane("rejected_sites", zIndex = 414) %>%
			leaflet::addCircleMarkers(data=site_asmnt, lat=~IR_Lat, lng=~IR_Long, group="Assessed sites",
				color = ~col, opacity=0.8, layerId=~IR_MLID, options = pathOptions(pane = "markers"),
				popup = paste0(
					"IR MLID: ", site_asmnt$IR_MLID,
					"<br> IR MLNAME: ", site_asmnt$IR_MLNAME,
					"<br> Assessment: ", site_asmnt$AssessCat,
					"<br> Impaired params: ", site_asmnt$Impaired_params,
					"<br> ID w/ exceedance params: ", site_asmnt$idE_params)
			) %>%
			leaflet::addCircleMarkers(data=na_sites, lat=~IR_Lat, lng=~IR_Long, group="Not-assessed sites",
				color = ~col, opacity=0.8, options = pathOptions(pane = "markers"),
				popup = paste0(
					"IR MLID: ", na_sites$IR_MLID,
					"<br> IR MLNAME: ", na_sites$IR_MLNAME)
			) %>%	
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
			) %>%
			addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
				label=lapply(au_asmnt_poly$lab, HTML)
			) %>%
			addPolygons(data=ss_poly,group="Site-specific standards",smoothFactor=4,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0("SS std: ", ss_poly$SiteSpecif)
			) %>%
			leaflet::addLayersControl(position ="topleft",
				baseGroups = c("Topo","Satellite"),overlayGroups = c("Assessed sites", "Not-assessed sites", "Rejected sites", "Assessment units","Beneficial uses", "Site-specific standards"),
				options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
			#hideGroup("Assessment units") %>%
			hideGroup("Not-assessed sites") %>%
			hideGroup("Rejected sites") %>%
			hideGroup("Site-specific standards") %>%
			hideGroup("Beneficial uses") %>%
			hideGroup("Assessed sites") %>%
			leaflet::addLegend(position = 'topright',
						colors = c('green','yellow','orange','red','grey'), 
						labels = c('Fully supporting', 'Insufficient data, no exceedances', 'Insufficient data, exceedances', 'Not supporting', 'Not assessed')) %>% 
			fitBounds(-114.0187, 37.02012, -109.0555, 41.99088)
	
return(assessment_map)	
	
}
	