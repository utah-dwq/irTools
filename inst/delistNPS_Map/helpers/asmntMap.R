#Map for NPS Delisting Connects

asmntMap=function(au_asmnt_poly, dragging=T, ...){
	# Load delist_candidates1, delist_mlids, nps_proj
	load(system.file("extdata", "delist_NPS_mlids.Rdata", package = "irTools"))
	
	ss_poly=wqTools::ss_poly
	bu_poly=wqTools::bu_poly
	
	delist_mlids$IR_Lat=as.numeric(delist_mlids$IR_Lat)
	delist_mlids$IR_Long=as.numeric(delist_mlids$IR_Long)
	
	delist_mlids1 <- delist_mlids %>%
	  group_by(IR_MLID) %>%
	  summarise(param_summary = paste("<tr><td>", R3172ParameterName, "</td><td>", cycleFirstListed, "</td><td>", Site_cat, "</td></tr>", sep="", collapse=""))
	
	delist_mlids1=merge(delist_mlids1,delist_mlids[,c("IR_MLID","IR_Lat","IR_Long")])
	
	nps_proj$IR_Lat=wqTools::facToNum(nps_proj$IR_Lat)
	nps_proj$IR_Long=wqTools::facToNum(nps_proj$IR_Long)
	
	assessment_map <- 
		buildMap(au_poly=au_asmnt_poly,plot_polys = F, search='', dragging=dragging, ...) %>%
			addMapPane("highlight", zIndex = 413) %>%
			addMapPane("markerPoints", zIndex = 417) %>%
			addCircleMarkers(lat=nps_proj$IR_Lat, lng=nps_proj$IR_Long, options = pathOptions(pane = "markerPoints"),
			                 group="NPS_projects", radius=5, color="blue", opacity = 0.8,
				popup = paste0(
					"Project ID: ", nps_proj$ProjectID,
					"<br> Project Title: ", nps_proj$ProjectTitle,
					"<br> Amount Spent: ", nps_proj$AmountSpent,
					"<br> Date Completed: ", nps_proj$DateCompleted)
			) %>%
	  addCircleMarkers(lat=delist_mlids1$IR_Lat, lng=delist_mlids1$IR_Long, options = pathOptions(pane = "markerPoints"),
	                   group="Delist_mlids", radius=5, color="red", opacity = 0.8,
	                   popup = paste0(
	                     "MLID: ", delist_mlids1$IR_MLID,
	                     "<br> Delisting Params - First Listed - Current Site Category:<br>", delist_mlids1$param_summary )
	  ) %>%
	  addCircles(data=delist_mlids1, lat=~IR_Lat, lng=~IR_Long, group="locationID", stroke=F, fill=F, label=~IR_MLID, 
	             popup = paste0(
	               delist_mlids1$IR_MLID
	               ))%>%
			addPolygons(data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0(
					"Description: ", bu_poly$R317Descrp,
					"<br> Uses: ", bu_poly$bu_class)
			)
			assessment_map = assessment_map  %>%
			leaflet::addLayersControl(position ="topleft",
				baseGroups = c("Topo","Satellite"),overlayGroups = c("NPS_projects", "Assessment units","Beneficial uses", "Site-specific standards", "Delist_mlids"),
				options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
			#hideGroup("Assessment units") %>%
			hideGroup("Beneficial uses") %>%
			leaflet::addLegend(position = 'topright',
						colors = c('blue','red'), 
						labels = c('NPS Projects', 'Delisting Sites')) %>% 
			fitBounds(-114.0187, 37.02012, -109.0555, 41.99088) %>%
			leaflet.extras::removeSearchFeatures() %>% leaflet.extras::addSearchFeatures(
				targetGroups = c('au_ids','au_names', 'locationID'),
				options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
return(assessment_map)	
}
