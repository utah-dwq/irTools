options(warn=0)


# site validation app server
server <- function(input, output, session){

permits=read.csv(system.file("extdata", "ut_facilities.csv", package = "irTools"))

observeEvent(input$collapse_panels, {
	if((2 %in% input$collapse_panels | 3 %in% input$collapse_panels) & (input$reviewer=="" | is.null(reactive_objects$sites_input))){
			showModal(modalDialog(easyClose=F, title='Inputs needed', "Please input your name and upload a sites file under the 'Start' box above before proceeding."))
		}	
})


# empty reactive objects list
reactive_objects=reactiveValues()

observeEvent(input$example_input, {
	showModal(urlModal('https://github.com/utah-dwq/irTools/blob/master/inst/extdata/IR_master_site_file-autoreview.xlsx', title = "Example data", subtitle = "An example data input for this application can be downloaded at this link."))
})


# Demo data input
observeEvent(input$demo_input, {
	sites_file=system.file("extdata", "siteValApp_demo_data.xlsx", package = "irTools")
		sites=as.data.frame(readxl::read_excel(sites_file, 'sites'))
		suppressWarnings({sites$IR_Lat=as.numeric(sites$IR_Lat)
		sites$IR_Long=as.numeric(sites$IR_Long)
		sites$ReviewComment=as.character(sites$ReviewComment)})
		sites=within(sites,{
			lat=ifelse(!is.na(IR_Lat), as.numeric(IR_Lat), LatitudeMeasure)
			long=ifelse(!is.na(IR_Long), as.numeric(IR_Long), LongitudeMeasure)
			color=NA
			color[IR_FLAG=="REJECT"]="red"
			color[IR_FLAG=="ACCEPT"]="green"
			color[IR_FLAG=="REVIEW"]="purple"
			color[IR_FLAG=="FURTHER"]="orange"
		})
		reactive_objects$sites_input=sites
		reasons=as.data.frame(readxl::read_excel(sites_file, 'reasons'))
		reactive_objects$reasons_input=reasons

showModal(modalDialog(easyClose=T, 'Demo data uploaded.'))
})


# Read input files
observeEvent(input$import_sites,{
	sites_file=input$import_sites$datapath
	if(is.null(sites_file)){
		return(NULL)
	}else{
		sites=as.data.frame(readxl::read_excel(sites_file, 'sites'))
		suppressWarnings({sites$IR_Lat=as.numeric(sites$IR_Lat)
		sites$IR_Long=as.numeric(sites$IR_Long)
		sites$ReviewComment=as.character(sites$ReviewComment)})
		sites=within(sites,{
			lat=ifelse(!is.na(IR_Lat), as.numeric(IR_Lat), LatitudeMeasure)
			long=ifelse(!is.na(IR_Long), as.numeric(IR_Long), LongitudeMeasure)
			color=NA
			color[IR_FLAG=="REJECT"]="red"
			color[IR_FLAG=="ACCEPT"]="green"
			color[IR_FLAG=="REVIEW"]="purple"
			color[IR_FLAG=="FURTHER"]="orange"
		})
		reactive_objects$sites_input=sites
		reasons=as.data.frame(readxl::read_excel(sites_file, 'reasons'))
		reactive_objects$reasons_input=reasons
			
	}
})

observe({
		req(reactive_objects$sites_input, reactive_objects$reasons_input)
		isolate({
			sites=reactive_objects$sites_input
			reasons=reactive_objects$reasons_input
			reasons_conc=as.data.frame(matrix(nrow=length(unique(reasons$MonitoringLocationIdentifier)), ncol=2))
			names(reasons_conc)=c('MonitoringLocationIdentifier', 'IR_FLAG_REASONS')
			reasons_conc$MonitoringLocationIdentifier=unique(reasons$MonitoringLocationIdentifier)
			for(n in 1:dim(reasons_conc)[1]){
				mlid_n=reasons_conc$MonitoringLocationIdentifier[n]
				reasons_n=as.vector(reasons[reasons$MonitoringLocationIdentifier==mlid_n,"Reason"])
				reasons_conc[n,'IR_FLAG_REASONS']=noquote(paste(reasons_n, collapse='; '))
			}
			
			sites=merge(sites, reasons_conc, all.x=T)
			accepted=sites[sites$IR_FLAG=='ACCEPT',c('MonitoringLocationIdentifier','IR_FLAG')]
			accepted$Reason="Accept"
			names(accepted)[names(accepted)=='IR_FLAG']='FLAG'
			reasons=plyr::rbind.fill(reasons, accepted)
			
			sites=sf::st_as_sf(sites, coords=c("long","lat"), crs=4326, remove=F)
			sites$ReviewDate=as.Date(sites$ReviewDate, format='%Y-%m-%d')
			reactive_objects$reasons=reasons
			reactive_objects$reasons_orig=reasons
			reactive_objects$selected_sites=vector()
			reactive_objects$sites=sites
			reactive_objects$sites_orig=sites
		})
})

observe({
	reactive_objects$reject_reasons=unique(reactive_objects$reasons$Reason[reactive_objects$reasons$FLAG=="REJECT"])
})

observeEvent(reactive_objects$sites, {
	merged_sites=subset(reactive_objects$sites, IR_COMMENT=='Two or more sites merged')
	if(dim(merged_sites)[1]>0){
		merged_sites=sf::st_drop_geometry(merged_sites)
		reactive_objects$merged_sites=sf::st_as_sf(merged_sites, coords=c("LongitudeMeasure","LatitudeMeasure"), crs=4326, remove=F)
	}else{reactive_objects$merged_sites=NULL}
})

# Reasons checkbox
observe({
	reactive_objects$review_reasons=unique(reactive_objects$reasons[reactive_objects$reasons$FLAG %in% input$site_types,]$Reason)
})

observe({
	if(!is.null(input$review_reasons)){
		sel_reasons=input$review_reasons
	}else{sel_reasons=NULL}
	reactive_objects$sel_reasons=sel_reasons
})

output$review_reasons <- renderUI({
	req(reactive_objects$review_reasons)
	isolate({
		shinyWidgets::pickerInput("review_reasons", "Review reason", reactive_objects$review_reasons[order(reactive_objects$review_reasons)], selected=reactive_objects$sel_reasons, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
	})
})

# Select mlids in reasons
observe({
	reactive_objects$reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
})

# ML types checkbox
observe({
	reactive_objects$ml_types=unique(reactive_objects$sites$MonitoringLocationTypeName[reactive_objects$sites$MonitoringLocationIdentifier %in% reactive_objects$reason_mlids & reactive_objects$sites$IR_FLAG %in% input$site_types])
})
output$ml_types=renderUI({
	req(reactive_objects$ml_types)
	shinyWidgets::pickerInput("ml_types", "Site types", reactive_objects$ml_types, selected=reactive_objects$ml_types, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
})

# Select mlids in mltypes
observe({
	req(reactive_objects$sites)
	reactive_objects$mltype_mlids=unique(sf::st_drop_geometry(reactive_objects$sites)[reactive_objects$sites$MonitoringLocationTypeName %in% input$ml_types,'MonitoringLocationIdentifier'])
})

observe({
	reactive_objects$map_sites=
		reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & 
		reactive_objects$sites$MonitoringLocationIdentifier %in% reactive_objects$reason_mlids &
		reactive_objects$sites$MonitoringLocationIdentifier %in% reactive_objects$mltype_mlids
		,]
})

# Map output
session$onFlushed(once = T, function() {
	output$map=renderLeaflet({
		wqTools::buildMap(search="") %>%
		addMapPane("permits", zIndex = 417) %>%
		addMapPane("highlight", zIndex = 418) %>%
		addMapPane("merged_sites", zIndex = 419) %>%
		addMapPane("labels", zIndex = 420) %>%
		addCircleMarkers(lat=permits$LatitudeMeasure, lng=permits$LongitudeMeasure, options = pathOptions(pane = "permits"), group="Permits", radius=5,
			popup = paste0(
				"Permit ID: ", permits$locationID,
				"<br> Permit name: ", permits$locationName,
				"<br> Permit type: ", permits$locationType)
		) %>%
		#addPolygons(data=wqTools::ut_poly,group="State boundary",smoothFactor=4,fillOpacity = 0.1,weight=3,color="purple", options = pathOptions(pane = "underlay_polygons"))  %>%
		hideGroup('Permits')
	})
})

map_proxy=leafletProxy("map")


#observe({
#	print(memory.size())
#})


# Add sites via proxy on site_types change
observeEvent({
	reactive_objects$map_sites
	reactive_objects$merged_sites}, ignoreNULL = F, ignoreInit=T, {
	if(dim(reactive_objects$map_sites)[1]>0){
		mlocs=unique(reactive_objects$map_sites[,c('MonitoringLocationIdentifier','MonitoringLocationName')])
		map_proxy %>% clearGroup(group='Sites') %>% clearGroup(group='Merged sites') %>% clearGroup(group='Site IDs') %>%  clearGroup(group='Site names') %>% 
		addCircleMarkers(data=reactive_objects$map_sites, layerId=~MonitoringLocationIdentifier, group="Sites", color=~color, options = pathOptions(pane = "markers")) %>%
		addCircles(data=mlocs, group="locationID", stroke=F, fill=F, label=~MonitoringLocationIdentifier,
			popup = paste0(
				mlocs$MonitoringLocationIdentifier,
				"<br>", mlocs$MonitoringLocationName)) %>%
		addCircles(data=mlocs, group="locationName", stroke=F, fill=F, label=~MonitoringLocationName,
			popup = paste0(
				mlocs$MonitoringLocationIdentifier,
				"<br>", mlocs$MonitoringLocationName)) %>%
		addLabelOnlyMarkers(data=reactive_objects$map_sites, group="Site IDs", lat=~lat, lng=~long, options = pathOptions(pane = "labels"),
			label=~MonitoringLocationIdentifier,labelOptions = labelOptions(noHide = T, textsize = "15px"),
			clusterOptions=markerClusterOptions(spiderfyOnMaxZoom=T)) %>%
		addLabelOnlyMarkers(data=reactive_objects$map_sites, group="Site names", lat=~lat, lng=~long, options = pathOptions(pane = "labels"),
			label=~MonitoringLocationName,labelOptions = labelOptions(noHide = T, textsize = "15px"),
			clusterOptions=markerClusterOptions(spiderfyOnMaxZoom=T)) %>%
		leaflet.extras::removeSearchFeatures() %>%
		leaflet.extras::addSearchFeatures(
					targetGroups = c('au_ids','au_names','locationID'),
					options = leaflet.extras::searchFeaturesOptions(
						zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
						autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))

		if(!is.null(reactive_objects$merged_sites)){
			if(dim(reactive_objects$merged_sites)[1]>0){
				map_proxy %>% 
				addCircleMarkers(data=reactive_objects$merged_sites, group="Merged sites", color='grey', options = pathOptions(pane = "merged_sites")) %>%
				clearControls() %>%
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Merged sites", "Permits", "Site IDs", "Site names", "Assessment units","Beneficial uses", "Site-specific standards", "Watershed management units", "UT boundary"),
					options = layersControlOptions(collapsed = FALSE)
				)
			}else{
				map_proxy %>% 
				clearControls() %>%
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Permits", "Site IDs", "Site names", "Assessment units","Beneficial uses", "Site-specific standards", "Watershed management units", "UT boundary"),
					options = layersControlOptions(collapsed = FALSE)
				)
			}
		}else{
			map_proxy %>% 
			clearControls() %>%
			addLayersControl(
				position ="topleft",
				baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Permits", "Site IDs", "Site names", "Assessment units","Beneficial uses", "Site-specific standards", "Watershed management units", "UT boundary"),
				options = layersControlOptions(collapsed = FALSE)
			)		
		}
		
		if(input$auto_zoom){
			map_proxy %>% fitBounds(min(reactive_objects$map_sites$long)*0.99, min(reactive_objects$map_sites$lat)*0.99, max(reactive_objects$map_sites$long)*1.01, max(reactive_objects$map_sites$lat)*1.01)
		}
		
		
		map_proxy %>% hideGroup("Site IDs") %>% hideGroup("Site names")
		
	}
	gc()
})


# Map marker click (to identify selected sites will also select 2 sites w/ identical (round(lat/long, 4) but different MLIDs
observeEvent(input$map_marker_click, {
	site_click <- input$map_marker_click
	siteid=site_click$id
	if(!is.null(siteid)){
	  lat = unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'lat'])
		lat = round(as.numeric(paste(lat[1,])[1]),4)
		long = unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'long'])
		long = round(as.numeric(paste(long[1,])[1]),4)
	#   lat=round(as.numeric(paste(unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'lat']))[1]),4)
	# 	long=round(as.numeric(paste(unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'long']))[1]),4)
		latlong_matches=reactive_objects$map_sites[round(reactive_objects$map_sites$lat,4)==lat & round(reactive_objects$map_sites$long,4)==long,]
		if(dim(latlong_matches)[1] > 1){siteid = as.character(unique(latlong_matches$MonitoringLocationIdentifier))}
	
		if(!is.null(siteid)){
			if(any(siteid %in% reactive_objects$selected_sites)){
				reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% siteid]
			}else{
				reactive_objects$selected_sites=append(reactive_objects$selected_sites, siteid)
			}
		}
	}
})


# Update map marker highlights
observeEvent(reactive_objects$selected_sites, ignoreNULL=F, {
	req(reactive_objects$map_sites)
	map_proxy %>%
	clearGroup(group='highlight') %>%
	addCircleMarkers(data=reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier %in% reactive_objects$selected_sites,],
		group='highlight', options = pathOptions(pane = "highlight"), radius = 20, color='chartreuse', opacity = 0.75, fillOpacity = 0.4)
})


# Selected sites table
output$selected_sites_table=DT::renderDT({
	req(reactive_objects$selected_sites)
	reactive_objects$selected_sites_table=as.data.frame(sf::st_drop_geometry(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier %in% reactive_objects$selected_sites,
		c("UID","MonitoringLocationIdentifier","OrganizationIdentifier","MonitoringLocationName","MonitoringLocationTypeName",
		  "IR_FLAG_REASONS","IR_FLAG", "OrganizationFormalName","ProviderName","IR_MLID","IR_MLNAME","ASSESS_ID",
		  "AU_NAME","AU_Type","Water_Type","R317Descrp","ss_R317Descrp",
		  "BEN_CLASS","LatitudeMeasure","LongitudeMeasure","IR_Lat","IR_Long","IR_COMMENT", "ReviewComment")]))
	DT::datatable(reactive_objects$selected_sites_table,
		selection='multiple', rownames=FALSE, filter="top",
		options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="ltipr")
	)

})


# Table selections
observe({
	req(reactive_objects$selected_sites_table)
	reactive_objects$table_selected_table=reactive_objects$selected_sites_table[input$selected_sites_table_rows_selected,]
	reactive_objects$table_selected_mlids=reactive_objects$table_selected_table$MonitoringLocationIdentifier
	
})

# Reviewer actions
## Clear table selection
observeEvent(input$clear_sel, {
	reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
})

## Clear all selected sites
observeEvent(input$clear_all, {
	reactive_objects$selected_sites=NULL
	reactive_objects$table_selected_mlids=NULL
})

## Accept selected sites
observeEvent(input$accept, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="OK to ACCEPT site(s)?",size="l", footer=NULL,
		DT::renderDT({
			DT::datatable(reactive_objects$table_selected_table,
				selection='none', rownames=FALSE, filter="none",
				options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
			)
		}),
		br(),
		br(),
		textInput('accept_comment', 'Additional comments & documentation'),
		actionButton('accept_ok', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('check-circle')),
		actionButton('accept_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}
})

### Update attributes
observeEvent(input$accept_cancel, {removeModal()})
observeEvent(input$accept_ok, {
	accept_mlids=reactive_objects$table_selected_mlids
	reactive_objects$sites=within(reactive_objects$sites, {
		IR_FLAG[MonitoringLocationIdentifier %in% accept_mlids] = "ACCEPT"
		IR_FLAG_REASONS[MonitoringLocationIdentifier %in% accept_mlids] = "Accept"
		IR_COMMENT[MonitoringLocationIdentifier %in% accept_mlids] = "Manually accepted"
		IR_MLID[MonitoringLocationIdentifier %in% accept_mlids] = MonitoringLocationIdentifier[MonitoringLocationIdentifier %in% accept_mlids]
		IR_MLNAME[MonitoringLocationIdentifier %in% accept_mlids] = MonitoringLocationName[MonitoringLocationIdentifier %in% accept_mlids]
		IR_Lat[MonitoringLocationIdentifier %in% accept_mlids] = LatitudeMeasure[MonitoringLocationIdentifier %in% accept_mlids]
		IR_Long[MonitoringLocationIdentifier %in% accept_mlids] = LongitudeMeasure[MonitoringLocationIdentifier %in% accept_mlids]
		color[MonitoringLocationIdentifier %in% accept_mlids]='green'
		ReviewComment[MonitoringLocationIdentifier %in% accept_mlids]=input$accept_comment
		ReviewDate[MonitoringLocationIdentifier %in% accept_mlids]=Sys.Date()
		Reviewer[MonitoringLocationIdentifier %in% accept_mlids]=input$reviewer
		ValidationType[MonitoringLocationIdentifier %in% accept_mlids]="MANUAL"
})

	reactive_objects$reasons=within(reactive_objects$reasons,{
		Reason[MonitoringLocationIdentifier %in% accept_mlids] = 'Accept'
		FLAG[MonitoringLocationIdentifier %in% accept_mlids]="ACCEPT"
	})

	### Re-build reactive_objects$map_sites
	reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
	reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
	
	### Clear table selection & update map highlights (via reactive_objects$selected_sites)
	reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
	
	### Clear modal
	removeModal()


})


## Reject selected sites
observeEvent(input$reject, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="OK to REJECT site(s)?",size="l", footer=NULL,
			DT::renderDT({
				DT::datatable(reactive_objects$table_selected_table,
					selection='none', rownames=FALSE, filter="none",
					options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
				)
			}),
			br(),
			shinyWidgets::pickerInput("reject_reason", label="Reason for rejecting (applied to all selected sites)", choices=c('',reactive_objects$reject_reasons[order(reactive_objects$reject_reasons)]), selected=''),
			textInput('reject_comment', 'Additional comments & documentation'),
			actionButton('reject_ok', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('minus-circle')),
			actionButton('reject_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}
})

observeEvent(input$rejres_ok, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="OK to REJECT site(s)?",size="l", footer=NULL,
			DT::renderDT({
				DT::datatable(reactive_objects$table_selected_table,
					selection='none', rownames=FALSE, filter="none",
					options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
				)
			}),
			br(),
			shinyWidgets::pickerInput("reject_reason", label="Reason for rejecting (applied to all selected sites)", choices=c('',reactive_objects$reject_reasons[order(reactive_objects$reject_reasons)]), selected=''),
			textInput('reject_comment', 'Additional comments & documentation'),
			actionButton('reject_ok', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('minus-circle')),
			actionButton('reject_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
			))
	}
})


### Update attributes
observeEvent(input$reject_cancel, {removeModal()})
observeEvent(input$reject_ok, {
	if(input$reject_reason==""){
		showModal(modalDialog(title="Error.", footer=actionButton('rejres_ok', 'OK', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('check-circle')), 
		size="l",easyClose=F,'Select a rejection reason to proceed.'
			))
	}else{
		reject_mlids=reactive_objects$table_selected_mlids
		reactive_objects$sites=within(reactive_objects$sites, {
			IR_FLAG[MonitoringLocationIdentifier %in% reject_mlids] = "REJECT"
			IR_MLID[MonitoringLocationIdentifier %in% reject_mlids] = "REJECT"
			IR_MLNAME[MonitoringLocationIdentifier %in% reject_mlids] = "REJECT"
			IR_COMMENT[MonitoringLocationIdentifier %in% reject_mlids] = paste("Manually rejected,",paste(input$reject_reason))
			IR_FLAG_REASONS[MonitoringLocationIdentifier %in% reject_mlids]=paste(input$reject_reason)
			ReviewComment[MonitoringLocationIdentifier %in% reject_mlids]=input$reject_comment
			ReviewDate[MonitoringLocationIdentifier %in% reject_mlids]=Sys.Date()
			Reviewer[MonitoringLocationIdentifier %in% reject_mlids]=input$reviewer
			color[MonitoringLocationIdentifier %in% reject_mlids]='red'
			ValidationType[MonitoringLocationIdentifier %in% reject_mlids]="MANUAL"
		})
		
		reactive_objects$reasons=within(reactive_objects$reasons,{
			Reason[MonitoringLocationIdentifier %in% reject_mlids] = paste(input$reject_reason)	
			FLAG[MonitoringLocationIdentifier %in% reject_mlids]="REJECT"
		})
		
		### Re-build reactive_objects$map_sites
		reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
		reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
		
		### Clear table selection & update map highlights (via reactive_objects$selected_sites)
		reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
		
		### Clear modal
		removeModal()
	}
})


## Merge selected sites
observeEvent(input$merge, {
	if(length(reactive_objects$table_selected_mlids)<2){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select two or more site in map & table to merge.")
	)}else{
		showModal(modalDialog(title="OK to MERGE site(s)?",size="l", footer=NULL,
		DT::renderDT({
			DT::datatable(reactive_objects$table_selected_table,
				selection='none', rownames=FALSE, filter="none",
				options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
			)
		}),
		br(),
		shinyWidgets::pickerInput("merge_mlid", label="MLID to merge TO:", choices=reactive_objects$table_selected_mlids),
		renderUI({
			shinyWidgets::pickerInput("merge_mlname", label="ML name to merge TO:", choices=reactive_objects$table_selected_table$MonitoringLocationName,
				selected=reactive_objects$merge_select_mlname)
		}),
		textInput('merge_comment', 'Additional comments & documentation'),
		actionButton('merge_ok', 'Merge', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('object-group')),
		actionButton('merge_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}

})

observe({
	reactive_objects$merge_select_mlname=reactive_objects$table_selected_table$MonitoringLocationName[reactive_objects$table_selected_table$MonitoringLocationIdentifier == input$merge_mlid]
})



### Update attributes
observeEvent(input$merge_cancel, {removeModal()})
observeEvent(input$merge_ok, {
	merge_mlids=reactive_objects$table_selected_mlids
	merged_lat_long=reactive_objects$table_selected_table
	merged_lat_long=merged_lat_long[merged_lat_long$MonitoringLocationIdentifier == input$merge_mlid,]
	reactive_objects$sites=within(reactive_objects$sites, {
		IR_FLAG[MonitoringLocationIdentifier %in% merge_mlids] = "ACCEPT"
		IR_COMMENT[MonitoringLocationIdentifier %in% merge_mlids] = "Two or more sites merged"
		IR_MLID[MonitoringLocationIdentifier %in% merge_mlids] = paste(input$merge_mlid)
		IR_MLNAME[MonitoringLocationIdentifier %in% merge_mlids] = paste(input$merge_mlname)
		IR_Lat[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$LatitudeMeasure
		IR_Long[MonitoringLocationIdentifier %in% merge_mlids]  = merged_lat_long$LongitudeMeasure
		ASSESS_ID[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$ASSESS_ID
		AU_NAME[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$AU_NAME
		AU_Type[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$AU_Type
		Water_Type[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$Water_Type
		R317Descrp[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$R317Descrp
		ss_R317Descrp[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$ss_R317Descrp
		BEN_CLASS[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$BEN_CLASS
		lat[MonitoringLocationIdentifier %in% merge_mlids] = merged_lat_long$LatitudeMeasure
		long[MonitoringLocationIdentifier %in% merge_mlids]  = merged_lat_long$LongitudeMeasure
		IR_FLAG_REASONS[MonitoringLocationIdentifier %in% merge_mlids]="Merge"
		ReviewComment[MonitoringLocationIdentifier %in% merge_mlids]=input$merge_comment
		Reviewer[MonitoringLocationIdentifier %in% merge_mlids]=input$reviewer
		ReviewDate[MonitoringLocationIdentifier %in% merge_mlids]=Sys.Date()
		color[MonitoringLocationIdentifier %in% merge_mlids]='green'
		ValidationType[MonitoringLocationIdentifier %in% merge_mlids]="MANUAL"
	})
	
	reactive_objects$reasons=within(reactive_objects$reasons,{
		Reason[MonitoringLocationIdentifier %in% merge_mlids] = "Merge"
		FLAG[MonitoringLocationIdentifier %in% merge_mlids]= "ACCEPT"
	})
	
	### Re-build reactive_objects$map_sites
	reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
	reactive_objects$sites=sf::st_as_sf(sf::st_drop_geometry(reactive_objects$sites), coords=c("long","lat"), crs=4326, remove=F)
	reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
	
	### Clear table selection & update map highlights (via reactive_objects$selected_sites)
	reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
	
	### Clear modal
	removeModal()
})

## Flag for further review
observeEvent(input$flag_further, ignoreNULL=T, ignoreInit=T, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="FLAG site(s) for additional review?",size="l", footer=NULL,
		DT::renderDT({
			DT::datatable(reactive_objects$table_selected_table,
				selection='none', rownames=FALSE, filter="none",
				options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
			)
		}),
		br(),
		textInput('flag_further_comment', 'Additional comments & documentation'),
		actionButton('flag_ok', 'Flag', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('flag')),
		actionButton('flag_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}
})
observeEvent(input$flag_dialog, ignoreNULL=T, ignoreInit=T, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T, footer=NULL,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="FLAG site(s) for additional review?",size="l", footer=NULL,
		DT::renderDT({
			DT::datatable(reactive_objects$table_selected_table,
				selection='none', rownames=FALSE, filter="none",
				options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
			)
		}),
		br(),
		textInput('flag_further_comment', 'Additional comments & documentation'),
		actionButton('flag_ok', 'Flag', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('flag')),
		actionButton('flag_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}
})

### Update attributes
observeEvent(input$flag_cancel, {removeModal()})
observeEvent(input$flag_ok, {
	if(input$flag_further_comment==""){
		showModal(modalDialog(title="Please make a comment.",size="l",easyClose=F,
			"Please explain the need for further review in the additional comments box to proceed.",
			footer=actionButton('flag_dialog', 'OK', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('check-circle'))
			))
	}else{
		flag_mlids=reactive_objects$table_selected_mlids
		reactive_objects$sites=within(reactive_objects$sites, {
			IR_FLAG[MonitoringLocationIdentifier %in% flag_mlids] = "FURTHER"
			IR_COMMENT[MonitoringLocationIdentifier %in% flag_mlids]="Flagged for further review"
			ReviewComment[MonitoringLocationIdentifier %in% flag_mlids]=input$flag_further_comment
			ReviewDate[MonitoringLocationIdentifier %in% flag_mlids]=Sys.Date()
			Reviewer[MonitoringLocationIdentifier %in% flag_mlids]=input$reviewer
			color[MonitoringLocationIdentifier %in% flag_mlids]='purple'
		})
		
		reactive_objects$reasons=within(reactive_objects$reasons,{
			FLAG[MonitoringLocationIdentifier %in% flag_mlids] = "FURTHER"
			#Reason[MonitoringLocationIdentifier %in% flag_mlids] = "Flagged for further review"
		})
		
		#### Append "Flagged for further review" to reasons for flag_mlids
		flag_reasons=data.frame(flag_mlids, 'Flagged for further review', 'Attribute', 'FURTHER')
		names(flag_reasons) = c('MonitoringLocationIdentifier','Reason','ReasonType','FLAG')
		reactive_objects$reasons=rbind(reactive_objects$reasons, flag_reasons)
		
		#### Re-build reactive_objects$map_sites
		reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
		reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
		
		#### Clear table selection & update map highlights (via reactive_objects$selected_sites)
		reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
		
		#### Clear modal
		removeModal()
	}
})





## Reset sites to input
observeEvent(input$reset, {
	if(length(reactive_objects$table_selected_mlids)==0){
		showModal(modalDialog(title="Error.",size="l",easyClose=T,
			"Select site(s) in map & table to make a review.")
	)}else{
		showModal(modalDialog(title="RESET site(s) to input values?",size="l", footer=NULL,
		DT::renderDT({
			DT::datatable(reactive_objects$table_selected_table,
				selection='none', rownames=FALSE, filter="none",
				options = list(scrollY = TRUE, paging = FALSE, scrollX=TRUE, dom="t")
			)
		}),
		br(),
		textInput('reset_comment', 'Additional comments & documentation'),
		actionButton('reset_ok', 'Reset', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('undo')),
		actionButton('reset_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', icon=icon('window-close'))
		))
	}


})

### Update attributes
observeEvent(input$reset_cancel, {removeModal()})
observeEvent(input$reset_ok, {
	reset_mlids=reactive_objects$table_selected_mlids
	sites=reactive_objects$sites[!reactive_objects$sites$MonitoringLocationIdentifier %in% reset_mlids,]
	reactive_objects$sites=rbind(sites, reactive_objects$sites_orig[reactive_objects$sites_orig$MonitoringLocationIdentifier %in% reset_mlids,])
	
	reasons=reactive_objects$reasons[!reactive_objects$reasons$MonitoringLocationIdentifier %in% reset_mlids,]
	reactive_objects$reasons=rbind(reasons, reactive_objects$reasons_orig[reactive_objects$reasons_orig$MonitoringLocationIdentifier %in% reset_mlids,])	
	
	### Re-build reactive_objects$map_sites
	reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
	reactive_objects$map_sites=NULL
	#reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
	
	### Clear table selection & update map highlights (via reactive_objects$selected_sites)
	reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% reactive_objects$table_selected_mlids]
	
	### Clear modal
	removeModal()
})


## Add rejection reason
observeEvent(input$add_reject_reason, {
	showModal(modalDialog(title="Add rejection reason", size="l",
		textInput('rej_reason_to_add', 'Add rejection reason'),
		actionButton('add_reject_reason_add', 'Add', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('plus-circle')),
		actionButton('add_reject_reason_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('window-close'))
	))
})
observeEvent(input$add_reject_reason_add, {
	reactive_objects$reject_reasons=append(reactive_objects$reject_reasons, paste(input$rej_reason_to_add))
	#reactive_objects$review_reasons=append(reactive_objects$review_reasons, paste(input$rej_reason_to_add))
	removeModal()
})
observeEvent(input$add_reject_reason_cancel, {
	removeModal()
})


# Export reviews
export_file=reactive(paste0('site-reviews-', input$reviewer,'-', Sys.Date(),'.xlsx'))
output$exp_rev <- downloadHandler(
	filename=function(){export_file()},
	content = function(file) {writexl::write_xlsx(
		list(sites=as.data.frame(sf::st_drop_geometry(reactive_objects$sites)[,!names(reactive_objects$sites) %in% c('long','lat','IR_FLAG_REASONS','color','geometry')]), reasons=reactive_objects$reasons[reactive_objects$reasons$FLAG!="ACCEPT",]),
		path = file, format_headers=F, col_names=T)}
)

}



