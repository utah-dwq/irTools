# site validation app server

server <- function(input, output, session){

#setwd('C:\\Users\\jvander\\Documents\\R\\irTools\\inst\\siteValApp')
library(wqTools)
library(leaflet)
sites=read.csv(file='IR_master_site_file.csv', stringsAsFactors=F)
reasons=read.csv(file='rev_rej_reasons.csv', stringsAsFactors=F)
sites=within(sites,{
	lat=ifelse(!is.na(IR_Lat), IR_Lat, LatitudeMeasure)
	long=ifelse(!is.na(IR_Long), IR_Long, LongitudeMeasure)
	color=NA
	color[IR_FLAG=="REJECT"]="red"
	color[IR_FLAG=="ACCEPT"]="green"
	color[IR_FLAG=="REVIEW"]="orange"
})

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



# empty reactive objects list
reactive_objects=reactiveValues()
reactive_objects$reasons=reasons
reactive_objects$selected_sites=vector()
reactive_objects$sites=sites

observe({
	reactive_objects$reject_reasons=unique(reactive_objects$reasons$Reason[reactive_objects$reasons$FLAG=="REJECT"])
})


# Reasons checkbox
observe({
	reactive_objects$review_reasons=unique(reactive_objects$reasons[reactive_objects$reasons$FLAG %in% input$site_types,]$Reason)
})
output$review_reasons <- renderUI({
	checkboxGroupInput("review_reasons", "Review reason", reactive_objects$review_reasons, inline=TRUE, selected=input$review_reasons)
})


observe({
	reason_mlids=unique(reactive_objects$reasons[reactive_objects$reasons$Reason %in% input$review_reasons,'MonitoringLocationIdentifier'])
	reactive_objects$map_sites=reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$site_types & reactive_objects$sites$MonitoringLocationIdentifier %in% reason_mlids,]
})

# Map output
session$onFlushed(once = T, function() {
	output$map=leaflet::renderLeaflet({
		wqTools::buildMap(search="") %>%
		# %>%
		addMapPane("highlight", zIndex = 419) %>%
		addLayersControl(
			position ="topleft",
			baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites","Site labels", "Assessment units","Beneficial uses", "Site-specific standards"),
			options = leaflet::layersControlOptions(collapsed = FALSE)
		)
	})
})

map_proxy=leaflet::leafletProxy("map")

# Add sites via proxy on site_types change
observeEvent(reactive_objects$map_sites, ignoreNULL = F, ignoreInit=T, {
		map_proxy %>% clearGroup(group='Sites') %>% clearGroup(group='Site labels') %>% 
		addCircleMarkers(data=reactive_objects$map_sites, layerId = ~MonitoringLocationIdentifier, group="Sites", color=~color, options = pathOptions(pane = "markers"))

		if(!is.null(input$site_types) & !is.null(input$review_reasons)){
			map_proxy %>% addLabelOnlyMarkers(data=reactive_objects$map_sites, group="Site labels", lat=~LatitudeMeasure, lng=~LongitudeMeasure,
				label=~MonitoringLocationIdentifier,labelOptions = leaflet::labelOptions(noHide = T, textsize = "15px"),
				clusterOptions=leaflet::markerClusterOptions(spiderfyOnMaxZoom=T))
		}
})


# Map marker click (to identify selected sites will also select 2 sites w/ identical (round(lat/long, 4) but different MLIDs
observeEvent(input$map_marker_click, {
	site_click <- input$map_marker_click
	siteid=site_click$id
	lat=round(as.numeric(paste(unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'lat']))[1]),4)
	long=round(as.numeric(paste(unique(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier == siteid,'long']))[1]),4)
	latlong_matches=reactive_objects$map_sites[round(reactive_objects$map_sites$lat,4)==lat & round(reactive_objects$map_sites$long,4)==long,]
	if(dim(latlong_matches)[1] > 1){siteid = as.character(unique(latlong_matches$MonitoringLocationIdentifier))}

	if(!is.null(siteid)){
		if(any(siteid %in% reactive_objects$selected_sites)){
			reactive_objects$selected_sites=reactive_objects$selected_sites[!reactive_objects$selected_sites %in% siteid]
		}else{
			reactive_objects$selected_sites=append(reactive_objects$selected_sites, siteid)
		}
	}

})


# Update map marker highlights
observeEvent(reactive_objects$selected_sites, ignoreNULL=F, {
	map_proxy %>%
	clearGroup(group='highlight') %>%
	addCircleMarkers(data=reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier %in% reactive_objects$selected_sites,],
		group='highlight', options = pathOptions(pane = "highlight"), radius = 20, color='chartreuse', opacity = 0.75, fillOpacity = 0.4)
})


# Selected sites table
output$selected_sites_table=DT::renderDT({
	req(reactive_objects$selected_sites)
	reactive_objects$selected_sites_table=as.data.frame(reactive_objects$map_sites[reactive_objects$map_sites$MonitoringLocationIdentifier %in% reactive_objects$selected_sites,
		c("MonitoringLocationIdentifier","OrganizationIdentifier","MonitoringLocationName","MonitoringLocationTypeName",
		  "IR_FLAG_REASONS","IR_FLAG", "OrganizationFormalName","ProviderName","IR_MLID","IR_MLNAME","ASSESS_ID",
		  "AU_NAME","AU_Type","Water_Type","R317Descrp","ss_R317Descrp",
		  "BEN_CLASS","LatitudeMeasure","LongitudeMeasure","IR_Lat","IR_Long","IR_COMMENT", "ReviewComment")])
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
		actionButton('accept_ok', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
		actionButton('accept_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')
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
		selectInput("reject_reason", label="Reason for rejecting (applied to all selected sites)", choices=reactive_objects$reject_reasons),
		textInput('reject_comment', 'Additional comments (optional)'),
		actionButton('reject_ok', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
		actionButton('reject_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%')
		))
	}
})


### Update attributes
observeEvent(input$reject_cancel, {removeModal()})
observeEvent(input$reject_ok, {
	reject_mlids=reactive_objects$table_selected_mlids
	reactive_objects$sites=within(reactive_objects$sites, {
		IR_FLAG[MonitoringLocationIdentifier %in% reject_mlids] = "REJECT"
		IR_COMMENT[MonitoringLocationIdentifier %in% reject_mlids] = "Manually rejected"
		IR_FLAG_REASONS[MonitoringLocationIdentifier %in% reject_mlids]=paste(input$reject_reason)
		ReviewComment[MonitoringLocationIdentifier %in% reject_mlids]=input$reject_comment
		color[MonitoringLocationIdentifier %in% reject_mlids]='red'
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

})



#observeEvent(input$merge, {})
#observeEvent(input$comment, {})

## Add rejection reason
observeEvent(input$add_reject_reason, {
	showModal(modalDialog(title="Add rejection reason", size="l",
		textInput('rej_reason_to_add', 'Add rejection reason'),
		actionButton('add_reject_reason_add', 'Add', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%'),
		actionButton('add_reject_reason_cancel', 'Cancel', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%')
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





}



