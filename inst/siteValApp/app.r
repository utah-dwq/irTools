



#library(shiny)
#shiny::runApp("P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\draft_code\\site_review_app")

###SET UP
master_site_file="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\02site_validation\\wqp_master_site_file.csv"
polygon_path="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\02site_validation\\polygons"
edit_log_path="P:\\WQ\\Integrated Report\\Automation_Development\\R_package\\demo\\02site_validation\\edit_logs"
###

#library(shiny)
library(raster)
library(rhandsontable)
library(mapview)
library(mapedit)
library(sf)
library(sp)
library(leaflet)
library(shinyjs)
library(V8)
library(rgdal)


#Map flag filter choices
IR_flag_choices=c("ACCEPT","REJECT","REVIEW")


au_poly=st_read(polygon_path,"AU_poly_wgs84")
bu_poly=st_read(polygon_path,"Beneficial_Uses_All_2020IR_wgs84")
ut_poly=st_read(polygon_path,"UT_state_bnd_noTribal_wgs84")
au_poly=au_poly[au_poly$Status=="ACTIVE",]
bu_poly=bu_poly[bu_poly$Status=="ACTIVE",]



jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <-fluidPage(

tags$head(
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    ) 
  ),
 
	#titlePanel("WQP Site Review Tool"),
    useShinyjs(),
	extendShinyjs(text = jscode),
    fluidRow(
		column(3,
			br(),
			actionButton("instruct","Instructions", icon=icon("question"),style='height:50px; padding:4px; font-size:150%',width="200px"),
			checkboxGroupInput("flag_checkbox",label=h3("Review flag"),choices=IR_flag_choices,selected="REVIEW",inline=T),
			uiOutput("reason_checkbox"),
			uiOutput("sitetype_checkbox"),
			uiOutput("autype_checkbox"),
			br(),
			actionButton("draw", "Draw map", icon=icon("pencil"),style='height:50px; padding:4px; font-size:150%',width="200px"),
			br(),
			br(),
			actionButton("save", "Save edits", icon=icon("save"),style='height:50px; padding:4px; font-size:150%',width="200px"),
			br(),
			br(),
			actionButton("refresh", "Refresh app", icon=icon("refresh"),style='height:50px; padding:4px; font-size:150%',width="200px")
		),			
		column(9,h3("Select site(s)"),editModUI("selection",height="600px"))
	),
    fluidRow(
		column(12,h3("Review & edit attributes"),rHandsontableOutput("hot",height="200px"))
	)
)

server <- function(input, output, session){
		
	##Reactive checkbox inputs
	reactive_objects=reactiveValues()

	reactive_objects$sites=read.csv(master_site_file)

	observe({
		reactive_objects$IR_reason_choices=unique(reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$flag_checkbox, "IR_COMMENT"])
	})

	output$reason_checkbox <- renderUI({
		checkboxGroupInput("reason_checkbox", h3("Review reason"), reactive_objects$IR_reason_choices, selected=reactive_objects$IR_reason_choices, inline=TRUE)
	})

	observe({
		reactive_objects$IR_sitetype_choices=unique(reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$flag_checkbox & reactive_objects$sites$IR_COMMENT %in% input$reason_checkbox, "MonitoringLocationTypeName"])
	})
    
	output$sitetype_checkbox <- renderUI({
		checkboxGroupInput("sitetype_checkbox", h3("Site type"), reactive_objects$IR_sitetype_choices, selected=reactive_objects$IR_sitetype_choices, inline=TRUE)
	})

	observe({
		reactive_objects$autype_choices=unique(reactive_objects$sites[reactive_objects$sites$IR_FLAG %in% input$flag_checkbox & reactive_objects$sites$IR_COMMENT %in% input$reason_checkbox & reactive_objects$sites$MonitoringLocationTypeName %in% input$sitetype_checkbox, "AU_Type"])
	})
    
	output$autype_checkbox <- renderUI({
		checkboxGroupInput("autype_checkbox", h3("AU type"), reactive_objects$autype_choices, selected=reactive_objects$autype_choices, inline=TRUE)
	})

	
	#Instructions popup
	observeEvent(input$instruct,{
		showModal(modalDialog(
		title = "User instructions",
		HTML("
				1. Ensure the master site and edit log files are closed.
				2. Select desired site attributes via checkboxes.<br> <br>
				3. Click 'Draw map' button to produce map (a bit slow, be patient).<br><br>
				4. Select desired sites by drawing a polygon or square on the map. Always draw just one polygon at a time and clear polygon when finished.<br><br>
				5. If necessary, edit feature attributes in table below map. Only IR_FLAG, IR_COMMENT, & IR_MLID columns are editable.<br><br>
				6. When satisfied, click 'Save edits' to save edits. Sites for which edits have been made to IR_FLAG will continue to display until the map is refreshed.<br><br>
				7. Click 'Refresh app' to refresh and redraw map to reflect previously saved edits.
			"),size="l"
		))
	})
	
	
	observeEvent(input$draw,{
		showModal(modalDialog(title="MAP DRAWING - PLEASE WAIT...","Please wait for map to draw before proceeding (a bit slow).",size="l",footer=NULL))
		reactive_objects$sites=read.csv(master_site_file)	
		reason_choices=unique(reactive_objects$sites$IR_COMMENT)
		reactive_objects$reason_choices=unique(as.factor(append(as.vector(reason_choices),c("Non-jurisdictional","Merged","Inaccurate location", "Unclear location", "Other"))))
		pal <- colorFactor('Set1', reactive_objects$sites$IR_FLAG)	
		reactive_objects$review<-reactive_objects$sites[reactive_objects$sites$IR_FLAG%in%input$flag_checkbox & reactive_objects$sites$IR_COMMENT%in%input$reason_checkbox & reactive_objects$sites$MonitoringLocationTypeName%in%input$sitetype_checkbox & reactive_objects$sites$AU_Type %in% input$autype_checkbox,]
		reactive_objects$other_sites<-reactive_objects$sites[!(reactive_objects$sites$UID%in%reactive_objects$review$UID),]	
		reactive_objects$review_points<-st_as_sf(reactive_objects$review, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326, remove=FALSE) # crs 4326 is WGS84
		if(dim(reactive_objects$review_points)[1]>0){
			review_map<-leaflet(reactive_objects$review) %>%
				addTiles() %>%
				addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
				addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
				addPolygons(data=au_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1,weight=3,color="orange",
					popup = paste0(
						"AU ID: ", au_poly$ASSESS_ID,
						"<br> AU description: ", au_poly$AU_DESCRIP,
						"<br> Basin: ", au_poly$MGMT_UNIT,
						"<br> AU type: ", au_poly$AU_Type,
						"<br> Assessment: ", au_poly$ASSESSMENT,
						"<br> Impairment: ", au_poly$IMPAIRMENT)
				)%>%
				addPolygons(data=bu_poly,group="Benficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="purple",
					popup = paste0(
						"<br> BU description: ", bu_poly$R317Descrp,
						"<br> BU class: ", bu_poly$BenUseClas,
						"<br> Mgmt unit: ", bu_poly$Mgmt_Unit,
						"<br> Waterbody type: ", bu_poly$Water_Type)
				)%>%
				addPolygons(data=ut_poly,group="State of Utah",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green",
				)%>%
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),
					overlayGroups = c("Benficial uses", "Assessment units","State of Utah","Sites","Site labels"),
					options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE)) %>%
				hideGroup("Assessment units")%>%
				hideGroup("Benficial uses")%>%
				hideGroup("Site labels")%>%
				hideGroup("State of Utah")%>%
				addLegend(position = 'topright',
					colors = ~pal(unique(reactive_objects$sites$IR_FLAG)), 
					labels = ~unique(reactive_objects$sites$IR_FLAG))%>%
				addMeasure(position = "topright", primaryLengthUnit = "meters")%>%
				addFeatures(reactive_objects$review_points,group="Sites",
					popup = paste0(
						"Organization: ", reactive_objects$review_points$OrganizationIdentifier,
						"<br> MLID: ", reactive_objects$review_points$MonitoringLocationIdentifier,
						"<br> MLNAME: ", reactive_objects$review_points$MonitoringLocationName,
						"<br> MLTYPE: ", reactive_objects$review_points$MonitoringLocationTypeName,
						"<br> IR_FLAG: ", reactive_objects$review_points$IR_FLAG,
						"<br> IR_COMMENT: ", reactive_objects$review_points$IR_COMMENT,
						"<br> Lat: ", reactive_objects$review_points$LatitudeMeasure,
						"<br> Long: ", reactive_objects$review_points$LongitudeMeasure,
						"<br> MLID_Count: ", reactive_objects$review_points$MLID_Count,
						"<br> Lat_Count: ", reactive_objects$review_points$Lat_Count,
						"<br> Long_Count: ", reactive_objects$review_points$Long_Count,
						"<br> sites100m_count: ", reactive_objects$review_points$sites100m_count
					),
					color=~pal(reactive_objects$review_points$IR_FLAG))%>%
				addLabelOnlyMarkers(lng=reactive_objects$review_points$LongitudeMeasure, lat=reactive_objects$review_points$LatitudeMeasure,group="Site labels",label=~(reactive_objects$review_points$MonitoringLocationIdentifier),labelOptions = labelOptions(noHide = T),
					clusterOptions=markerClusterOptions(spiderfyOnMaxZoom=T))		
			reactive_objects$polysel<-callModule(editMod, "selection", review_map)

		}else{
			review_map<-leaflet(reactive_objects$sites) %>%
				addTiles() %>%
				addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
				addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
				addPolygons(data=au_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1,weight=3,color="orange",
					popup = paste0(
						"AU ID: ", au_poly$ASSESS_ID,
						"<br> AU description: ", au_poly$AU_DESCRIP,
						"<br> Basin: ", au_poly$MGMT_UNIT,
						"<br> AU type: ", au_poly$AU_Type,
						"<br> Assessment: ", au_poly$ASSESSMENT,
						"<br> Impairment: ", au_poly$IMPAIRMENT)
				)%>%
				addPolygons(data=bu_poly,group="Benficial uses",fillOpacity = 0.1,weight=3,color="purple",
					popup = paste0(
						"<br> BU description: ", bu_poly$R317Descrp,
						"<br> BU class: ", bu_poly$BenUseClas,
						"<br> Mgmt unit: ", bu_poly$Mgmt_Unit,
						"<br> Waterbody type: ", bu_poly$Water_Type)
				)%>%
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),
					overlayGroups = c("Benficial uses", "Assessment units","Sites","Site labels"),
					options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE)) %>%
				hideGroup("Assessment units")%>%
				hideGroup("Benficial uses")%>%
				hideGroup("Site labels")%>%
				addLegend(position = 'topright',
					colors = ~pal(unique(reactive_objects$sites$IR_FLAG)), 
					labels = ~unique(reactive_objects$sites$IR_FLAG))%>%
				addMeasure(position = "topright", primaryLengthUnit = "meters")
			reactive_objects$polysel<-callModule(editMod, "selection", review_map)

		}
	})

	observe({
		req(reactive_objects$polysel)
		removeModal()
	})
	
	output$hot <-renderRHandsontable({
		req(reactive_objects$polysel)
		req(reactive_objects$polysel()$finished)
		suppressWarnings({
			suppressMessages({
				if(dim(reactive_objects$polysel()$finished)[1]>=1){
					intersection <- st_intersection(reactive_objects$polysel()$finished, reactive_objects$review_points)
				}
			})
		})
		req(intersection)
		intersection=st_set_geometry(intersection,NULL)
		intersection=intersection[,-c(1,2)]
		reactive_objects$hot_data<-reactive_objects$review[reactive_objects$review$UID%in%intersection$UID,]
		MLID_Choices=append(as.vector(reactive_objects$hot_data$MonitoringLocationIdentifier),"REJECT")
		rhandsontable(data.frame(reactive_objects$hot_data),readOnly=TRUE)%>%
			hot_col(col="IR_FLAG",type="dropdown",readOnly=FALSE,source=IR_flag_choices)%>%
			hot_col(col="IR_MLID",type="dropdown",readOnly=FALSE,source=MLID_Choices)%>%
			hot_col(col="IR_COMMENT",readOnly=FALSE,type="dropdown",source=reactive_objects$reason_choices)#%>%
	})

	####Potential plotting example
	#output$plotwindow=renderPlot({
	#	req(input$hot)
	#	req(polysel()$finished)
	#	other=review[!(review$UID%in%reactive_objects$hot_data$UID),]
	#	if(input$plot_type=="Boxplot"){
	#		par(mar=c(2.1,4.1,3.1,1.1))
	#		boxplot(review$randomnorm2,reactive_objects$hot_data$randomnorm2,other$randomnorm2,names=c("All","Selected","Other"),main="Random data")
	#	}
	#	if(input$plot_type=="Scatter plot"){
	#		par(mfrow=c(2,2),mar=c(4.1,4.1,3.1,1.1))
	#		plot(randomnorm2~randomnorm,data=reactive_objects$hot_data,main="Selected",cex=2)
	#		lm1=lm(randomnorm2~randomnorm,data=reactive_objects$hot_data)
	#		abline(lm1$coefficients[1],lm1$coefficients[2],lwd=3,lty=2,col="green")
	#		plot(randomnorm2~randomnorm,data=other,main="Other",cex=2)
	#		lm2=lm(randomnorm2~randomnorm,data=other)
	#		abline(lm2$coefficients[1],lm2$coefficients[2],lwd=3,lty=2,col="blue")
	#		plot(randomnorm2~randomnorm,data=review,main="All",cex=2)
	#		lm3=lm(randomnorm2~randomnorm,data=review)
	#		abline(lm3$coefficients[1],lm3$coefficients[2],lwd=3,lty=2,col="orange")
	#	}
	#})


	observeEvent(input$save, {
		req(input$hot)
		edits=hot_to_r(input$hot)

		#Require MLID input before saving (i.e. MLID!="REVIEW" & MLID!="REJECT")
		if(any(edits$IR_MLID=="REVIEW") | any(edits$IR_FLAG=="REVIEW") | any(edits$IR_COMMENT=="Manual review required")){
			showModal(modalDialog(title="TABLE UPDATES REQUIRED...","Please update IR_FLAG, IR_MLID, and IR_COMMENT columns for all selected sites before saving.",size="l"))
		}else{
			#Set validation type to MANUAL
			edits$ValidationType="MANUAL"
			
			#Auto-fill IR_Lat & IR_Long
			lat_long=reactive_objects$sites[,c("MonitoringLocationIdentifier","LatitudeMeasure","LongitudeMeasure")]
			edits=edits[,!names(edits) %in% c("LatitudeMeasure","LongitudeMeasure")]
			edits=merge(edits,lat_long,by.x="IR_MLID",by.y="MonitoringLocationIdentifier",all.x=T)
			edits$IR_Lat=edits$LatitudeMeasure
			edits$IR_Long=edits$LongitudeMeasure
			
			#save running csv of all edits
				if(!file.exists(paste0(edit_log_path,"//edit_log.csv"))){
					write.csv(edits,file=paste0(edit_log_path,"//edit_log.csv"),row.names=FALSE)
				}else{
					edits1=read.csv(file=paste0(edit_log_path,"//edit_log.csv"))
					edits2=rbind(edits1,edits)
					write.csv(edits2,file=paste0(edit_log_path,"//edit_log.csv"),row.names=FALSE)
				}

			#Remove edited rows from review
				review_locenv=reactive_objects$review[!(reactive_objects$review$UID%in%edits$UID),]			

			#Append edited rows to review, then review to sites (in hindsight, could have edited sites directly and saved a couple lines of code, may update this in future)
				reactive_objects$review<-rbind(review_locenv,edits)
				output=rbind(reactive_objects$other_sites,review_locenv,edits)
			
			showModal(modalDialog(title="EDITS SAVING...",size="l",footer=NULL))
			#Save sites to external file
				write.csv(output,file=master_site_file,row.names=FALSE)
			
			#Message
				removeModal()
				showModal(modalDialog(title="EDITS SAVED","Clear polygon before proceeding.",size="l",footer=modalButton("OK")))

  	
		}
	},ignoreInit=TRUE)
	
	observeEvent(input$refresh, {
		#Refresh panel to re-read / generate points & map after reconciling site file with edits.
		showModal(modalDialog(title="PLEASE WAIT...","Please wait for refresh before proceeding.",size="l",footer=NULL))
		js$refresh()
	})
	
	
}


## run app 
shinyApp(ui = ui, server = server)
