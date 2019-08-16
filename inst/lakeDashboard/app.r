### Utah DWQ Lake Profile Dashboard
### Jake Vander Laan, Utah DWQ, jvander@utah.gov
### Version 2.0, 8/14/2019

library(wqTools)
library(magrittr)


heatmap_param_choices=c("Dissolved oxygen (DO)","Temperature, water","pH","DO-temperature habitat profile width")
names(heatmap_param_choices)=c("Dissolved oxygen", "Temperature", "pH", "DO/temperature lens")
au_poly=wqTools::au_poly

ui <-fluidPage(
tags$head(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript")
      ),
	  
	# Header
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo_draft.png', height = 125, width = 100*2.85*1.75), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Lake profile dashboard")
	),

	# Input widgets
	fluidRow(
		column(5,
			conditionalPanel(condition="input.plot_tabs!='User guide'",
				tabsetPanel(id="ui_tab",
					tabPanel("Map",
						column(12,h4("Click a site or AU"),shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))
					),
					tabPanel("Table",
						column(12, h4("Click a row"), div(DT::dataTableOutput("table_input"), style = "font-size:70%"))
					)
				)
			),
			conditionalPanel(condition="input.plot_tabs=='User guide'",
				column(12)
			)
		),
		column(7,tabsetPanel(id="plot_tabs",

			tabPanel("Profile time series",
				fluidRow(column(8,
					uiOutput("date_slider"),
					radioButtons("ts_plot_type","Plot type:", choices=c("Heatmap", "Habitable width", "Water column exceedances"), inline=T),
					conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
						selectInput("heatmap_param",label="Heatmap parameter:",choices=heatmap_param_choices)
					),
					checkboxInput("show_dates", label="Show all profile dates", value=TRUE),
					conditionalPanel(condition="input.ts_plot_type=='Heatmap'",
						plotOutput("heatmap")
					),
					conditionalPanel(condition="input.ts_plot_type=='Habitable width'",
						plotOutput("hab_width")
					),
					conditionalPanel(condition="input.ts_plot_type=='Water column exceedances'",
						plotOutput("pct_exc")
					)
				))
			),
			tabPanel("Individual profiles",
				fluidRow(
					column(4, uiOutput("date_select"))
				),
				fluidRow(
					column(4,h4("Profile plot"),plotOutput("ind_prof_plot", height="500px")),
					column(8,h4("Profile data"),div(DT::dataTableOutput("profile_table"), style = "font-size:80%"))
				)
			),
			tabPanel("Trophic indicators",
				shinyWidgets::radioGroupButtons('trophic_type', 'Plot type:', choices=c('Time series','Boxplot','Scatter plot'), checkIcon = list(yes = icon("check"))),
				conditionalPanel(condition="input.trophic_type=='Time series'"),
				conditionalPanel(condition="input.trophic_type=='Boxplot'",
					plotlyOutput('tsi_boxplot', height="600px", width="900px")
				),
				conditionalPanel(condition="input.trophic_type=='Scatter plot'",
					plotOutput('tsi3d', height="600px", width="600px")
				)
			),
			tabPanel("User guide",
				fluidRow(
					column(8,
						includeMarkdown('./user_guide/user_guide.rmd')
					)
				)
			)
		))
	)
)

server <- function(input, output, session){

	# Loading modal to keep user out of trouble while map draws...
	showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))

	# Remove modal when app is ready
	observe({
		req(map,mlid_param_asmnts)
		removeModal()
	})

	# Load data
	load("./data/lake_data.rdata")

	# Subset polygons to lake polygons
	au_poly=wqTools::au_poly
	lake_aus=au_poly[au_poly$AU_Type=="Reservoir/Lake",]

	# Extract site locations
	prof_sites=unique(prof_asmnts_all$profile_asmnts_mlid_param[,c("ASSESS_ID","AU_NAME","IR_MLID","IR_MLNAME","IR_Lat","IR_Long")])
	prof_sites$MonitoringLocationTypeName="Lake/Reservoir"
	prof_sites=plyr::rename(prof_sites, c("IR_Lat"="LatitudeMeasure", "IR_Long"="LongitudeMeasure","IR_MLID"="MonitoringLocationIdentifier","IR_MLNAME"="MonitoringLocationName"))
	
	# Extract profiles long
	profiles_long=prof_asmnts_all$profiles_long
	profiles_long$MonitoringLocationIdentifier=profiles_long$IR_MLID
	profiles_long=unique(profiles_long[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","R3172ParameterName","IR_Value","IR_Unit","NumericCriterion","MonitoringLocationIdentifier")])
	profiles_long$ActivityStartDate=as.Date(profiles_long$ActivityStartDate,format='%Y-%m-%d')

	# Remove profiles where depths are not provided
	depths=profiles_long[profiles_long$R3172ParameterName=="Profile depth",]
	depth_actids=unique(depths$ActivityIdentifier)
	profiles_long=profiles_long[profiles_long$ActivityIdentifier %in% depth_actids,]

	# Remove any sites that do not produce any valid profiles
	prof_sites=prof_sites[prof_sites$MonitoringLocationIdentifier %in% profiles_long$MonitoringLocationIdentifier,]

	# Extract profiles wide
	profiles_wide=prof_asmnts_all$profiles_wide
	profiles_wide=profiles_wide[profiles_wide$ActivityIdentifier %in% profiles_long$ActivityIdentifier,]
	profiles_wide$ActivityStartDate=as.Date(profiles_wide$ActivityStartDate,format='%Y-%m-%d')

	# Calc max depth for each profile
	max_depth=aggregate(Depth_m~ActivityIdentifier,data=profiles_wide, FUN='max', na.rm=T)
	names(max_depth)[names(max_depth)=="Depth_m"]="max_depth_m"

	# Extract individual profile assessments
	ind_prof_asmnts=prof_asmnts_all$profile_asmnts_individual
	ind_prof_asmnts=ind_prof_asmnts[ind_prof_asmnts$ActivityIdentifier %in% profiles_long$ActivityIdentifier,]
	ind_prof_asmnts$ActivityStartDate=as.Date(ind_prof_asmnts$ActivityStartDate,format='%Y-%m-%d')
	ind_prof_asmnts=merge(ind_prof_asmnts,max_depth,all.x=T)
	ind_prof_asmnts=within(ind_prof_asmnts,{
		ph_pct_exc=pH_exc_cnt/samp_count*100
		temp_pct_exc=temp_exc_cnt/samp_count*100
		do_pct_exc=do_exc_cnt/samp_count*100
	})

	# Extract mlid/param level assessments
	mlid_param_asmnts=prof_asmnts_all$profile_asmnts_mlid_param
	mlid_param_asmnts=mlid_param_asmnts[,!names(mlid_param_asmnts) %in% c("IR_Lat","IR_Long","IR_MLNAME")]
	names(mlid_param_asmnts)[names(mlid_param_asmnts)=='IR_Cat']='prelim_asmnt'
	
	# Empty reactive values object
	reactive_objects=reactiveValues()

	# Select map set up
    map = leaflet::createLeafletMap(session, 'map')

    session$onFlushed(once = T, function() {
		output$map <- leaflet::renderLeaflet({
			buildMap(sites=prof_sites, plot_polys=TRUE, au_poly=lake_aus) %>% leaflet::showGroup('Assessment units') %>% leaflet::clearControls()
		})
    })

	# Table interface
	output$table_input=DT::renderDataTable({
		DT::datatable(mlid_param_asmnts, selection='single', rownames=FALSE, filter="top",
			options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr"#,
				#searchCols = list(NULL,list(search=paste(reactive_objects$sel_mlid)))
			)
		)
	})

	# Map marker click (to identify selected site)
	observeEvent(input$map_marker_click, {
		req(profiles_long)
		site_click <- input$map_marker_click
		if (is.null(site_click)){return()}
		siteid=site_click$id
		reactive_objects$sel_mlid=siteid
		reactive_objects$selected_au=as.character(mlid_param_asmnts[mlid_param_asmnts$IR_MLID==siteid,"ASSESS_ID"][1])
	})

	# Map AU click (to identify selected AU and site)
	observeEvent(input$map_shape_click,{
		au_click = input$map_shape_click$id
		if(!is.null(au_click)){
			reactive_objects$selected_au=as.character(unique(au_poly$ASSESS_ID[au_poly$polyID==au_click]))
			if(is.null(reactive_objects$sel_mlid)){
				reactive_objects$sel_mlid=mlid_param_asmnts[mlid_param_asmnts$ASSESS_ID==reactive_objects$selected_au,"IR_MLID"][1]
			}else{
				if((!prof_sites[prof_sites$MonitoringLocationIdentifier==reactive_objects$sel_mlid,"ASSESS_ID"][1]==reactive_objects$selected_au) | (is.na(reactive_objects$sel_mlid))){
					reactive_objects$sel_mlid=mlid_param_asmnts[mlid_param_asmnts$ASSESS_ID==reactive_objects$selected_au,"IR_MLID"][1]
				}
			}
		}
	})

	observe({
		print(reactive_objects$sel_mlid)
	})
	
	
	# Table row click (to identify selected site & parameter)
	observe({
		req(input$table_input_rows_selected)
		row_click=input$table_input_rows_selected
		siteid=mlid_param_asmnts[row_click,"IR_MLID"]
		reactive_objects$sel_param=mlid_param_asmnts[row_click,"R3172ParameterName"]
		reactive_objects$sel_mlid=siteid
		reactive_objects$selected_au=as.character(mlid_param_asmnts[mlid_param_asmnts$IR_MLID==siteid,"ASSESS_ID"][1])
	})

	# Change map zoom on table click & update selected heatmap_param to selected row param
	map_proxy=leaflet::leafletProxy("map")
	observeEvent(input$table_input_rows_selected,{
		lat=prof_sites[prof_sites$MonitoringLocationIdentifier==reactive_objects$sel_mlid,"LatitudeMeasure"]
		long=prof_sites[prof_sites$MonitoringLocationIdentifier==reactive_objects$sel_mlid,"LongitudeMeasure"]
		map_proxy %>% leaflet::setView(lng=long, lat=lat, zoom=12)
		updateSelectInput(session, "heatmap_param",selected=reactive_objects$sel_param)
	})


	# Select profiles & date options based on selected site ID
	observe({
		req(reactive_objects$sel_mlid)
		reactive_objects$sel_profiles=profiles_long[profiles_long$MonitoringLocationIdentifier==reactive_objects$sel_mlid,]
		profile_dates=unique(reactive_objects$sel_profiles$ActivityStartDate)
		profile_dates=profile_dates[order(profile_dates)]
		reactive_objects$profile_dates=profile_dates
	})


	# Filter table to match clicked site from map
	input_table_proxy = DT::dataTableProxy('table_input')
	observeEvent(reactive_objects$sel_mlid,{
		input_table_proxy %>% DT::clearSearch() %>% DT::updateSearch(keywords = list(global = "", columns=c("","",paste(reactive_objects$sel_mlid))))
	})

	# Profile date selection
	output$date_select <- renderUI({
		req(reactive_objects$profile_dates)
		selectInput("date_select", "Profile date:", reactive_objects$profile_dates)
	})
	output$date_slider <- renderUI({
		req(reactive_objects$profile_dates)
		date_min=min(reactive_objects$profile_dates)
		date_max=max(reactive_objects$profile_dates)
		sliderInput("date_slider", "Date range:", min=date_min, max=date_max, value=c(date_min,date_max))
	})

	# Generate selected aid
	observe({
		req(input$date_select)
		reactive_objects$selectedActID=reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityStartDate==input$date_select,"ActivityIdentifier"][1]
	})

	# Profile plot output
	output$ind_prof_plot=renderPlot({
		req(reactive_objects$sel_profiles,reactive_objects$selectedActID)
		one_profile=reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityIdentifier==reactive_objects$selectedActID,]

		do_crit=one_profile[one_profile$R3172ParameterName=="Dissolved oxygen (DO)","NumericCriterion"][1]
		temp_crit=one_profile[one_profile$R3172ParameterName=="Temperature, water","NumericCriterion"][1]

		one_profile=unique(one_profile[,c("DataLoggerLine","ActivityIdentifier","ActivityStartDate","R3172ParameterName","IR_Value","IR_Unit","MonitoringLocationIdentifier")])


		profilePlot(one_profile, parameter = "R3172ParameterName",
			units = "IR_Unit",
			depth = "Profile depth", do = "Dissolved oxygen (DO)",
			temp = "Temperature, water", pH = "pH",
			value_var = "IR_Value", line_no = "DataLoggerLine",
			pH_crit=c(6.5,9), do_crit=do_crit, temp_crit=temp_crit)
		box()
	})

	# Data table output
	observe({
		req(reactive_objects$selectedActID)
		table_data=profiles_wide[profiles_wide$ActivityIdentifier==reactive_objects$selectedActID,c("IR_MLID","ActivityStartDate","Depth_m","DO_mgL","pH","Temp_degC","do_exc","pH_exc","temp_exc")]
		reactive_objects$table_data=table_data[order(table_data$Depth_m),]
	})
	output$profile_table=DT::renderDataTable({
		req(reactive_objects$table_data)
		DT::datatable(reactive_objects$table_data, selection='multiple',
			options = list(scrollY = '500px', paging = FALSE, scrollX = TRUE, searching=F)
		) %>%
		DT::formatStyle("DO_mgL", "do_exc", backgroundColor = DT::styleEqual(1, "orange"))  %>%
		DT::formatStyle("pH", "pH_exc", backgroundColor = DT::styleEqual(1, "orange"))  %>%
		DT::formatStyle("Temp_degC", "temp_exc", backgroundColor = DT::styleEqual(1, "orange"))
	})

	prof_table_proxy = DT::dataTableProxy('profile_table')
	observe({
		prof_table_proxy %>% DT::hideCols(hide=which(names(reactive_objects$table_data) %in% c("do_exc","pH_exc","temp_exc")))
	})

	# Extract selected rows...
	#observeEvent(input$profile_table_rows_selected,{
	#	print(input$profile_table_rows_selected)
	#})


	# Extract profile assessments & profiles_wide for selected site
	observe({
		req(reactive_objects$sel_mlid,input$date_slider)
		selected_prof_asmnts=ind_prof_asmnts[
			ind_prof_asmnts$IR_MLID == reactive_objects$sel_mlid &
			ind_prof_asmnts$ActivityStartDate>=input$date_slider[1] &
			ind_prof_asmnts$ActivityStartDate<=input$date_slider[2]
		,]
		selected_prof_asmnts=selected_prof_asmnts[order(selected_prof_asmnts$ActivityStartDate),]
		reactive_objects$selected_prof_asmnts=selected_prof_asmnts

		reactive_objects$sel_profs_wide=profiles_wide[
			profiles_wide$IR_MLID == reactive_objects$sel_mlid &
			profiles_wide$ActivityStartDate>=input$date_slider[1] &
			profiles_wide$ActivityStartDate<=input$date_slider[2]
		,]
	})

	# Hab width plot output
	output$hab_width=renderPlot({
		req(reactive_objects$selected_prof_asmnts)
		if(dim(reactive_objects$selected_prof_asmnts)[1]>0){
			par(mar=c(7.1,5.1,7.1,2.1))
			plot(max_hab_width~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=NA, cex=1.5, ylab="Width (m)", xlab="", cex.axis=1.25, cex.lab=1.5, xaxt='n',
				ylim=c(0,max(reactive_objects$selected_prof_asmnts$max_depth_m,na.rm=T))
			)
			abline(h=3,lty=3,lwd=2,col="red")
			if(input$show_dates){
				axis(1, at=unique(reactive_objects$selected_prof_asmnts$ActivityStartDate), labels=unique(as.Date(reactive_objects$selected_prof_asmnts$ActivityStartDate)), par(las=2))
			}else{
				axis.Date(1, reactive_objects$selected_prof_asmnts$ActivityStartDate)
			}
			points(max_depth_m~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, type='l',lty=2,lwd=2,col="blue")
			points(max_hab_width~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, type='b', pch=21, cex=1.5, bg="grey", cex.axis=1.25, cex.lab=1.5)
			par(xpd=TRUE)
				legend("topleft", inset=c(0.05,-0.3), bty='n', pch=c(NA,21),pt.bg=c(NA,'grey'),lty=c(2,1),col=c("blue","black"),lwd=c(2,1),cex=1.5, legend=c("Max depth","DO/temp habitat"))
			par(xpd=FALSE)
		}
	})


	# pct exceedance plot
	output$pct_exc=renderPlot({
		req(reactive_objects$selected_prof_asmnts)
		if(dim(reactive_objects$selected_prof_asmnts)[1]>0){
			ymax=max(5,max(max(reactive_objects$selected_prof_asmnts$do_pct_exc, na.rm=T),max(reactive_objects$selected_prof_asmnts$temp_pct_exc, na.rm=T),max(reactive_objects$selected_prof_asmnts$ph_pct_exc, na.rm=T))*1.1)

			par(mar=c(7.1,5.1,7.1,2.1))

			plot(do_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts,ylim=c(0,ymax), pch=24, bg="deepskyblue3", type='b', ylab="% exceedance", cex=1.5, xlab="", xaxt='n')
			points(temp_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=21, bg="orange", type='b', cex=1.5)
			points(ph_pct_exc~ActivityStartDate, data=reactive_objects$selected_prof_asmnts, pch=22, bg="green", type='b', cex=1.5)
			if(input$show_dates){
				axis(1, at=unique(reactive_objects$selected_prof_asmnts$ActivityStartDate), labels=unique(as.Date(reactive_objects$selected_prof_asmnts$ActivityStartDate)), par(las=2))
			}else{
				axis.Date(1, reactive_objects$selected_prof_asmnts$ActivityStartDate)
			}
			par(xpd=TRUE)
			legend("topleft", inset=c(0.05,-0.3), bty='n',horiz=T,
				legend=c("Dissolved oxygen","Temperature","pH"),
				pch=c(24,21,22), pt.bg=c("deepskyblue3","orange","green"), cex=1.5)
			par(xpd=FALSE)
		}
	})

	# Profile heatmap plot
	output$heatmap=renderPlot({
		req(reactive_objects$sel_profs_wide, reactive_objects$sel_profiles)
		if(dim(reactive_objects$sel_profs_wide)[1]>0){
			if(length(unique(reactive_objects$sel_profs_wide$ActivityStartDate))==1 | dim(reactive_objects$sel_profs_wide)[1]<=2){
				plot.new()
				text(0.5,0.5,"Cannot interpolate. See individual profiles.")
				box()
			}else{
				# Define heatmap inputs based on selected parameter
				if(input$heatmap_param=="Dissolved oxygen (DO)"){
					name="Dissolved oxygen (DO)"
					parameter="DO_mgL"
					param_units="mg/L"
					param_lab="Dissolved oxygen"
				}
				if(input$heatmap_param=="pH"){
					name="pH"
					parameter="pH"
					param_units=""
					param_lab="pH"
				}
				if(input$heatmap_param=="Temperature, water"){
					name="Temperature, water"
					parameter="Temp_degC"
					param_units="deg C"
					param_lab="Temperature"
				}
				if(input$heatmap_param=="DO-temperature habitat profile width"){
					name="DO/temperature lens"
					parameter="do_temp_exc"
					param_units=""
					param_lab="DO/temp exc."
				}
				# Define criteria
				if(input$heatmap_param!="DO-temperature habitat profile width"){
					criteria=unique(reactive_objects$sel_profiles[reactive_objects$sel_profiles$R3172ParameterName==name,"NumericCriterion"])
				}else{criteria=1}
				# heat map
				if(input$show_dates){show_dates=TRUE}else{show_dates=FALSE}
				profileHeatMap(reactive_objects$sel_profs_wide,parameter=parameter,param_units=param_units,param_lab=param_lab,depth="Depth_m",depth_units="m",criteria=criteria,show_dates=show_dates)
		}
		}
	})


	# Trophic indicators tab
	## Extract trophic data
	observeEvent(reactive_objects$selected_au, ignoreInit=T, {
		
		#reactive_objects=list()
		#reactive_objects$selected_au='UT-L-16020201-004_01'
		
		trophic_data_flat=trophic_data[trophic_data$ASSESS_ID==reactive_objects$selected_au,]
		trophic_data_flat$TSI[trophic_data_flat$TSI<1]=1
		tsi_wide=reshape2::dcast(trophic_data_flat, MonitoringLocationIdentifier+ActivityStartDate+AU_NAME+ASSESS_ID~CharacteristicName, value.var='TSI', fun.aggregate=mean, na.rm=T)
		reactive_objects$trophic_data_flat=trophic_data_flat
		reactive_objects$tsi_wide=plyr::rename(tsi_wide, c('Chlorophyll a'='TSIchl', 'Depth, Secchi disk depth'='TSIsd', 'Phosphate-phosphorus'='TSItp'))
	})
	
	## 3D TSI plot
	### plot function
	plot3dTSI=function(data, title){
		data=na.omit(data)
		ChlA_Secchi=data$TSIchl-data$TSIsd
		ChlA_TP=data$TSIchl-data$TSItp
		
			plot(NA,NA,xaxt='n',yaxt='n',ylim=c(-100,100),xlim=c(-100,100),ylab="",xlab="",bty="n",main=title,cex.axis=2,cex.lab=1.5)
			axis(1,at=c(-50,50),pos=0)
			axis(1,at=seq(-75,75,25),pos=0,labels=F)
			axis(2,at=c(-50,50),pos=0,las=1)
			axis(2,at=seq(-75,75,25),pos=0,las=1,labels=F)
			axis(2,at=seq(-75,75,25),pos=0,las=1,labels=F)
			segments(-55,-55,55,55,lty=2,lwd=2,col="dimgrey")	
			arrows(40,-40,52,-52,lwd=2,col="dimgrey",length=0.125)
			arrows(-40,40,-52,52,lwd=2,col="dimgrey",length=0.125)
			rect(-100,-100,100,100)
			
			points(ChlA_Secchi,ChlA_TP,pch=21,col="black",bg="orange",cex=3)
			#points(mean(ChlA_Secchi),mean(ChlA_TP),pch="+",col="darkgreen",cex=5)
			
			par(xpd=NA)
			text(x=-75,y=110,"Small particulates")
			arrows(-15,110,-45,110,lwd=2,col="dimgrey",length=0.125)
			text(x=75,y=110,"Large particulates")
			arrows(15,110,45,110,lwd=2,col="dimgrey",length=0.125)
			text(x=-50,y=-110,"TSI ChlA < TSI Secchi")
			text(y=-125,x=0,"TSI ChlA - TSI Secchi", cex=1.5)
			text(x=-125,y=0,"TSI ChlA - TSI TP", cex=1.5,srt=90)
			text(x=50,y=-110,"TSI ChlA > TSI Secchi")
			text(x=-110,y=-50,"TSI ChlA < TSI TP",srt=90)
			text(x=-110,y=50,"TSI ChlA > TSI TP",srt=90)
			text(x=50,y=-90,"Zooplankton grazing")
			text(x=-50,y=90,"Dissolved color/clay particles")
			text(x=57,y=-57,"TSI Secchi < TSI TP",srt=45)
			text(x=-57,y=57,"TSI Secchi > TSI TP",srt=45)
			text(x=-95,y=-65,"P surplus",srt=90)
			text(x=-95,y=65,"P limitation",srt=90)
			text(x=40,y=40,adj=c(0,-0.5),"TSI Secchi = TSI TP",srt=45)
			arrows(-95,-15,-95,-45,lwd=2,col="dimgrey",length=0.125)
			arrows(-95,15,-95,45,lwd=2,col="dimgrey",length=0.125)	
	}

	
	output$tsi3d=renderPlot({
		req(reactive_objects$tsi_wide)
		plot3dTSI(reactive_objects$tsi_wide, title=reactive_objects$tsi_wide$AU_NAME[1])
	})

	output$tsi_boxplot=renderPlotly({
		
		req(reactive_objects$trophic_data_flat)
		title=reactive_objects$trophic_data_flat$AU_NAME[1]
		au_vis=as.list(append(T, rep(F, length(unique(reactive_objects$trophic_data_flat$MonitoringLocationIdentifier)))))
		site_vis=as.list(append(F, rep(T, length(unique(reactive_objects$trophic_data_flat$MonitoringLocationIdentifier)))))
		suppressWarnings(suppressMessages(
			plot_ly(data=reactive_objects$trophic_data_flat) %>%
				add_trace(type = 'box', y = ~TSI, x=~CharacteristicName, visible=T, name='TSI') %>%
				add_trace(type = 'box', y = ~TSI, x=~CharacteristicName, color=~MonitoringLocationIdentifier, visible=F) %>%
				layout(title = title,
					boxmode = "group",
					yaxis = list(side = 'left', title = 'TSI'),
					updatemenus = list(
						list(
							buttons = list(
								list(method = "update", label='Group to AU', 
									args = list(list(visible = au_vis))
								),
								list(method = "update", label='Split by site', 
									args = list(list(visible = site_vis))
								)
							)
						)
					)
				) %>% 
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'select2d',
						'lasso2d'
					)
				)
		))
	})




}

## run app
shinyApp(ui = ui, server = server)
