# Figures module

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard\\modules')
load('figures-test-data.Rdata')
library(leaflet)
library(wqTools)

ui <-fluidPage(
#figuresUI <- function(id){
	#ns <- NS(id)
#}
	
	tags$style(type = "text/css", "html, body {width:100%;height:100%}",
		".leaflet .legend i{
		border-radius: 50%;
		width: 10px;
		height: 10px;
		margin-top: 4px;
		}"
	),
	
	fluidRow(
		column(2,uiOutput('sel_param1'), uiOutput('sel_units1')),
		conditionalPanel("input.tabs=='Multiple parameters'",
			column(2,
				uiOutput('sel_param2'),
				uiOutput('sel_units2')
			),
			column(2,
				uiOutput('sel_site')
			)
		)
	),
	tabsetPanel(id='tabs',
		tabPanel('Multiple sites',
			fluidRow(column(3,radioButtons("compare_plottype", "Plot type", choices = c("Time series","Boxplot", "Concentration map"), selected = "Time series", inline = TRUE))),
			conditionalPanel("input.compare_plottype == 'Time series'", plotlyOutput('multi_site_ts')),
			conditionalPanel("input.compare_plottype == 'Boxplot'", plotlyOutput('multi_site_bp')),
			conditionalPanel("input.compare_plottype == 'Concentration map'", shinycssloaders::withSpinner(leafletOutput('conc_map'),size=2, color="#0080b7"))
		),
		tabPanel("Multiple parameters", 
			plotlyOutput("multi_param_ts")
		)
	)
)



shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7")



server <- function(input, output, session){
	# Date formats
	sel_data$ActivityStartDate=as.Date(sel_data$ActivityStartDate)
	sel_crit$ActivityStartDate=as.Date(sel_crit$ActivityStartDate)
	
	# Empty reactive objects
	reactive_objects=reactiveValues()
	
	
	# Make sure numeric criterion is numeric
	sel_crit$NumericCriterion=as.numeric(sel_crit$NumericCriterion)
	
	# Select param 1
	output$sel_param1 <- renderUI({
		selectInput("sel_param1","Select Parameter 1", choices = c(sel_data$R3172ParameterName))
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param1, 'IR_Unit'])
		selectInput("sel_units1","Select Units 1", choices = units)
	})

	# Select param 2
	output$sel_param2 <- renderUI({
		param_choices=sel_data$R3172ParameterName[! sel_data$R3172ParameterName %in% input$sel_param1]
		selectInput("sel_param2","Select Parameter 2", choices = param_choices)
	})

	# Select units 2
	output$sel_units2 <- renderUI({
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param2, 'IR_Unit'])
		selectInput("sel_units2","Select Units 2", choices = units)
	})

	# Generate parameter 1 data & criteria (need to do criteria still)
	observe({
		req(input$sel_param1, input$sel_units1)
		
		## Data
		param1=subset(sel_data, R3172ParameterName == input$sel_param1)
		### Convert units if multiple available
		if(length(unique(param1$IR_Unit)>1)){
			param1$target_unit=input$sel_units1
			param1=wqTools::convertUnits(param1, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
		}else{param1$plot_value=param1$IR_Value}
		param1=param1[order(param1$ActivityStartDate),]
		reactive_objects$param1=unique(param1[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
		
		
		## Criteria
		crit1=subset(sel_crit, R3172ParameterName == input$sel_param1)
		### Convert units if multiple available
		if(length(unique(crit1$CriterionUnits)>1)){
			crit1$target_unit=input$sel_units1
			crit1=wqTools::convertUnits(crit1, input_units='CriterionUnits', target_units = "target_unit", value_var='NumericCriterion', conv_val_col='plot_value')
		}else{crit1$plot_value=crit1$NumericCriterion}
		crit1=crit1[order(crit1$ActivityStartDate),]
		reactive_objects$crit1<-crit1
	})

	# Generate parameter 2 data & criteria (need to do criteria still)
	observe({if(input$tabs=='Multiple parameters'){
		req(input$sel_param2, input$sel_units2)
		param2=subset(sel_data, R3172ParameterName == input$sel_param2)
		## Convert units if multiple available
		if(length(unique(param2$IR_Unit)>1)){
			param2$target_unit=input$sel_units2
			param2=wqTools::convertUnits(param2, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
		}else{param2$plot_value=param2$IR_Value}
		param2=param2[order(param2$ActivityStartDate),]
		reactive_objects$param2=unique(param2[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
	}})	
	
	# Multi-site figure labels & visibilities
	observe({
		req(reactive_objects$param1, reactive_objects$crit1)
		reactive_objects$title = input$sel_param1
		reactive_objects$ylab = paste0(input$sel_param1,' (', input$sel_units1,')')
		reactive_objects$ylab2 = paste0(input$sel_param2,' (', input$sel_units2,')')
		mlid_len=length(unique(reactive_objects$param1$IR_MLID))
		au_len=length(unique(reactive_objects$param1$ASSESS_ID))
		crit_plot=unique(reactive_objects$crit1[,c('ActivityStartDate','BeneficialUse','CriterionLabel','plot_value')])
		crit_plot=within(crit_plot, {
			label=paste(BeneficialUse, 'use', CriterionLabel, 'criterion')	
			label=gsub(" NA", "", label)
		})
		reactive_objects$crit_plot=crit_plot
		crit_len=length(unique(crit_plot$label))
		mlid_vis=as.list(append(rep(T, mlid_len), rep(F, au_len)))
		reactive_objects$mlid_vis=as.list(append(mlid_vis, rep('legendonly',crit_len)))
		au_vis=as.list(append(rep(F,mlid_len), rep(T, au_len)))
		reactive_objects$au_vis=as.list(append(au_vis, rep('legendonly',crit_len)))	
	})

	# Multi-site time series
	output$multi_site_ts=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$crit1, reactive_objects$au_vis)
		
		if(all(!is.na(reactive_objects$param1$plot_value))){
			plot_ly() %>%
				add_trace(type = 'scatter', mode = 'lines+markers', x=as.Date(reactive_objects$param1$ActivityStartDate), y = reactive_objects$param1$plot_value, color = reactive_objects$param1$IR_MLID, marker = list(size=10), visible=T) %>%
				add_trace(type = 'scatter', mode = 'markers',x = as.Date(reactive_objects$param1$ActivityStartDate), y=reactive_objects$param1$plot_value, color = reactive_objects$param1$ASSESS_ID, marker = list(size=10), visible=F) %>%
				add_trace(type = 'scatter', mode='lines', x = as.Date(reactive_objects$crit_plot$ActivityStartDate), y=reactive_objects$crit_plot$plot_value, color = reactive_objects$crit_plot$label, visible='legendonly') %>%
					layout(title = reactive_objects$title,
							titlefont = list(
							family = "Arial, sans-serif"),
							font = list(
							family = "Arial, sans-serif"),
							xaxis = list(title = "Date"),
							yaxis = list(title = reactive_objects$ylab),
						updatemenus = list(
							list(
								buttons = list(
									list(method = "update", label='Group by site', 
										args = list(list(visible = reactive_objects$mlid_vis))
									),
									list(method = "update", label='Group by AU', 
										args = list(list(visible = reactive_objects$au_vis))
									)
								)
							)
						)
					) %>% 
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'hoverClosestCartesian',
						'hoverCompareCartesian',
						'lasso2d',
						'select2d'
					)
				)
		}
	})

	# Multi site boxplot
	output$multi_site_bp=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$crit1, reactive_objects$au_vis)
		crit_plot=reactive_objects$crit_plot
		crit_plot<<-crit_plot
		param1<<-reactive_objects$param1
		crit_plot=unique(crit_plot[!is.na(crit_plot$plot_value),c('plot_value','label')])
		crit_plot0=crit_plot
		crit_plot0$x=0
		crit_plot1=crit_plot
		crit_plot1$x=1
		crit_plot=rbind(crit_plot0,crit_plot1)
		
		if(all(!is.na(reactive_objects$param1$plot_value))){
			plot_ly(type = 'box', y = reactive_objects$param1$plot_value, color = reactive_objects$param1$IR_MLID, visible=T) %>%
				add_trace(type = 'box', y = reactive_objects$param1$plot_value, color = reactive_objects$param1$ASSESS_ID, visible=F) %>%
				layout(title = reactive_objects$title,
					titlefont = list(
					family = "Arial, sans-serif"),
					font = list(
					family = "Arial, sans-serif"),
					xaxis = list(title = "MLID"),
					xaxis2 = list(overlaying = "x", zeroline=F, showticklabels = FALSE, showgrid = FALSE),
					yaxis = list(title = reactive_objects$ylab),
					updatemenus = list(
						list(
							buttons = list(
								list(method = "update", label='Group by site', 
									args = list(list(visible = reactive_objects$mlid_vis))
								),
								list(method = "update", label='Group by AU', 
									args = list(list(visible = reactive_objects$au_vis), list(xaxis = list(title = 'Assessment unit ID')))
								)
							)
						)
					)
				) %>%
				add_trace(type = 'scatter', mode='lines', x=crit_plot$x, y = crit_plot$plot_value, color = crit_plot$label, visible='legendonly', xaxis = "x2")%>%
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'hoverClosestCartesian',
						'hoverCompareCartesian',
						'lasso2d',
						'select2d'
					)
				)
		}
	})

		
	
	# Concentration map
	session$onFlushed(once = T, function() {
		observeEvent(input$sel_units1, ignoreInit=T, once=T,{	
			output$conc_map <- leaflet::renderLeaflet({
				
				# Map parameters
				conc_map = wqTools::buildMap(plot_polys=TRUE, search="")
				conc_map = leaflet::addLayersControl(conc_map,
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
				conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
				conc_map=hideGroup(conc_map, "Assessment units")
				conc_map=hideGroup(conc_map, "Site-specific standards")
				conc_map=hideGroup(conc_map, "Beneficial uses")
				conc_map=removeMeasure(conc_map)
			})
		})
	})

	# Map proxy
	conc_proxy = leaflet::leafletProxy("conc_map")

	# Custom leaflet legend (re-size circles)
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
		colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
		labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
		return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
	}


	# Update concentration map via proxy on param1 change
	observe({
		req(reactive_objects$param1)
		if(all(!is.na(reactive_objects$param1$plot_value))){
			sites=reactive_objects$param1
			count=aggregate(plot_value~IR_MLID+IR_Lat+IR_Long+target_unit, sites, FUN='length')
			names(count)[names(count)=='plot_value'] = 'count'
			sites=aggregate(plot_value~IR_MLID+IR_MLNAME+IR_Lat+IR_Long+target_unit, sites, FUN='mean')
			sites=merge(sites,count,all.x=T)
			sites$radius=scales::rescale(sites$plot_value, c(5,35))
			min_lat=min(sites$IR_Lat)*0.999
			min_lng=min(sites$IR_Long)*0.999
			max_lat=max(sites$IR_Lat)*1.001
			max_lng=max(sites$IR_Long)*1.001
			leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
			leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
			conc_proxy%>%clearGroup('Sites') %>% clearControls() %>%
				flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
				addCircleMarkers(data = sites, lat=~IR_Lat, lng=~IR_Long, group="Sites", layerId=~IR_MLID, color='blue', stroke=F, fillOpacity=0.5,
					radius = ~radius, options = pathOptions(pane = "site_markers"),
					popup = paste0(
						"MLID: ", sites$IR_MLID,
						"<br> ML name: ", sites$IR_MLNAME,
						"<br> Average Parameter Value: ", sites$plot_value,
						"<br> Sample Count: ", sites$count)
				) %>%
			addLegendCustom(colors = c("blue", "blue", "blue"), labels = leg_labs, sizes = leg_sizes, title=reactive_objects$ylab)
		}
	})
	
	
	# Multi-parameter time series
	## Site selection
	output$sel_site <- renderUI({
		req(reactive_objects$param1,reactive_objects$param2)
		param_site = as.character(unique(reactive_objects$param1$IR_MLID[reactive_objects$param1$IR_MLID %in% reactive_objects$param2$IR_MLID]))
		selectInput("sel_site","Select Site", choices = param_site)
	})

	output$multi_param_ts=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$param2, input$sel_units2, input$sel_site)
		param1=reactive_objects$param1[reactive_objects$param1$IR_MLID %in% input$sel_site,]
		param2=reactive_objects$param2[reactive_objects$param2$IR_MLID %in% input$sel_site,]
		if(all(!is.na(reactive_objects$param1$plot_value)) & all(!is.na(reactive_objects$param2$plot_value))){
			plot_ly(type = 'scatter', mode = 'lines+markers')%>%
				layout(title = input$sel_site,
					titlefont = list(
					family = "Arial, sans-serif"),
					font = list(
					family = "Arial, sans-serif"),
					xaxis = list(title = "Date"),
					yaxis = list(title = reactive_objects$ylab),
					yaxis2 = list(side="right", overlaying = "y",title = reactive_objects$ylab2)
				) %>% 
				add_trace(x = param1$ActivityStartDate, y = param1$plot_value, name = reactive_objects$ylab, marker = list(size = 10)) %>%
				add_trace(x = param2$ActivityStartDate, y = param2$plot_value, name = reactive_objects$ylab2, marker = list(size = 10), yaxis = "y2") %>%
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'hoverClosestCartesian',
						'hoverCompareCartesian',
						'lasso2d',
						'select2d'
					)
				)
		}
	})
	
	
}




# run app
shinyApp(ui = ui, server = server)



#	
#	##Mult params one site
#	
#	# Parameter 1 selection based on site
#	output$sel_param1 <- renderUI({
#		reactive_objects$params_1 = unique(reactive_objects$sel_data$R3172ParameterName[reactive_objects$sel_data$IR_MLID==input$sel_param_site])
#		selectInput("sel_param1", "Select Parameter 1", choices = c("",reactive_objects$params_1), selected = "")
#	})
#	
#	# Parameter 2 selection based on parameter 1
#	output$sel_param2 <- renderUI({
#		params_2 = reactive_objects$params_1[reactive_objects$params_1!=input$sel_param1]
#		selectInput("sel_param2", "Select Parameter 2", choices = c("",params_2), selected = "")
#	})
#	
#	# Plot
#	output$compare_params <- renderPlotly({
#	req(input$sel_param1)
#	data = reactive_objects$sel_data
#	plotdata = data[data$IR_MLID==input$sel_param_site&data$R3172ParameterName%in%c(input$sel_param1, input$sel_param2),]
#	plotdata = plotdata[order(plotdata$ActivityStartDate),]
#	param1 = plotdata[plotdata$R3172ParameterName==input$sel_param1,]
#	param2 = plotdata[plotdata$R3172ParameterName==input$sel_param2,]
#	
#	
#	##Spatial Relationships between site/params
#	output$sel_maparameter <- renderUI({
#	selectInput("sel_maparameter","Select Parameter", choices = c("",unique(reactive_objects$sel_data$R3172ParameterName)), selected = "")
#	})
#	
#	output$sel_paramdate <- renderUI({
#	dates = as.character(unique(reactive_objects$sel_data$ActivityStartDate))
#	dates = as.Date(dates, "%Y-%m-%d")
#	sliderInput("sel_paramdate", "Select Date Range", min = min(dates), max = max(dates), value = c(min(dates),max(dates)))
#	})
#	

