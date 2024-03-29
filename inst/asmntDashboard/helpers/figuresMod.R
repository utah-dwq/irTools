# Figures module

figuresModUI <- function(id){
	ns <- NS(id)
	tagList(	
	
		tags$style(type = "text/css", "html, body {width:100%;height:100%}",
			".leaflet .legend i{
			border-radius: 50%;
			width: 10px;
			height: 10px;
			margin-top: 4px;
			}"
		),
		
		fluidRow(
			column(2,uiOutput(ns('sel_param1')), uiOutput(ns('sel_units1')), uiOutput(ns('sel_frac1'))),
			conditionalPanel(paste0("input['", ns("tabs"),"'] == 'Multiple parameters'"),
				column(2,
					uiOutput(ns('sel_param2')),
					uiOutput(ns('sel_units2')),
					uiOutput(ns('sel_frac2'))
				),
				column(3,
					uiOutput(ns('sel_site'))
				)
			)
		),
		tabsetPanel(id=ns('tabs'),
			tabPanel('Multiple sites',
				fluidRow(column(4,radioButtons(ns("compare_plottype"), "Plot type", choices = c("Time series","Boxplot", "Concentration map"), selected = "Time series", inline = TRUE))),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Time series'"), plotlyOutput(ns('multi_site_ts'), height='600px')),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Boxplot'"), plotlyOutput(ns('multi_site_bp'), height='600px')),
				conditionalPanel(paste0("input['", ns("compare_plottype"),"'] == 'Concentration map'"), shinycssloaders::withSpinner(leafletOutput(ns('conc_map'), height='600px', width="100%"),size=2, color="#0080b7"))
			),
			tabPanel("Multiple parameters", 
				plotlyOutput(ns("multi_param_ts"))
			)
		)
	)
}


figuresMod <- function(input, output, session, sel_data, sel_crit){
	
	
	# Empty reactive objects
	reactive_objects=reactiveValues()

	# Get data & format
	observe({
		req(sel_data(), sel_crit())
		sel_data=sel_data()
		sel_crit=sel_crit()
		sel_data$ActivityStartDate=as.Date(sel_data$ActivityStartDate)
		sel_crit$ActivityStartDate=as.Date(sel_crit$ActivityStartDate)
		sel_crit$NumericCriterion=as.numeric(sel_crit$NumericCriterion)
		sel_data$IR_Unit=toupper(sel_data$IR_Unit)
		sel_crit$CriterionUnits=toupper(sel_crit$CriterionUnits)
		reactive_objects$sel_data=sel_data
		reactive_objects$sel_crit=sel_crit
	})
	
	# Select param 1
	output$sel_param1 <- renderUI({
		ns <- session$ns
		req(reactive_objects$sel_data$R3172ParameterName)
		selectInput(ns("sel_param1"),"Select parameter 1", choices = unique(reactive_objects$sel_data$R3172ParameterName[order(reactive_objects$sel_data$R3172ParameterName)]))
	})
	
	observe({
		req(input$sel_param1)
		reactive_objects$param1_sub=reactive_objects$sel_data[reactive_objects$sel_data$R3172ParameterName == input$sel_param1,]
	})
	
	observe({
		req(reactive_objects$param1_sub)
		reactive_objects$units1=unique(reactive_objects$param1_sub$IR_Unit)
		reactive_objects$fractions1=unique(reactive_objects$param1_sub$IR_Fraction)
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		req(reactive_objects$units1)
		ns <- session$ns
		units=reactive_objects$units1
		selectInput(ns("sel_units1"),"Select units 1", choices = units, selected="")
	})
	
    
	observe({
		req(reactive_objects$sel_crit, reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_units1'), selected="")
	})
    
	# Select fraction 1
	output$sel_frac1 <- renderUI({
		req(reactive_objects$fractions1)
		ns <- session$ns
		selectInput(ns("sel_frac1"),"Select fraction 1", choices = reactive_objects$fractions1, selected="")
	})
    
	observe({
		req(reactive_objects$sel_crit, reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_frac1'), selected="")
	})
    
	
	# Select param 2
	output$sel_param2 <- renderUI({
		ns <- session$ns
		param_choices=reactive_objects$sel_data$R3172ParameterName[! reactive_objects$sel_data$R3172ParameterName %in% input$sel_param1]
		selectInput(ns("sel_param2"),"Select parameter 2", choices = param_choices[order(param_choices)])
	})
    
	# Select units 2
	output$sel_units2 <- renderUI({
		ns <- session$ns
		units=unique(reactive_objects$sel_data[reactive_objects$sel_data$R3172ParameterName == input$sel_param2, 'IR_Unit'])
		selectInput(ns("sel_units2"),"Select units 2", choices = units)
	})

	# Select fraction 2
	output$sel_frac2 <- renderUI({
		ns <- session$ns
		fraction=unique(reactive_objects$sel_data[reactive_objects$sel_data$R3172ParameterName == input$sel_param2, 'IR_Fraction'])
		selectInput(ns("sel_frac2"),"Select fraction 2", choices = fraction)
	})

	observe({
		req(reactive_objects$sel_crit, reactive_objects$sel_data)
		ns <- session$ns
		updateSelectInput(session, ns('sel_frac2'), selected="")
	})
	
	# Generate parameter 1 data & criteria
	observe({
		req(input$sel_param1, input$sel_units1, input$sel_frac1)
			## Data
			param1=subset(reactive_objects$sel_data, R3172ParameterName == input$sel_param1 & IR_Fraction==input$sel_frac1)
			if(dim(param1)[1]>0){
				param1$target_unit=input$sel_units1
				param1=wqTools::convertUnits(param1, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
				param1=param1[order(param1$ActivityStartDate),]
				reactive_objects$param1=unique(param1[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
				
				## Criteria
				crit1=subset(reactive_objects$sel_crit, R3172ParameterName == input$sel_param1)
				if(dim(crit1)[1]>0){
					crit1$target_unit=input$sel_units1
					crit1=wqTools::convertUnits(crit1, input_units='CriterionUnits', target_units = "target_unit", value_var='NumericCriterion', conv_val_col='plot_value')
				}
				crit1=crit1[order(crit1$ActivityStartDate),]
				reactive_objects$crit1<-crit1
			}
	})
	
    
	# Generate parameter 2 data & criteria (need to do criteria still)
	observe({if(input$tabs=='Multiple parameters'){
		req(input$sel_param2, input$sel_units2, input$sel_frac2)
		param2=subset(reactive_objects$sel_data, R3172ParameterName == input$sel_param2 & IR_Fraction==input$sel_frac2)
		if(dim(param2)[1]>0){
			param2$target_unit=input$sel_units2
			param2=wqTools::convertUnits(param2, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
			param2=param2[order(param2$ActivityStartDate),]
			reactive_objects$param2=unique(param2[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
		}
	}})
	
	# Multi-site figure labels & visibilities
	observe({
		req(reactive_objects$param1, reactive_objects$crit1)
		reactive_objects$title = input$sel_param1
		reactive_objects$ylab = paste0(input$sel_param1,' (', input$sel_units1,')')
		reactive_objects$ylab2 = paste0(input$sel_param2,' (', input$sel_units2,')')
		mlid_len=length(unique(reactive_objects$param1$IR_MLID))
		au_len=length(unique(reactive_objects$param1$ASSESS_ID))
		if(dim(reactive_objects$crit1)[1]>0){
			crit_plot=unique(reactive_objects$crit1[,c('ActivityStartDate','BeneficialUse','label','CriterionType','plot_value')])
		}else{crit_plot=reactive_objects$crit1}
		reactive_objects$crit_plot=crit_plot
		crit_len=length(unique(crit_plot$label))
		mlid_vis=as.list(append(rep(T, mlid_len), rep(F, au_len)))
		reactive_objects$mlid_vis=as.list(append(mlid_vis, rep('legendonly',crit_len)))
		au_vis=as.list(append(rep(F,mlid_len), rep(T, au_len)))
		reactive_objects$au_vis=as.list(append(au_vis, rep('legendonly',crit_len)))	
	})
    
	# Multi-site time series
	output$multi_site_ts=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$crit1, reactive_objects$au_vis, reactive_objects$crit_plot)				
		#if(all(!is.na(reactive_objects$param1$plot_value))){
			msts=plot_ly(source="a") %>%
				add_trace(data=reactive_objects$param1, type = 'scatter', mode = 'lines+markers', x=~as.Date(ActivityStartDate), y=~plot_value, color = ~IR_MLID, marker = list(size=10), visible=T) %>%
				add_trace(data=reactive_objects$param1, type = 'scatter', mode = 'markers',x = ~as.Date(ActivityStartDate), y=~plot_value, color = ~ASSESS_ID, marker = list(size=10), visible=F)
			if(dim(reactive_objects$crit_plot)[1]>0){
				msts=add_trace(msts, data=reactive_objects$crit_plot, type = 'scatter', mode='lines', x = ~as.Date(ActivityStartDate), y=~plot_value, connectgaps=TRUE, color = ~label, visible='legendonly')
			}
			msts=layout(msts,
							title = reactive_objects$title,
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
				config(displaylogo = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'lasso2d'
					)
				)
		#}
		msts
	})
    	
	# Multi site boxplot
	output$multi_site_bp=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, reactive_objects$crit1, reactive_objects$au_vis, reactive_objects$title, reactive_objects$ylab, reactive_objects$mlid_vis, reactive_objects$au_vis)
		crit_plot=reactive_objects$crit_plot
		if(dim(crit_plot)[1]>0){
			crit_plot=unique(crit_plot[!is.na(crit_plot$plot_value),c('plot_value','label')])
			crit_plot0=crit_plot
			crit_plot0$x=0
			crit_plot1=crit_plot
			crit_plot1$x=1
			crit_plot=rbind(crit_plot0,crit_plot1)
		}
		#if(all(!is.na(reactive_objects$param1$plot_value))){
			msbp=plot_ly(data=reactive_objects$param1, type = 'box', y = ~plot_value, color = ~IR_MLID, visible=T) %>%
				add_trace(type = 'box', y = ~plot_value, color = ~ASSESS_ID, visible=F) %>%
				layout(
					title = reactive_objects$title,
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
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'select2d',
						'lasso2d'
					)
				)
			if(dim(crit_plot)[1]>0){
				msbp=add_trace(msbp, data=crit_plot, type = 'scatter', mode='lines', x=~x, y = ~plot_value, color = ~label, visible='legendonly', xaxis = "x2")
			}

		#}
		msbp
	})
    
	
	
	
	
	# Concentration map		
	conc_map = wqTools::buildMap(plot_polys=TRUE, search="")
	conc_map=conc_map%>%clearGroup('Sites') %>% clearControls()
	conc_map = leaflet::addLayersControl(conc_map,
		position ="topleft",
		baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
		options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
	conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
	conc_map=hideGroup(conc_map, "Assessment units")
	conc_map=hideGroup(conc_map, "Site-specific standards")
	conc_map=hideGroup(conc_map, "Beneficial uses")
	conc_map=removeMeasure(conc_map)
    
	output$conc_map <- leaflet::renderLeaflet({
		isolate({
			if(!is.null(reactive_objects$param1) & dim(reactive_objects$param1)[1]>0){
				sites=reactive_objects$param1
				sites=sites[!is.na(sites$plot_value),]
				count=aggregate(plot_value~IR_MLID+IR_Lat+IR_Long+target_unit, sites, FUN='length')
				names(count)[names(count)=='plot_value'] = 'count'
				sites=aggregate(plot_value~IR_MLID+IR_MLNAME+IR_Lat+IR_Long+target_unit, sites, FUN='mean', na.rm=TRUE)
				sites=merge(sites,count,all.x=T)
				sites$radius=scales::rescale(sites$plot_value, c(5,35))
				min_lat=min(sites$IR_Lat)*0.999
				min_lng=min(sites$IR_Long)*0.999
				max_lat=max(sites$IR_Lat)*1.001
				max_lng=max(sites$IR_Long)*1.001
				leg_labs=c(signif(quantile(sites$plot_value, 0.10),3), signif(median(sites$plot_value),3), signif(quantile(sites$plot_value, 0.90),3))
				leg_sizes=c(quantile(sites$radius, 0.10), median(sites$radius), quantile(sites$radius, 0.90))*2
				conc_map = conc_map %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
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

		conc_map
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
	observeEvent(reactive_objects$param1, {
		#req(reactive_objects$param1, reactive_objects$ylab)
		conc_proxy%>%clearGroup('Sites') %>% clearControls()
		if(exists('sites')){rm(sites)}
		if(all(!is.na(reactive_objects$param1$plot_value))){
			sites=reactive_objects$param1
			sites=sites[!is.na(sites$plot_value),]
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
			conc_proxy %>% flyToBounds(min_lng,min_lat,max_lng,max_lat) %>%	
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
		ns <- session$ns
		selectInput(ns("sel_site"),"Select Site", choices = param_site)
	})
    
	output$multi_param_ts=renderPlotly({
		req(reactive_objects$param1, input$sel_units1, input$sel_frac1, reactive_objects$param2, input$sel_units2, input$sel_frac2, input$sel_site)
		param1=reactive_objects$param1[reactive_objects$param1$IR_MLID %in% input$sel_site,]
		param2=reactive_objects$param2[reactive_objects$param2$IR_MLID %in% input$sel_site,]
		#if(all(!is.na(reactive_objects$param1$plot_value)) & all(!is.na(reactive_objects$param2$plot_value))){
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
				add_trace(data=param1, x = ~ActivityStartDate, y = ~plot_value, name = reactive_objects$ylab, marker = list(size = 10)) %>%
				add_trace(data=param2, x = ~ActivityStartDate, y = ~plot_value, name = reactive_objects$ylab2, marker = list(size = 10), yaxis = "y2") %>%
				config(displaylogo = FALSE, collaborate = FALSE,
					modeBarButtonsToRemove = c(
						'sendDataToCloud',
						'lasso2d',
						'select2d'
					)
				)
		#}
	})

	return(list(
		select_data=reactive({event_data("plotly_selected", source="a")}),
		param1=reactive({input$sel_param1}),
		param_choices=reactive({
				req(reactive_objects$sel_data)
				unique(reactive_objects$sel_data$R3172ParameterName[order(reactive_objects$sel_data$R3172ParameterName)])
			})
	))
	
}
