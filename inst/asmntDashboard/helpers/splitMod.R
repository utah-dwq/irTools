
# AU split module

splitModUI <-function(id){
	ns <- NS(id)
	tagList(	
		shinycssloaders::withSpinner(editModUI(ns("auSplit"), height='800px', width='800px'),size=2, color="#0080b7")
	)
}

splitMod <- function(input, output, session, selected_aus, au_asmnt_poly, site_asmnt, na_sites, rejected_sites){
	reactive_objects=reactiveValues()
	
	# Get data & format
	observe({
		req(req(selected_aus(), au_asmnt_poly(), site_asmnt(), na_sites(), rejected_sites())
		reactive_objects$selected_aus = selected_aus()
		reactive_objects$au_asmnt_poly = au_asmnt_poly()
		reactive_objects$site_asmnt = site_asmnt()
		reactive_objects$na_sites = na_sites()
		reactive_objects$rejected_sites = rejected_sites()
	})

	print(selected_aus)
	print(au_asmnt_poly)
	
	au_asmnt_poly=au_asmnt_poly[au_asmnt_poly$ASSESS_ID %in% selected_aus,]
	view=sf::st_bbox(au_asmnt_poly)
	site_asmnt=subset(site_asmnt, IR_MLID %in% sel_sites)
	sel_aus_map=asmntMap(au_asmnt_poly, site_asmnt, na_sites, rejected_sites) %>%
		fitBounds(paste(view[1]),paste(view[2]),paste(view[3]),paste(view[4])) %>%
		showGroup('Assessed sites')
    
	splits<-callModule(editMod, "auSplit", sel_aus_map, targetLayerId='split_shapes')
    
	observe({
		req(splits()$finished)
		print(splits()$finished)
	})
	
	#return(splits()$finished)
}




