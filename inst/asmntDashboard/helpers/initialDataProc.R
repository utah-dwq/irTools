initialDataProc=function(site_use_param_asmnt){

#load("C:/Users/jvander/Documents/R/irTools/inst/extdata/asmntDashboard_data.Rdata")


# Initial data processing
## Extract pollution indicator assessments
pol_ind=subset(site_use_param_asmnt, pol_ind=='Y')

#Extract new listings for map label
new_listings = subset(site_use_param_asmnt, new_listing=='Y')

site_use_param_asmnt=subset(site_use_param_asmnt, pol_ind=='N')

## Site level rollups
site_param_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
if(dim(pol_ind)[1]>0){site_param_pol_ind=irTools::rollUp(list(pol_ind), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)}
site_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME'), cat_var="AssessCat", print=F, expand_uses=F)

## Read master site list
master_site_file=system.file("extdata", "0_master_site_file_SLCOWS_v2.xlsx", package = "irTools")
master_site=as.data.frame(readxl::read_excel(master_site_file, 'sites'))

## ID rejected site locations
rejected_sites=subset(master_site, IR_FLAG=="REJECT")

## ID accepted sites w/o assessments
na_sites=subset(master_site, IR_FLAG=="ACCEPT" & !IR_MLID %in% site_asmnt$IR_MLID)

## Join site types back to site asmnt (if not already present)
if(all(names(site_asmnt)!='MonitoringLocationTypeName')){
	site_types=unique(master_site[master_site$IR_MLID==master_site$MonitoringLocationIdentifier,c('IR_MLID','MonitoringLocationTypeName')])
	site_asmnt=merge(site_asmnt,site_types, all.x=T)
}


## Join AU types back to site asmnt (if not already present)
if(all(names(site_asmnt)!='AU_Type')){
	au_types=unique(master_site[,c('IR_MLID','AU_Type')])
	site_asmnt=merge(site_asmnt,au_types, all.x=T)
}

### Generate impaired params list
sites_ns=subset(site_param_asmnt, AssessCat=='NS' & !is.na(as.character(IR_MLID)) & IR_MLID!='NA')
if(dim(sites_ns)[1]>0){
	impaired_params=reshape2::dcast(IR_MLID~R3172ParameterName, data=sites_ns, value.var='R3172ParameterName')
	nms=names(impaired_params[2:dim(impaired_params)[2]])
	impaired_params=tidyr::unite(impaired_params, 'Impaired_params', nms, sep='; ')
	impaired_params=within(impaired_params, {
		Impaired_params=gsub('NA; ', '', Impaired_params)
		Impaired_params=gsub('NA', '', Impaired_params)
		Impaired_params=sub("; $","",Impaired_params)
	})
	head(impaired_params)
	site_asmnt=merge(site_asmnt, impaired_params, all.x=T)
}else{site_asmnt$Impaired_params=NA}

### Generate IDEX params list
sites_IDEX=subset(site_param_asmnt, AssessCat=='IDEX' & !is.na(as.character(IR_MLID)) & IR_MLID!='NA')
if(dim(sites_IDEX)[1]>0){
	IDEX_params=reshape2::dcast(IR_MLID~R3172ParameterName, data=sites_IDEX, value.var='R3172ParameterName')
	nms=names(IDEX_params[2:dim(IDEX_params)[2]])
	IDEX_params=tidyr::unite(IDEX_params, 'IDEX_params', nms, sep='; ')
	IDEX_params=within(IDEX_params, {
		IDEX_params=gsub('NA; ', '', IDEX_params)
		IDEX_params=gsub('NA', '', IDEX_params)
		IDEX_params=sub("; $","",IDEX_params)
	})
	head(IDEX_params)
	site_asmnt=merge(site_asmnt, IDEX_params, all.x=T)
}else{site_asmnt$IDEX_params=NA}


### Generate pollution indicator NS list
if(dim(pol_ind)[1]>0){sites_pi=subset(site_param_pol_ind, AssessCat=='NS' & !is.na(as.character(IR_MLID)) & IR_MLID!='NA')}
if(dim(sites_pi)[1]>0){
	pi_params=reshape2::dcast(IR_MLID~R3172ParameterName, data=sites_pi, value.var='R3172ParameterName')
	nms=names(pi_params[2:dim(pi_params)[2]])
	pi_params=tidyr::unite(pi_params, 'pi_params', nms, sep='; ')
	pi_params=within(pi_params, {
		pi_params=gsub('NA; ', '', pi_params)
		pi_params=gsub('NA', '', pi_params)
		pi_params=sub("; $","",pi_params)
	})
	head(pi_params)
	site_asmnt=merge(site_asmnt, pi_params, all.x=T)
}else{site_asmnt$pi_params=NA}


## AU level rollups
au_param_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
au_param_pol_ind=irTools::rollUp(list(pol_ind), group_vars=c('ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
au_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('ASSESS_ID','AU_NAME'), cat_var="AssessCat", print=F, expand_uses=F)

# Helper function to summarize parameters (replaces many lines of repeated code)
summarize_params <- function(data, category, group_vars, new_col_name) {
  data %>%
    filter(AssessCat == {{category}}) %>%
    group_by(across(all_of(group_vars))) %>%
    dplyr::summarize({{new_col_name}} := paste(unique(R3172ParameterName), collapse = "; "), .groups = "drop")}

# Generate all parameter lists using the helper function
### Generate impaired params list
### Generate IDEX params list
### Generate Pollution Indicator params list
au_impaired_params <- summarize_params(au_param_asmnt, "NS", c("ASSESS_ID"), "Impaired_params")
au_idex_params <- summarize_params(au_param_asmnt, "IDEX", c("ASSESS_ID"), "IDEX_params")
au_pi_params <- summarize_params(pol_ind,"NS",c("ASSESS_ID"), "pi_params")

### Generate New listing param list
new_listings=new_listings%>%group_by(ASSESS_ID,AU_NAME)%>%
  dplyr::summarize(new_listings=paste(unique(R3172ParameterName),collapse="; "))

au_asmnt <- au_asmnt %>%
  left_join(au_impaired_params, by = "ASSESS_ID") %>%
  left_join(au_idex_params, by = "ASSESS_ID") %>%
  left_join(au_pi_params, by = "ASSESS_ID") %>%
  left_join(new_listings, by = "ASSESS_ID")


# *** Remove duplicate parameters from Impaired_params here ***
au_asmnt <- au_asmnt %>%
  rowwise() %>%
  mutate(
    Impaired_params = if_else(
      is.na(Impaired_params) | is.na(new_listings),
      Impaired_params,
      paste(setdiff(strsplit(Impaired_params, ";\\s*")[[1]], strsplit(new_listings, ";\\s*")[[1]]), collapse = "; ")
    )
  ) %>%
  ungroup()


# Assign colors
assignAsmntCols=function(x){
	y=within(x, {
		col=NA
		col[is.na(AssessCat)]='grey'
		col[AssessCat=='FS']='green'
		col[AssessCat=='IDNE']='yellow'
		col[AssessCat=='IDEX']='orange'
		col[AssessCat=='NS']='red'
	})
	return(y)
}

## AUs & sites
au_poly=wqTools::au_poly
bu_poly=wqTools::bu_poly
ss_poly=wqTools::ss_poly

## Outputs

au_poly=merge(au_poly, au_asmnt, all.x=T)
au_poly=assignAsmntCols(au_poly)
site_asmnt=assignAsmntCols(site_asmnt)

return(list(au_asmnt_poly=au_poly, site_asmnt=site_asmnt, rejected_sites=rejected_sites, na_sites=na_sites, master_site=master_site))

}

