initialDataProc=function(site_use_param_asmnt){

#load("C:/Users/jvander/Documents/R/irTools/inst/extdata/asmntDashboard_data.Rdata")


# Initial data processing
## Extract pollution indicator assessments
pol_ind=subset(site_use_param_asmnt, pol_ind=='Y')
site_use_param_asmnt=subset(site_use_param_asmnt, pol_ind=='N')

## Site level rollups
site_param_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
if(dim(pol_ind)[1]>0){site_param_pol_ind=irTools::rollUp(list(pol_ind), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)}
site_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME'), cat_var="AssessCat", print=F, expand_uses=F)

## Read master site list
master_site_file=system.file("extdata", "master_site_file_08292019_2020IR_final.xlsx", package = "irTools")
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

### Generate impaired params wIDEX list
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

### Generate IDEX params wIDEX list
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


### Generate pollution indicator NS wIDEX list
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

### Generate impaired params wIDEX list
aus_ns=subset(au_param_asmnt, AssessCat=='NS')
if(dim(aus_ns)[1]>0){
	impaired_params=reshape2::dcast(ASSESS_ID~R3172ParameterName, data=aus_ns, value.var='R3172ParameterName')
	nms=names(impaired_params[2:dim(impaired_params)[2]])
	impaired_params=tidyr::unite(impaired_params, 'Impaired_params', nms, sep='; ')
	impaired_params=within(impaired_params, {
		Impaired_params=gsub('NA; ', '', Impaired_params)
		Impaired_params=gsub('NA', '', Impaired_params)
		Impaired_params=sub("; $","",Impaired_params)
	})
	head(impaired_params)
	au_asmnt=merge(au_asmnt, impaired_params, all.x=T)
}else{au_asmnt$Impaired_params=NA}

### Generate IDEX params wIDEX list
aus_IDEX=subset(au_param_asmnt, AssessCat=='IDEX')
if(dim(aus_IDEX)[1]>0){
	IDEX_params=reshape2::dcast(ASSESS_ID~R3172ParameterName, data=aus_IDEX, value.var='R3172ParameterName')
	nms=names(IDEX_params[2:dim(IDEX_params)[2]])
	IDEX_params=tidyr::unite(IDEX_params, 'IDEX_params', nms, sep='; ')
	IDEX_params=within(IDEX_params, {
		IDEX_params=gsub('NA; ', '', IDEX_params)
		IDEX_params=gsub('NA', '', IDEX_params)
		IDEX_params=sub("; $","",IDEX_params)
	})
	head(IDEX_params)
	au_asmnt=merge(au_asmnt, IDEX_params, all.x=T)
}else{au_asmnt$IDEX_params=NA}


### Generate IDEX params wIDEX list
if(dim(pol_ind)[1]>0){aus_pi=subset(au_param_pol_ind, AssessCat=='NS')}
if(dim(aus_pi)[1]>0){
	pi_params=reshape2::dcast(ASSESS_ID~R3172ParameterName, data=aus_pi, value.var='R3172ParameterName')
	nms=names(pi_params[2:dim(pi_params)[2]])
	pi_params=tidyr::unite(pi_params, 'pi_params', nms, sep='; ')
	pi_params=within(pi_params, {
		pi_params=gsub('NA; ', '', pi_params)
		pi_params=gsub('NA', '', pi_params)
		pi_params=sub("; $","",pi_params)
	})
	head(pi_params)
	au_asmnt=merge(au_asmnt, pi_params, all.x=T)
}else{au_asmnt$pi_params=NA}


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

