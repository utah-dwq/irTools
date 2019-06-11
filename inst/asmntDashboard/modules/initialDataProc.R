initialDataProc=function(site_use_param_asmnt){

#site_use_param_asmnt=read.csv("C:\\Users\\jvander\\Desktop\\site-use-param-asmnt - Copy.csv")

# Initial data processing
## Site level rollups
site_param_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
site_asmnt=irTools::rollUp(list(site_use_param_asmnt), group_vars=c('IR_MLID','IR_MLNAME','IR_Lat','IR_Long','ASSESS_ID','AU_NAME'), cat_var="AssessCat", print=F, expand_uses=F)

## Read master site list
master_site=as.data.frame(readxl::read_excel('data/master-site-reviews-2019-05-03.xlsx', 'sites'))

## ID rejected site locations
rejected_sites=subset(master_site, IR_FLAG=="REJECT")

## ID accepted sites w/o assessments
na_sites=subset(master_site, IR_FLAG=="ACCEPT" & !IR_MLID %in% site_asmnt$IR_MLID)

## Join site types back to site asmnt
site_types=master_site[,c('IR_MLID','MonitoringLocationTypeName')]
site_asmnt=merge(site_asmnt,site_types, all.x=T)


### Generate impaired params wide list
sites_ns=subset(site_param_asmnt, AssessCat=='NS')
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

### Generate idE params wide list
sites_idE=subset(site_param_asmnt, AssessCat=='idE')
if(dim(sites_idE)[1]>0){
	idE_params=reshape2::dcast(IR_MLID~R3172ParameterName, data=sites_idE, value.var='R3172ParameterName')
	nms=names(idE_params[2:dim(idE_params)[2]])
	idE_params=tidyr::unite(idE_params, 'idE_params', nms, sep='; ')
	idE_params=within(idE_params, {
		idE_params=gsub('NA; ', '', idE_params)
		idE_params=gsub('NA', '', idE_params)
		idE_params=sub("; $","",idE_params)
	})
	head(idE_params)
	site_asmnt=merge(site_asmnt, idE_params, all.x=T)
}else{site_asmnt$idE_params=NA}


## AU level rollups
au_param_asmnt=rollUp(list(site_use_param_asmnt), group_vars=c('ASSESS_ID','AU_NAME','R3172ParameterName'), cat_var="AssessCat", print=F, expand_uses=F)
au_asmnt=rollUp(list(site_use_param_asmnt), group_vars=c('ASSESS_ID','AU_NAME'), cat_var="AssessCat", print=F, expand_uses=F)

### Generate impaired params wide list
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

### Generate idE params wide list
aus_idE=subset(au_param_asmnt, AssessCat=='idE')
if(dim(aus_idE)[1]>0){
	idE_params=reshape2::dcast(ASSESS_ID~R3172ParameterName, data=aus_idE, value.var='R3172ParameterName')
	nms=names(idE_params[2:dim(idE_params)[2]])
	idE_params=tidyr::unite(idE_params, 'idE_params', nms, sep='; ')
	idE_params=within(idE_params, {
		idE_params=gsub('NA; ', '', idE_params)
		idE_params=gsub('NA', '', idE_params)
		idE_params=sub("; $","",idE_params)
	})
	head(idE_params)
	au_asmnt=merge(au_asmnt, idE_params, all.x=T)
}else{au_asmnt$idE_params=NA}

# Assign colors
assignAsmntCols=function(x){
	y=within(x, {
		col=NA
		col[is.na(AssessCat)]='grey'
		col[AssessCat=='FS']='green'
		col[AssessCat=='idNE']='yellow'
		col[AssessCat=='idE']='orange'
		col[AssessCat=='NS']='red'
	})
	return(y)
}

## AUs & sites
data("au_poly")
data("bu_poly")
data("ss_poly")

## Outputs

au_poly=merge(au_poly, au_asmnt, all.x=T)
au_poly=assignAsmntCols(au_poly)
site_asmnt=assignAsmntCols(site_asmnt)

return(list(au_asmnt_poly=au_poly, site_asmnt=site_asmnt, rejected_sites=rejected_sites, na_sites=na_sites, master_site=master_site))

}

