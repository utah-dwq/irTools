#' Prepares rolled up AU-Use and AU-Use-Parameter assessments for ATTAINS upload
#'
#' @param current_assessment A list of assessment output objects to be rolled up to the AU-use and (for cat 5 only) AU-use-parameter resolutions.
#' @param trans_wb Excel workbook containing translation tables connecting irTools outputs to ATTAINS inputs. Currently resides in lookup tables repo.
#' @param pull_attains Logical. If TRUE (default), function uses readAttains function from the irTools package to pull assessment data from the previous reporting cycle. If FALSE, param_path and assess_path are needed.
#' @param assess_path File path to ATTAINS assessments.csv from previous reporting cycle.
#' @param param_path File path to ATTAINS parameters.csv from previous reporting cycle.
#' @param last_reporting_cycle Year of previous IR reporting cycle.
#' @param current_reporting_cycle Year of current IR reporting cycle.
#' @param filesave Logical. If TRUE (default), function saves AU-Use-Assessments and Category 5 AU-Use-Parameter Assessments to .csvs in a folder selected by the user.
#' @return Returns a list consisting of two objects: AU-Use-Assessments for all AU's and Category 5 AU-Use-Parameter Assessments.
#' @import openxlsx
#' @import tidyr

#' @export

#### TESTING #####
# Load irTools, load data
#library(devtools)
#install_github("utah-dwq/irTools")
#setwd("~/Documents/GitHub/attainsTest")
#setwd("~/GitHub/attainsTest")
#setwd("P:\\WQ\\Integrated Report\\Automation_Development\\elise\\ATTAINS_demo")
# load("assessments-2016.Rdata")
# current_assessment = assessments
# trans_wb = "ATTAINS_domains.xlsx"
# assess_path = "exported_2016_asmnts//assessments.csv"
# param_path = "exported_2016_asmnts//parameters.csv"
# last_reporting_cycle = 2016
# current_reporting_cycle = 2018
# 
# testing = prepAttains(current_assessment = current_assessment, trans_wb = trans_wb, pull_attains = TRUE, assess_path = assess_path, param_path = param_path, last_reporting_cycle = last_reporting_cycle, current_reporting_cycle = current_reporting_cycle, filesave = TRUE)

prepAttains <- function(current_assessment, trans_wb, pull_attains = TRUE, assess_path, param_path, last_reporting_cycle, current_reporting_cycle, filesave = TRUE){
  
  out <- list()
  
  if(!pull_attains&(missing(param_path)|missing(assess_path))){
    stop("ATTAINS file path arguments required when pull_attains argument is FALSE.")
  }
  
  # Run roll up at AU-use level
  au_u=irTools::rollUp(data=current_assessment, group_vars=c("ASSESS_ID","AU_NAME", "BeneficialUse"), expand_uses=TRUE, print = FALSE)
  au_u$AssessCat[au_u$AssessCat=="NA"] = "Not Applicable"
  
  # Run roll up at AU-use-parameter level
  au_up=irTools::rollUp(data=current_assessment, group_vars=c("ASSESS_ID","AU_NAME","BeneficialUse", "R3172ParameterName"), expand_uses=FALSE, print = FALSE)
  
  # Isolate NS au/use/param combos
  au_up_cat5 = au_up[au_up$AssessCat=="NS",]
  rm(au_up)
  
  # Read in translation workbook
  #### NOTE ##### Need to update to overarching translation workbook (with combined domain tables)
  attains_trans = openxlsx::loadWorkbook(trans_wb)
  uses = openxlsx::readWorkbook(attains_trans, sheet = "uses")
  attainment = openxlsx::readWorkbook(attains_trans, sheet = "assess_use")
  params = openxlsx::readWorkbook(attains_trans, sheet = "params")
 
  ##### AU USE TRANSLATIONS 
  # Cycle last assessed
  # Bring in ATTAINS assessment csv and obtain list of AUs assessed in 2016
  if(pull_attains){
    attainspull=irTools::readAttains(type="assessments", state="UT", reportingCycle = last_reporting_cycle)
    prev.assess = attainspull$assessments
    
    # Isolate columns needed for AU updates (cycle last assessed)
    attains_asmnt = data.frame(prev.assess[[1]]["assessmentUnitIdentifier"], prev.assess[[1]]["agencyCode"], "CYCLE_LAST_ASSESSED" = last_reporting_cycle)
    colnames(attains_asmnt) = c("ASSESSMENT_UNIT_ID","AGENCY_CODE","CYCLE_LAST_ASSESSED")
    
    # Isolate list of dataframes needed for param updates
    prev.params <- prev.assess[[1]]$parameters
    names(prev.params) = prev.assess[[1]]$assessmentUnitIdentifier
    prev.params.df <- plyr::ldply(prev.params, data.frame)
    
    prev.params.flat = unnest(prev.params.df, associatedUses)
    prev.params.flat = prev.params.flat[,!names(prev.params.flat)%in%c("seasons")]
    prev.params.flat$ASSESSMENT_UNIT_ID = prev.params.flat$.id
    prev.params.flat$PARAM_NAME = prev.params.flat$parameterName
    prev.params.flat$PARAM_USE_NAME = prev.params.flat$associatedUseName
    prev.params.flat$PARAM_POLLUTANT_INDICATOR = prev.params.flat$pollutantIndicator
    prev.params.flat$PARAM_YEAR_LISTED = prev.params.flat$impairedWatersInformation.listingInformation.cycleFirstListedText
    attains_params = prev.params.flat
    }else{
    # AU's
    attains_asmnt <- read.csv(assess_path, stringsAsFactors = FALSE)
    attains_asmnt = attains_asmnt[,c("ASSESSMENT_UNIT_ID","AGENCY_CODE","CYCLE_LAST_ASSESSED")]
    attains_asmnt$CYCLE_LAST_ASSESSED=last_reporting_cycle
    
    # PARAMS
    # Bring in pre-existing ATTAINS parameters csv and obtain year param listed
    attains_params <- read.csv(param_path, stringsAsFactors = FALSE)
  }
  
  # Match assessment unit column in attains output
  au_u$ASSESSMENT_UNIT_ID = au_u$ASSESS_ID
  
  # Merge attains output to current roll up
  au_u1 = merge(au_u, attains_asmnt, all.x = TRUE)
  au_u1$CYCLE_LAST_ASSESSED[is.na(au_u1$CYCLE_LAST_ASSESSED)] = current_reporting_cycle
  
  # Merge uses to au_use
  au_u2 = merge(au_u1, uses, all.x = TRUE)
  rm(au_u1)
  # Merge use attainment code
  au_u3 = merge(au_u2, attainment, all.x=TRUE)
  rm(au_u2)
  
  au_u3$USE_AGENCY_CODE = au_u3$AGENCY_CODE
  #au_u3$PARAM_USE_NAME = au_u3$USE_NAME
  
  out$au_uses = au_u3
  
  ##### AU USE PARAM TRANSLATIONS
  # Get rid of unnecessary columns in attains output (NA's throw an error in ATTAINS)
  attains_params = attains_params[,c("ASSESSMENT_UNIT_ID", "PARAM_NAME","PARAM_USE_NAME", "PARAM_POLLUTANT_INDICATOR","PARAM_YEAR_LISTED")]
    
  au_up_cat5_1 = merge(au_up_cat5, uses, all.x = TRUE)
  au_up_cat5_1$PARAM_USE_NAME = au_up_cat5_1$USE_NAME
  au_up_cat5_2 = merge(au_up_cat5_1, params, all.x = TRUE)
  au_up_cat5_2$ASSESSMENT_UNIT_ID = au_up_cat5_2$ASSESS_ID
  
  # Merge 2016 cat5 assessment params with current au_up_cat5 to get cycle first assessed
  au_up_cat5_3 = merge(au_up_cat5_2, attains_params, all.x = TRUE)
  
  # Add info
  au_up_cat5_3$AGENCY_CODE = "S"
  au_up_cat5_3$PARAM_AGENCY_CODE = "S"
  au_up_cat5_3$PARAM_STATUS_NAME = "Cause"
  au_up_cat5_3$PARAM_ATTAINMENT_CODE = "not meeting criteria"
  au_up_cat5_3$PARAM_POLLUTANT_INDICATOR[is.na(au_up_cat5_3$PARAM_POLLUTANT_INDICATOR)] <- "Y"
  au_up_cat5_3$PARAM_YEAR_LISTED[is.na(au_up_cat5_3$PARAM_YEAR_LISTED)] <- current_reporting_cycle
  
  cat5_all = merge(au_up_cat5_3,au_u3, all.x = TRUE)
  
  out$cat5_asmnts = cat5_all
  
  if(filesave){
    spot = choose.dir(getwd(), caption = "Select folder in which to save ATTAINS input files")
    setwd(spot)
    write.csv(au_u3, "All_AU_Use_Assessments.csv", row.names = FALSE)
    write.csv(cat5_all, "Cat5_AU_Use_Parameter_Assessments.csv", row.names = FALSE)
  }
  
  return(out)
}

