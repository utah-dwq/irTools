% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countExceedances.r
\name{countExceedances}
\alias{countExceedances}
\title{Calculate sample and exceedance counts (conventional & toxic assessments)}
\usage{
countExceedances(
  data,
  group_vars = c("IR_MLID", "IR_MLNAME", "R317Descrp", "IR_Lat", "IR_Long", "ASSESS_ID",
    "AU_NAME", "BeneficialUse", "BEN_CLASS", "R3172ParameterName", "CriterionLabel",
    "SSC_MLID", "SSC_StartMon", "SSC_EndMon", "AsmntAggFun"),
  agg_exc = FALSE,
  agg_exc_as_n = TRUE
)
}
\arguments{
\item{data}{A prepped water quality portal data object (i.e. output from dataPrep() ). Must include IR_Value and ActivityStartDate.}

\item{group_vars}{Vector of column names on which to group data when calculating sample counts and exceedances. This should not include any factors that prevent aggregation to site-scale assessments (e.g. date, time, etc.), but should include any columns that indicate a unique standard (e.g. season, acute v. chronic, etc.). See default for recommended.}

\item{agg_exc}{Logical. If FALSE (default), individual samples are compared to the criterion as with non-aggregated criteria to count exceedances. If TRUE (default) aggregate samples by AsmntAggFun prior to counting exceedances.}

\item{agg_exc_as_n}{Logical. If FALSE exceedance/support of aggregate water quality criteria (e.g. seasonal means) is indicated as 1 or 0 in ExcCount column. Only used if agg_exc==TRUE. If TRUE (default), set the ExcCount value of aggregate water quality criteria to equal the associated sample count. In this case, ExcCount==SampleCount indicates an exceedance and ExcCount==0 indicates no exceedance. This allows sample and exceedance counts for these types of criteria to pass through assessExc using the same input arguments.}
}
\value{
Returns sample and exceedance counts aggregated by grouping variables.
}
\description{
Compares water quality result values to standards to calculates sample and exceedance counts. This is geared towards conventional and toxic assessments.
}
