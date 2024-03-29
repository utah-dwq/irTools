% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateMethodsTable.r
\name{updateMethodTrans}
\alias{updateMethodTrans}
\title{Update paramTransTable, paramFractionGroup, & WQPParamCASID tables in translation workbook}
\usage{
updateMethodTrans(
  data,
  translation_wb,
  methodsTransTable_sheetname = "paramMethodsTable"
)
}
\arguments{
\item{data}{A WQP results (merged or narrow result) R-object}

\item{translation_wb}{Full path and filename for IR translation workbook}

\item{methodsTransTable_sheetname}{Name of sheet in workbook holding parameter translation table. Defaults to "paramTransTable".}
}
\value{
Appends any new parameter/fraction/unit combinations in WQP data to translation_wb. This updates the input translation_wb with those new rows with system date in the "DateAdded" column.
}
\description{
This function identifies parameter/fraction/unit/method combinations in WQP data for which an IR translation has not yet been defined and appends them to the translation table for review.
}
