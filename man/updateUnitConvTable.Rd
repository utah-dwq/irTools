% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateUnitConvTable.r
\name{updateUnitConvTable}
\alias{updateUnitConvTable}
\title{Update IR unit conversion table}
\usage{
updateUnitConvTable(
  data,
  translation_wb,
  sheetname = "unitConvTable",
  startRow = 1,
  startCol = 1
)
}
\arguments{
\item{data}{A merged WQP results-activity R-object that has had detection limit values filled (passed through fillMaskedValues function) and numeric criteria assigned (passed through assignCriteria function).}

\item{translation_wb}{Full path and filename for IR translation workbook (.xlsx).}

\item{sheetname}{Name of sheet in workbook holding IR unit conversion table. Defaults to "unitConvTable".}

\item{startRow}{Row to start reading the unit conversion excel sheet from (in case headers have been added). Defaults to 1.}

\item{startCol}{Column to start reading the unit conversion excel sheet from (in case columns have been added). Defaults to 1.}
}
\value{
Updates unit conversion table in translation_wb with any new combinations of native and target units.
}
\description{
Updates IR unit conversion table based on combinations of native and target units as defined by numeric criteria tables.
}
