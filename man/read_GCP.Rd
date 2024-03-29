% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_GCP.R
\name{read_GCP}
\alias{read_GCP}
\title{Read GCP}
\usage{
read_GCP(
  data_file,
  sheet = "Global Carbon Budget",
  skip_rows = 20,
  nyears_GCP = 61,
  land_sink_col_name = "land sink",
  luc_col_name = "land-use change emissions",
  imbalance_col_name = "budget imbalance"
)
}
\arguments{
\item{data_file}{Optional, a character string defining the benchmarking data to be used.  If not supplied the file internal
to the package will be used}

\item{sheet}{Optional, a character string giving the name of the shet to be read}

\item{skip_rows}{Optional, integer, the number of rows on the sheet to skip to get to the data}

\item{nyears_GCP}{Optional, integer, the number if years (rows) of data to read}

\item{land_sink_col_name}{Optional, a character string giving the column name for the land sink}

\item{luc_col_name}{Optional, a character string giving the column name for the land use change column}

\item{imbalance_col_name}{Optional, a character string giving the column name for the budget imbalance column}
}
\value{
A DGVMTools::Field object containing the GCP data
}
\description{
Read the Global Carbon Project's Global Carbon Budget.  The default are for the GCB 2020 Excel sheet.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
