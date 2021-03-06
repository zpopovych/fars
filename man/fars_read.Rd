% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read data from FARS}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{character string}
}
\value{
This function returns a data frame.
}
\description{
This is a function that reads the annual FARS data file
for concrete year properly specified by \code{filename} argument.
Filename could be generated with \code{make_filename} function.
}
\details{
The FARS data should be R in the working directory
}
\section{Depends on}{

\enumerate{
  \item \code{\link[readr]{read_csv}} in the \code{readr} package.
  \item \code{\link[dplyr]{tbl_df}} in the \code{dplyr} package.
}
}

\examples{
\dontrun{
  fars_read("accident_2014.csv.bz2")
  fars_read(make_filename(2014))
}

}
