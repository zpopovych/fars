% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS data for multiple years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{numeric vector that specifies the years to summarise the data}
}
\value{
This function returns a list of data frames

 @section Depends on:
\enumerate{
  \item \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{group_by}} and \code{\link[dplyr]{summarize}} in the
  \code{dplyr} package.
  \item \code{\link[tidyr]{spread}} in the \code{tidyr} package.
}
}
\description{
This is a function that reads and then summarises FARS data for one or more years
specified by \code{years} argument and returnes summary data frame.
}
\details{
Summary table will be received
}
\examples{
\dontrun{
fars_summarize_years(c(2013, 2014, 2015))
fars_summarize_years(c(2013:2015))
fars_summarize_years(list(2013, 2014, 2015))
}

}
