% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-ELPA21.R
\name{helper.cleandata}
\alias{helper.cleandata}
\title{Function to clean elpa dataset}
\usage{
helper.cleandata(ds)
}
\arguments{
\item{ds}{data frame that is constructed to the expected format}
}
\value{
Data frame
}
\description{
Performs several standardized housekeeping tasks to a 'standard' ELPA dataset.
}
\details{
Fixes school names in the *School* variable to remove non alpha characters, maps *Enrolled.Grade*
so that it's always a two digit value, maps common values in *Ethnicity* to a standard set,
and sets *Performance.Level*,*Enrolled.Grade*,*Ethnicity* to be factors.
}
