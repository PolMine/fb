% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facepager.R
\name{fp_read_csv}
\alias{fp_read_csv}
\title{Read Facepager CSV files}
\usage{
fp_read_csv(x, cols = fp_colnames)
}
\arguments{
\item{x}{Path of a csv file exported from Facepager. Can be a URL.}
}
\value{
A \code{tibble} with columns "User Name", "URL", "date" and "Message".
}
\description{
Read Facepager CSV files
}
