% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TangledFeatures.R
\name{DataCleaning}
\alias{DataCleaning}
\title{Automatic Data Cleaning}
\usage{
DataCleaning(Data, Y_var)
}
\arguments{
\item{Data}{The imported Data Frame}

\item{Y_var}{The X variable}
}
\value{
The cleaned data.
}
\description{
Automatic Data Cleaning
}
\examples{
DataCleaning(Data = TangledFeatures::Housing_Prices_dataset,  Y_var = 'SalePrice')
}
