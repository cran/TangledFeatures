% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TangledFeatures.R
\name{GeneralCor}
\alias{GeneralCor}
\title{Generalized Correlation function}
\usage{
GeneralCor(df, cor1 = "pearson", cor2 = "polychoric", cor3 = "spearman")
}
\arguments{
\item{df}{The imported Data Frame}

\item{cor1}{The correlation metric between two continuous features. Defaults to pearson}

\item{cor2}{The correlation metric between one categorical feature and one cont feature. Defaults to biserial}

\item{cor3}{The correlation metric between two categorical features. Defaults to Cramers-V}
}
\value{
Returns a correlation matrix containing the correlation values between the features
}
\description{
Generalized Correlation function
}
\examples{
GeneralCor(df = TangledFeatures::Advertisement)
}
