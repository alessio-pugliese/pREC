\name{pREC-package}
\alias{pREC-package}
\alias{pREC}
\docType{package}
\title{Probabilistic Regional Envelope Curve}
\description{
This package aims to compute the probabilistic regional envelope curve of extremes regional values with an assigned return period (i.e. non exceedence probability). 
}
\details{
\tabular{ll}{
Package: \tab pREC\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-11-15\cr
License: \tab GPLv3\cr
}
}
\author{
Attilio Castellarin, Bjorn Guse and Alessio Pugliese

Maintainer: Alessio Pugliese <alessio.pugliese3@unibo.it>

}
\references{

}

\keyword{nsRFA}
\examples{
############
############
library(nsRFA)

### output Initialization 
PREC <- list()

## Loading of input parameters
data(list=c("ams_stat_all","region_all"))

# Arrange matrixes input data

PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all

# statistical values for the region
PREC$result$statistic <- regional_statistic(PREC$input_data)
                     
# homogeneity test
PREC$result$hw <- hw_region(PREC$input_data)

# distances between sites
PREC$intermed$distmat <- coordinates(PREC$input_data$region)

# correlation between sites
PREC$intermed <- correl(PREC$input_data$ams, PREC$intermed)

#####
PREC$intermed<-crosscorr_vectors(PREC)

# optimation of the TS model parameters
PREC$intermed$ts_par=optim(c(1,1),ts_model2)

# crosscorrelation plot
crossdist(PREC$intermed)

# slope and intercept of PREC
PREC$result$REC_par <- REC_parameters(PREC$result$statistic)

# REC discharge estimation
PREC$result$statistic <- QREC_estimates(PREC$result)

## Calculation of REC
PREC_plot(PREC$result$statistic, PREC$result$REC_par)

# Effective number of observation
PREC$result$effective_observations <- effective_sites(PREC$input_data$ams, 
      PREC$intermed$distmat, PREC$intermed$ts_par$par, PREC$result$hw)

}
