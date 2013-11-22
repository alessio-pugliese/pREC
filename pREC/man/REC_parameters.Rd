\name{REC_parameters}
\alias{REC_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regional Envelope Curve parameters
}
\description{
This function computes the parameters of regional envelope curve trhough a linea regression. The function uses \code{lm} fucntion for the estimation of parameters.
}
\usage{
REC_parameters(regiondata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regiondata}{A matrix or dataframe with dimensions \code{Nx3} where \code{N} is the number of catchment in the study area and the columns header need to be ordered as follow: 1. Catchment area, 2. Unit mean value, 3. Maximum value. All values must be logartmics  
 
}}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
###################################
data(list=c("region_all","ams_stat_all"))

PREC<-list()
PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all
PREC$result$statistic <- regional_statistic(PREC$input_data)
############################# compute linear prameters

PREC$result$REC_par <- REC_parameters(PREC$result$statistic)
}
