\name{effective_sites}
\alias{effective_sites}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate the effective sample-years of data (equivalent number of independent data) 
}
\description{
This function provides a statistical computation of the number of statistical effective sample years of data.  
}
\usage{
effective_sites(ams, distmat, ts, hw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ams}{A matrix or data frame containing the AMS values. See \code{ams_stat_all}. 
}
  \item{distmat}{A matrix or dataframe with mutual catchemnts distances. See function \code{coordinates} 
}
  \item{ts}{A vactor of lenght 2 containing the optimized parameters for Tasker and Stedinger cross-corraltion formula. See function \code{ts_model2}.
}
  \item{hw}{A vector of length 3 containing the Hosking and Wallis omogeinity test results. See function \code{hw_region}. 
}
}
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
####################
data(list=c("region_all","ams_stat_all"))
PREC<-list()

################## Cross-corralation distances matrix
PREC$intermed$distmat <- coordinates(region_all)
PREC$intermed <- correl(ams_stat_all)
PREC$intermed <- crosscorr_vectors(PREC)

################# Optimization of T&S cross-correlaltion formula
PREC$intermed$ts_par<-optim(c(1,1),ts_model2)

################# H&W homogeinity test
PREC$result$hw <- hw_region(PREC$input_data)

################# computation of effective sites and recurrence interval

PREC$result$effective_observations <- effective_sites(ams_stat_all, 
PREC$intermed$distmat, PREC$intermed$ts_par$par, PREC$result$hw)
}
