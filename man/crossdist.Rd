\name{crossdist}
\alias{crossdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot intersite-correlation: empirical coefficients against fitted model 
}
\description{
This function plot coefficient of correlation (blue dots) agaist the spatial distance and the Tasker and Stendiger correlation function (dashed red line), using the parameters obtained with optimization of \code{ts_model2} function.   
}
\usage{
crossdist(intermed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{intermed}{A list containing the output of functions \code{coordinates, correl, crosscorr_vectors, ts_model2} 
%%     ~~Describe \code{cdresult} here~~
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
data=(list=c("ams_stat_all","region_all"))
PREC=list()

############## Creating matrixes
PREC$intermed$distmat <- coordinates(region_all)
PREC$intermed <- correl(ams_stat_all)
PREC$intermed <- crosscorr_vectors(PREC)

############# Tasker and Stedinger correlation formula parameters optimizations

PREC$intermed$ts_par<-optim(c(1,1),ts_model2)

############# Plot correlation and T&S function

crossdist(PREC$intermed)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
