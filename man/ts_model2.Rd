\name{ts_model2}
\alias{ts_model2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimization of Tasker and Stedinger (1989) model 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ts_model2(lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{
  parameters vector for Tasker and Stedinger correlation formula. Need to be of length 2.  
  }
}
\details{

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
######
data=(list=c("ams_stat_all","region_all"))
PREC=list()

############## Creating matrixes
PREC$intermed$distmat <- coordinates(region_all)
PREC$intermed <- correl(ams_stat_all)
PREC$intermed <- crosscorr_vectors(PREC)

############# Tasker and Stedinger correlation formula parameters optimizations

PREC$intermed$ts_par<-optim(c(1,1),ts_model2)
}
