\name{correl}
\alias{correl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix of empirical correlation coefficients
}
\description{
Compute the correlation matrix between AMS values discarding eventually \code{NA} values (i.e. missing records). This function makes use of \code{cor} as its main function.   
}
\usage{
correl(xsdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsdata}{The input AMS dataframe. See \code{ams_stat_all}. 
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
################################
data(ams_stat_all)
PREC<-list()
PREC$intermed<-correl(ams_stat_all)
}
