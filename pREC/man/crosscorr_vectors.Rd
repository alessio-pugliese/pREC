\name{crosscorr_vectors}
\alias{crosscorr_vectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creoss-correlations as vector
}
\description{
This function arranges the output of function \code{correl} in vectors
}
\usage{
crosscorr_vectors(correl_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{correl_output}{A list containing the output of function \code{correl}. Also distances matrix is needed, see \code{coordinates} function for details. 

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
######
data=(list=c("ams_stat_all","region_all"))
PREC=list()

PREC$intermed$distmat <- coordinates(region_all)
PREC$intermed <- correl(ams_stat_all)
PREC$intermed <- crosscorr_vectors(PREC)


}
