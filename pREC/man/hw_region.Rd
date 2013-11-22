\name{hw_region}
\alias{hw_region}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hosking and Wallis homogeneity test
}
\description{
This function computes the homogeneity test for homogeneous regions, developed by Hosking and Wallis. 
}
\usage{
hw_region(inputdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputdata}{A list containing \code{ams_stat_all} and \code{region_all}
%%     ~~Describe \code{inputdata} here~~
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
data(list=c("ams_stat_all","region_all"))
PREC=list()
PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all
PREC$result$hw <- hw_region(PREC$input_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

