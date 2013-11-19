\name{REC_parameters}
\alias{REC_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
REC_parameters(regiondata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regiondata}{
%%     ~~Describe \code{regiondata} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (regiondata) 
{
    qmean_area_log <- regiondata[, 7]
    area_log <- regiondata[, 6]
    qmax_area_log <- regiondata[, 8]
    slope <- lm(qmean_area_log ~ area_log)
    b <- slope$coefficients[2]
    a_b <- slope$coefficients[1]
    intercep <- qmax_area_log - b * area_log
    a <- max(intercep)
    recpar <- c(a, b, a_b)
    return(recpar)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
