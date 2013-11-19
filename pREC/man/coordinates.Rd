\name{coordinates}
\alias{coordinates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coordinates(coord_region)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord_region}{
%%     ~~Describe \code{coord_region} here~~
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
function (coord_region) 
{
    sites_no <- NROW(coord_region)
    xcoord <- coord_region[, 4]
    ycoord <- coord_region[, 5]
    coordmatrix <- matrix(NA, sites_no, sites_no)
    for (i1 in 1:(sites_no - 1)) {
        for (i2 in (i1 + 1):sites_no) {
            coordmatrix[i1, i2] = sqrt((xcoord[i1] - xcoord[i2])^2 + 
                (ycoord[i1] - ycoord[i2])^2)/1000
        }
    }
    return(coordmatrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
