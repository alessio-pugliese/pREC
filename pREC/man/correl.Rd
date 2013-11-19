\name{correl}
\alias{correl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correl(xsdata, interm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsdata}{
%%     ~~Describe \code{xsdata} here~~
}
  \item{interm}{
%%     ~~Describe \code{interm} here~~
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
function (xsdata, interm) 
{
    ams_stat_all <- xsdata
    sites_no <- NCOL(ams_stat_all)
    correlmatrix <- matrix(NA, sites_no, sites_no)
    novermatrix <- correlmatrix
    for (j1 in 1:(sites_no - 1)) {
        for (j2 in (j1 + 1):sites_no) {
            ams1 = ams_stat_all[, j1]
            ams2 = ams_stat_all[, j2]
            ams1_NA = !is.na(ams1)
            ams1_1 <- ams1[ams1_NA]
            ams2_1 <- ams2[ams1_NA]
            ams2_NA = !is.na(ams2_1)
            ams1_2 <- ams1_1[ams2_NA]
            ams2_2 <- ams2_1[ams2_NA]
            correlmatrix[j1, j2] = round(cor(ams1_2, ams2_2), 
                4)
            novermatrix[j1, j2] = length(ams1_2)
        }
    }
    interm$correlmat <- correlmatrix
    interm$novermat <- novermatrix
    return(interm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
