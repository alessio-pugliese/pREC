\name{crossdist}
\alias{crossdist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crossdist(cdresult)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdresult}{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cdresult) 
{
    corrv <- cdresult$corrv
    distav <- cdresult$distav
    noverv <- cdresult$noverv
    larger4 <- which(noverv > 4)
    corrv <- corrv[larger4]
    distav <- distav[larger4]
    xall <- cdresult$ts_par$par[1]
    xall[2] <- cdresult$ts_par$par[2]
    RWM_coord <- distav
    RWM_xcorr <- corrv
    corr_min = floor(min(corrv) * 10)/10
    dist_min <- min(distav)
    dist_min <- 0
    dist_max <- max(distav)
    dist_diff <- (dist_max - dist_min)/100
    distx <- seq(dist_min, dist_max, dist_diff)
    corrfunc = exp(-xall[1] * distx/(1 + xall[2] * distx))
    plot(RWM_coord, RWM_xcorr, pch = 16, lty = 1, lwd = 3, col = "blue", 
        xlim = c(0, max(distav)), ylim = c(-1, 1), main = "Cross-correlation function", 
        xlab = "Distance [km]", ylab = "Correlation coefficient [-]", 
        xaxs = "i", yaxs = "i", cex = 0.7)
    lines(distx, corrfunc, lty = 2, lwd = 2, col = "red", xaxs = "i", 
        yaxs = "i", cex = 0.5)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
