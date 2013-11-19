\name{QREC_estimates}
\alias{QREC_estimates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QREC_estimates(RECres)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RECres}{
%%     ~~Describe \code{RECres} here~~
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
function (RECres) 
{
    area <- RECres$statistic[, 1]
    area_log <- RECres$statistic[, 6]
    a <- RECres$REC_par[1]
    b <- RECres$REC_par[2]
    min_area <- min(area)
    max_area <- max(area)
    min_Q_I <- min(a + b * min_area)
    max_Q_I <- max(a + b * max_area)
    pI <- c(10^min_Q_I, 10^max_Q_I)
    q_rec_log <- a + b * area_log
    q_rec <- 10^(q_rec_log) * area/100
    qrecs <- cbind(q_rec_log, q_rec)
    colrec <- c("REC discharge log", "REC discharge")
    colnames(qrecs) <- colrec
    RECres$statistic <- cbind(RECres$statistic, qrecs)
    return(RECres$statistic)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
