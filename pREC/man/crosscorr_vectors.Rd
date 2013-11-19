\name{crosscorr_vectors}
\alias{crosscorr_vectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crosscorr_vectors(xsprec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsprec}{
%%     ~~Describe \code{xsprec} here~~
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
function (xsprec) 
{
    region_all <- xsprec$input_data$region
    sites_no <- NROW(region_all)
    coordmatrix <- xsprec$intermed$distmat
    correlmatrix <- xsprec$intermed$correlmat
    novermatrix <- xsprec$intermed$novermat
    corrv = as.vector(correlmatrix)
    NOT_NA_index = !is.na(corrv)
    corrv = corrv[NOT_NA_index]
    distav = as.vector(coordmatrix)
    distav = distav[NOT_NA_index]
    noverv = as.vector(novermatrix)
    noverv = noverv[NOT_NA_index]
    RWM_matrix = rbind(distav[order(distav)], corrv[order(distav)], 
        noverv[order(distav)])
    if (length(distav) > 499) 
        RWM_bin = length(distav) - 299
    else RWM_bin = round(length(distav)/20, 1)
    RWM_length = length(distav) - RWM_bin + 1
    RWM_coord = rep(0, RWM_length)
    RWM_xcorr = rep(0, RWM_length)
    for (iRWM in 1:RWM_length) {
        RWM_indices = iRWM:(iRWM + RWM_bin - 1)
        RWM_coord[iRWM] = mean(RWM_matrix[1, RWM_indices])
        RWM_xcorr[iRWM] = sum(RWM_matrix[2, RWM_indices] * RWM_matrix[3, 
            RWM_indices])/sum(RWM_matrix[3, RWM_indices])
    }
    xsprec$intermed$corrv <- corrv
    xsprec$intermed$distav <- distav
    xsprec$intermed$noverv <- noverv
    return(xsprec$intermed)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
