\name{ams_reg_syn_02}
\alias{ams_reg_syn_02}
\docType{data}
\title{
Load high correlated AMS data
}
\description{
This data frame load in work space AMS (Annual Maximum Series) data is obtained by running a random stochastic generator (synthetic series) with the cross-correlation between pairs equal to 0.2 for 36 catchments in 92 year time-span. NA in data frame stands for year with no record. 
}
\usage{data(ams_reg_syn_02)}
\format{
  A data frame with 92 observations on the following 36 variables.
  \describe{
    \item{\code{Xi}}{a numeric vector of annual maximum stream flow}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ams_reg_syn_02)
}
\keyword{datasets}
