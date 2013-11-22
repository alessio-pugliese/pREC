\name{QREC_estimates}
\alias{QREC_estimates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Regional Envelope Curve estimates}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QREC_estimates(RECres)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RECres}{A list containing the output of both \code{REC_parameters} and \code{regional_statistics} functions. See example below.}
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
###################################
data(list=c("region_all","ams_stat_all"))

PREC<-list()
PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all
PREC$result$statistic <- regional_statistic(PREC$input_data)
############################# compute linear prameters

PREC$result$REC_par <- REC_parameters(PREC$result$statistic)

############################# estimates REC
PREC$result$statistic <- QREC_estimates(PREC$result)


}
