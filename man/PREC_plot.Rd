\name{PREC_plot}
\alias{PREC_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Regional Envelope Curve
}
\description{
This function allows to plot the regional envelope curve 
}
\usage{
PREC_plot(region_data, abpar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{region_data}{The output of function \code{QREC_estimates}. A list containing regional statistics catchment characteristics. 
  
}
  \item{abpar}{The slope and intercept of PREC; is the output of function \code{REC_parameters}. 
%%     ~~Describe \code{abpar} here~~
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

############################## plot PREC

par(mfrow=c(1,1),mar=c(6,7,4,2), las=1, xaxp=c(0,4,1), yaxp=c(0,5,1), mgp=c(4.5,1,0),
    cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.5)
PREC_plot(PREC$result$statistic, PREC$result$REC_par)

}
