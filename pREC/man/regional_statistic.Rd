\name{regional_statistic}
\alias{regional_statistic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regional statistics
}
\description{
This function outputs some useful summary regional statistics
}
\usage{
regional_statistic(PREC_input_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PREC_input_data}{Must be a list containing \code{ams_stat_al} and \code{region_all}
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
data(c(region_all,ams_stat_all))

PREC$input_data$region <- region_all
PREC$input_data$ams <- ams_stat_all
PREC$result$statistic <- regional_statistic(PREC$input_data)

}

