\name{coordinates}
\alias{coordinates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix of distances
}
\description{
Creates a matrix between mutual catchment distances (in meters). The element \code{d{i,j}} in the matrix represents the distance between catchment \code{i} and \code{j}.
}
\usage{
coordinates(coord_region) 
}

\arguments{
  \item{coord_region}{a dataframe or matrix with catchments coordinates of the centroid, dim \code{Nx2}, where \code{N} is the number of stations. 
}
}
\details{
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
#### Load data
data(region_all)

PREC$input_data$region<-coordinates(region_all)
}
