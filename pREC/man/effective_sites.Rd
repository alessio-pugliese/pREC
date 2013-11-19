\name{effective_sites}
\alias{effective_sites}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
effective_sites(ams_stat, distmat, xall, hw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ams_stat}{
%%     ~~Describe \code{ams_stat} here~~
}
  \item{distmat}{
%%     ~~Describe \code{distmat} here~~
}
  \item{xall}{
%%     ~~Describe \code{xall} here~~
}
  \item{hw}{
%%     ~~Describe \code{hw} here~~
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
function (ams_stat, distmat, xall, hw) 
{
    amsl <- NCOL(ams_stat)
    ams_years <- NROW(ams_stat)
    ams_stat_save <- ams_stat
    ams_stat_ones <- ams_stat
    ams_stat_fac <- ams_stat
    count_obs_year <- matrix(-1, ams_years, 1)
    for (k in 1:ams_years) {
        for (k2 in 1:amsl) {
            yobs <- ams_stat[k, k2]
            ams_stat_ones[k, k2] <- ifelse(yobs > 0, 1, 0)
        }
        count_obs_year[k] <- sum(ams_stat_ones[k, ], na.rm = T)
    }
    n_zero <- length(count_obs_year[count_obs_year == 0])
    n_sing_T <- count_obs_year == 0
    n_sing_F <- n_sing_T == FALSE
    count_obs_year2 <- count_obs_year[n_sing_F]
    count_obs_year <- count_obs_year2
    ams_stat2 <- ams_stat[n_sing_F, ]
    ams_stat <- ams_stat2
    n_sing <- length(count_obs_year[count_obs_year == 1])
    n_sing_T <- count_obs_year == 1
    n_sing_F <- n_sing_T == FALSE
    count_obs_year2 <- count_obs_year[n_sing_F]
    count_obs_year <- count_obs_year2
    ams_stat2 <- ams_stat[n_sing_F, ]
    ams_stat <- ams_stat2
    ams_years_xx <- length(count_obs_year)
    n_total <- sum(count_obs_year) + n_sing
    fac_obs_year <- count_obs_year
    for (k in 1:ams_years_xx) {
        for (k2 in 1:amsl) {
            yobs <- ams_stat[k, k2]
            ams_stat_fac[k, k2] <- ifelse(yobs > 0, 2^k2, 0)
        }
        fac_obs_year[k] <- sum(ams_stat_fac[k, ], na.rm = T)
    }
    fac_obs_year_xx <- fac_obs_year
    ams_stat_xx <- ams_stat
    obsl <- ams_years_xx
    isub = 0
    eff_sites_subset = 0
    subs_years = 0
    corrf = 0
    ic = 0
    beta_all = 0
    distcol <- NCOL(distmat)
    distrange <- 1:distcol
    while (obsl > 0) {
        isub = isub + 1
        subs_xx_fac <- fac_obs_year_xx[1]
        subs_xx_T <- fac_obs_year_xx == subs_xx_fac
        subs_xx <- ams_stat_xx[subs_xx_T, ]
        year_site <- length(subs_xx[, 1])
        subs_years[isub] <- year_site
        sites_xx <- which(subs_xx[1, ] > 0)
        ts_ls <- length(sites_xx)
        distmat_xx <- distmat[sites_xx, sites_xx]
        dist_xx <- na.omit(as.vector(distmat_xx))
        distl <- length(dist_xx)
        corr_func = exp((-xall[1] * dist_xx)/((1 + xall[2] * 
            dist_xx)))
        corrf[isub] = mean(corr_func)
        onemat <- as.vector(matrix(1, distl, 1))
        ts_beta = (1.4 * ((year_site * ts_ls)^0.176))/(mean((onemat - 
            corr_func)^0.376))
        beta_all[isub] = ts_beta
        inf_cont = (1 + mean(corr_func^ts_beta) * (ts_ls - 1))^(-1)
        ic[isub] = inf_cont
        eff_sites_subset[isub] = year_site * ts_ls * inf_cont
        subs_xx_F <- subs_xx_T == FALSE
        fac_obs_year_xx2 <- fac_obs_year_xx[subs_xx_F]
        fac_obs_year_xx <- fac_obs_year_xx2
        ams_stat_xx2 <- ams_stat_xx[subs_xx_F, ]
        ams_stat_xx <- ams_stat_xx2
        obsl <- length(fac_obs_year_xx)
    }
    corr_mean = mean(corrf)
    ic_mean = mean(ic)
    eff_sites = sum(eff_sites_subset) + n_sing
    p_ec_h = 0.5/eff_sites
    t_h = 1/p_ec_h
    M <- matrix(nrow = 10, ncol = 1)
    M[, 1] <- c(amsl, n_total, n_sing, round(hw[1], 2), isub, 
        round(corr_mean, 3), round(xall[1], 8), round(xall[2], 
            8), round(eff_sites, 2), round(t_h, 2))
    coleff <- c("Number of sites", "Total observations", "Single observations", 
        "Heterogeneity measure", "Number of different subsets", 
        "Mean of cross-correlation of the subsets", "TS parameter 1", 
        "TS parameter 2", "Effective number of observations", 
        "Recurrence interval")
    rownames(M) <- coleff
    return(M)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
