effective_sites <- function(ams_stat, distmat, xall, hw){

# number of sites
amsl <-NCOL(ams_stat)

# number of years
ams_years <- NROW(ams_stat)

# ams_stat = matrix with AMS (rows == years, column = sites, no value = NaN)
ams_stat_save <- ams_stat
ams_stat_ones <- ams_stat
ams_stat_fac <- ams_stat
count_obs_year <- matrix(-1, ams_years, 1)

# AMS matrix
# effective number of observations
for (k in 1:ams_years) {
  for (k2 in 1:amsl) {
    yobs <- ams_stat[k,k2]
    ams_stat_ones[k,k2]<- ifelse(yobs > 0, 1, 0)
    }
    count_obs_year[k]<- sum(ams_stat_ones[k,], na.rm=T)
  }

# delete years with no values
n_zero<- length(count_obs_year[count_obs_year==0])

# reduction of the matrix
n_sing_T <- count_obs_year==0
n_sing_F <- n_sing_T==FALSE
count_obs_year2 <- count_obs_year[n_sing_F]
count_obs_year <- count_obs_year2
# reduction of ams matrix
ams_stat2 <- ams_stat[n_sing_F,]
ams_stat <- ams_stat2

#ams_years_xx <- length(count_obs_year)

# single years of observation
n_sing<-length(count_obs_year[count_obs_year==1])

# reduction of the matrix
n_sing_T <- count_obs_year==1
n_sing_F <- n_sing_T==FALSE
count_obs_year2 <- count_obs_year[n_sing_F]
count_obs_year <- count_obs_year2
# reduction of ams matrix
ams_stat2 <- ams_stat[n_sing_F,]
ams_stat <- ams_stat2

# number of years with more than one observation
ams_years_xx <- length(count_obs_year)

#total number of observations
n_total <- sum(count_obs_year)+n_sing

fac_obs_year <- count_obs_year

for (k in 1:ams_years_xx) {
  for (k2 in 1:amsl) {
    yobs <- ams_stat[k,k2]
    ams_stat_fac[k,k2]<- ifelse(yobs > 0, 2^k2, 0)
    }
    fac_obs_year[k]<- sum(ams_stat_fac[k,], na.rm=T)
   }

# preset for while loop, i.e. calculation of number of effective observations
fac_obs_year_xx <- fac_obs_year
ams_stat_xx <- ams_stat
obsl <- ams_years_xx
isub = 0
eff_sites_subset=0
subs_years=0
corrf = 0
ic = 0
beta_all = 0
distcol <- NCOL(distmat)
distrange <- 1:distcol

# subsets
# while-loop
while (obsl > 0) {

# index
isub = isub +1
# selection of all years with identical combination with first considered year
subs_xx_fac <- fac_obs_year_xx[1]
subs_xx_T <- fac_obs_year_xx==subs_xx_fac
# current subset to work with
subs_xx <- ams_stat_xx[subs_xx_T,]

# number of years in current subset
year_site <- length(subs_xx[,1])
subs_years[isub] <- year_site

# number of sites in the current subset, i.e. sites with AMS value
sites_xx <- which(subs_xx[1,]>0)
ts_ls <- length(sites_xx)

# reduction of distmat to sites with the current subset
distmat_xx <- distmat[sites_xx,sites_xx]
dist_xx <- na.omit(as.vector(distmat_xx))
distl <- length(dist_xx)

# correlation function
corr_func = exp((-xall[1]*dist_xx)/((1+xall[2]*dist_xx)))
corrf[isub] = mean(corr_func)

# matrix with ones as much as values in the distance vector
onemat <- as.vector(matrix(1,distl,1))

# calculation of effective number of observations for current subset
ts_beta = (1.4*((year_site*ts_ls)^0.176))/(mean((onemat-corr_func)^0.376))
beta_all[isub] = ts_beta

# information content
inf_cont = (1+mean(corr_func^ts_beta)*(ts_ls-1))^(-1)
ic[isub] = inf_cont

# effective number of observations for current subset
eff_sites_subset[isub] = year_site * ts_ls * inf_cont

# removing of already calculated subsets from the intermediate AMS matrix and
# factorial number of observation vector
subs_xx_F <- subs_xx_T==FALSE
fac_obs_year_xx2 <- fac_obs_year_xx[subs_xx_F]
fac_obs_year_xx <- fac_obs_year_xx2
ams_stat_xx2 <- ams_stat_xx[subs_xx_F,]
ams_stat_xx <- ams_stat_xx2

# new number of years
obsl <- length(fac_obs_year_xx)

}

corr_mean = mean(corrf)
ic_mean = mean(ic)

# calculation of effective number of observations
eff_sites = sum(eff_sites_subset) + n_sing

# calculation of recurrence interval
p_ec_h = 0.5/eff_sites
t_h = 1/p_ec_h

# result output
M <- matrix(nrow=10, ncol=1)

M[,1] <- c(amsl, n_total, n_sing, round(hw[1],2), isub, round(corr_mean,3), round(xall[1],8),
 round(xall[2],8), round(eff_sites,2), round(t_h,2))

coleff<-c( "Number of sites", "Total observations", "Single observations",
"Heterogeneity measure","Number of different subsets", "Mean of cross-correlation of the subsets",
"TS parameter 1", "TS parameter 2",
 "Effective number of observations", "Recurrence interval")

rownames(M)<-coleff

return(M)

}
