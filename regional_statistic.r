# Calculation of statistical values for the region
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

regional_statistic <- function(PREC_input_data)
{
region_data <- PREC$input_data$region
ams <- PREC$input_data$ams
ams_no <- NCOL(ams)

# Result matrix
STAT <- matrix(-1,ams_no,9)

# region membership
region <- region_data[,2]
max_reg <- max(region)

# area
area_all <- region_all[,3]
STAT[,1] <- area_all

for (n in 1:ams_no) {
  STAT[n,2] <- mean(na.omit(ams[,n]))
  STAT[n,3] <- max(na.omit(ams[,n]))
  }
  
# qmean/area
qmean_area_all <- STAT[,2]/STAT[,1]*1000
STAT[,4] <- round(qmean_area_all)

# qmax/area
qmax_area_all <- STAT[,3]/STAT[,1]*1000
STAT[,5] <- round(qmax_area_all,2)

# area_log
area_log_all <- log10(area_all)
STAT[,6] <- round(area_log_all,2)

# qmean_area_log
qmean_area_log_all <- log10(qmean_area_all)
STAT[,7] <- round(qmean_area_log_all,2)

# qmax_area_log
qmax_area_log_all <- log10(qmax_area_all)
STAT[,8] <- round(qmax_area_log_all,2)

# region
STAT[,9] <- region

colm<-c("Area", "Mean discharge", "Maximum discharge", "Unit mean discharge",
"Unit maximum discharge", "Area log", "Unit mean discharge log",
 "Unit maximum discharge log", "Region")
colnames(STAT)<-colm

return(STAT)

}