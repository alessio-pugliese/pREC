hw_region <-
function (inputdata)
{
hw_reg <- inputdata$region
hw_ams <- inputdata$ams

id_list <- hw_reg[,1]

ams_row <- NROW(hw_ams)
ams_col <- NCOL(hw_ams)

vec<-na.omit(c(as.matrix(hw_ams)))
vecl <- length(vec)

AMSLE <- matrix(-1,ams_col,1)
AMS <- matrix(-1,vecl,1)
ID <- AMS
ik=1
il=0

for (n in 1:ams_col) {
    ams_st <- na.omit(hw_ams[,n])
    amsl <- length(ams_st)
    id <- id_list[n]
    il = ik+amsl-1
    AMS[ik:il,1] <- ams_st
    ID[ik:il,1] <- rep(id,amsl)
    AMSLE[n,1] <- amsl
    ik=ik+amsl
    }

  library(nsRFA)

  hw_results<-HW.tests(AMS, ID, Nsim=500)

  return(hw_results)

  }
