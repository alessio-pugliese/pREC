# Calculation of the distances between the sites
# belongs to rPREC
# written by
# Bjorn Guse: bguse@hydrology.uni-kiel.de
# Attilio Castellarin: attilio.castellarin.unibo.it

coordinates <- function(coord_region)

{

sites_no <- NROW(coord_region)
xcoord <- coord_region[,4]
ycoord <- coord_region[,5]

# all values are set to -1
coordmatrix <- matrix(NA,sites_no,sites_no)

 for (i1 in 1:(sites_no-1)) {
  for (i2 in (i1+1):sites_no) {
    coordmatrix[i1,i2] = sqrt((xcoord[i1]-xcoord[i2])^2+(ycoord[i1]-ycoord[i2])^2)/1000
      }
}

return(coordmatrix)

}