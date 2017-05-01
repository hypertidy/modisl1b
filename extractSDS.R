
extractSDS <- function(x, pattern) {
  txt <- system(sprintf("gdalinfo %s", x), intern = TRUE)
  sapply(strsplit(txt[grep(pattern, txt) - 1], "="), "[", 2)
}
MOD021 <- function(x) {
  x <- "/rdsi/PRIVATE/scratch/gdalthing/MOD021KM.A2012062.0455.006.2014220083128.hdf"
#   MOD021KM.A2012062.0455.006.2014220083128.hdf
#   SUBDATASET_20_NAME=HDF4_SDS:UNKNOWN:"MOD021KM.A2012062.0455.006.2014220083128.hdf":0
#   SUBDATASET_20_DESC=[406x271] Latitude (32-bit floating-point)
#   SUBDATASET_21_NAME=HDF4_SDS:UNKNOWN:"MOD021KM.A2012062.0455.006.2014220083128.hdf":1
#   SUBDATASET_21_DESC=[406x271] Longitude (32-bit floating-point)
#   SUBDATASET_22_NAME=HDF4_SDS:UNKNOWN:"MOD021KM.A2012062.0455.006.2014220083128.hdf":2
#   SUBDATASET_22_DESC=[15x2030x1354] EV_1KM_RefSB (16-bit unsigned integer)
  
  sds <- sapply(c("Longitude", "Latitude", "EV_1KM_RefSB \\(16"), function(tok) extractSDS(x, tok))
  lapply(sds, raster)
}

l <- MOD021(1)

library(dplyr)
modGCP <- function(x) {
  arr <- MOD021(x)
  bigdim <- dim(arr[[3]])[1:2]
  smalldim <- dim(arr[[1]])[1:2]
  lonind <- seq(1, bigdim[1], length = smalldim[1])
  latind <- seq(1, bigdim[2], length = smalldim[2])
  
 
#   <GCPList Projection="EPSG:4326">
#   <GCP Id="1" Info="a" Pixel="0.5" Line="0.5" X="0.0" Y="0.0" Z="0.0" />
#   <GCP Id="2" Info="b" Pixel="13.5" Line="23.5" X="1.0" Y="2.0" Z="0.0" />
#   </GCPList>
#         
        pl <- expand.grid(lonind, latind)
  gcp <- data_frame(Pixel = pl[,1] - 1, Line = pl[,2] - 1, 
                    X =  as.vector(values(arr$Longitude)), Y = as.vector(values(arr$Latitude)))
  
  gcpline <- c('<GCPList Projection="EPSG:4326">', 
                    sprintf('<GCP Id="%i" Info="%i" Pixel="%f" Line="%f" X="%f" Y="%f" Z="0.0" />', 
                     seq(nrow(gcp)), seq(nrow(gcp)), gcp$Pixel + 0.5, gcp$Line + 0.5, 
                     gcp$X, gcp$Y), 
               '</GCPList>')
 gcpline 
}

#system(sprintf("gdal_translate %s bark.vrt -of VRT", extractSDS(x, "EV_1KM_RefSB \\(16")))
tx <- readLines("bark.vrt")
tx <- c(tx[1], gcpline, tx[2:length(tx)])
writeLines(tx, "/rdsi/PRIVATE/scratch/gdalthing/ark.vrt")
