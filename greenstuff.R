
A2015054053000.L1A_LAC	A2015054053000.L2_LAC_OC			


library(raster)
dp <- "/rdsi/PRIVATE/data/ladsweb.nascom.nasa.gov/__orders"
fs <- list.files(dp, pattern = "hdf$")
op <- par(mfrow = n2mfrow(length(fs)), 
          mar = rep(0, 4))

scl <- function(x) (x - min(na.omit(x)))/diff(range(na.omit(x)))
for (i in seq_along(fs)) {
chan <- sprintf('HDF4_EOS:EOS_SWATH:"%s/%s":MODIS_SWATH_Type_L1B:EV_1KM_RefSB', dp, fs[i])
la <-     sprintf('HDF4_SDS:UNKNOWN:"%s/%s":0', dp, fs[i])
lo <-     sprintf('HDF4_SDS:UNKNOWN:"%s/%s":1', dp, fs[i])

r <- brick(chan)
lon <- raster(lo)
lat <- raster(la)

cdex <- extent(65, 72, -68.5, -66.5)
plot(cdex + 10, type = "n", asp = 1/cos(-67.5 * pi/180))
points(as.vector(lon), as.vector(lat), pch = ".")
plot(cdex, add = TRUE, col = "red")
}
par(op)
## 6, 7 no good
i <- 2
#chan <- sprintf('HDF4_EOS:EOS_SWATH:"%s/%s":MODIS_SWATH_Type_L1B:EV_1KM_RefSB', dp, fs[i])
chan <- sprintf('HDF4_EOS:EOS_SWATH:"%s/%s":MODIS_SWATH_Type_L1B:EV_500_Aggr1km_RefSB', dp, fs[i])   
la <-     sprintf('HDF4_SDS:UNKNOWN:"%s/%s":0', dp, fs[i])
lo <-     sprintf('HDF4_SDS:UNKNOWN:"%s/%s":1', dp, fs[i])
r <- brick(chan)
lon <- raster(lo)
lat <- raster(la)

rs <- r[[1]]
plot(rs)

loni <- resample(setExtent(lon, extent(rs)), rs, method = "bilinear")
lati <- resample(setExtent(lat, extent(rs)), rs, method = "bilinear")
proj <- "+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84"
projectExtent(raster(cdex, nrow = 100, ncol = 100, crs = "+proj=longlat"), proj)

#gdal_translate 'HDF4_EOS:EOS_SWATH:"MOD021KM.A2012062.0315.006.2014220082743.hdf":MODIS_SWATH_Type_L1B:EV_500_Aggr1km_RefSB' -b 1 -b 4 -b 3 band.tif -ot Float64 
#gdalwarp band.tif out.tif -te -177929.00 -172559.00  133488.60   55758.43 -tr 1000 1000 -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'

# band 1 EV_250_Aggr1km_RefSB_data[0,i,j]
# band 4 EV_500_Aggr1km_RefSB_data[1,i,j]
# band 3 EV_500_Aggr1km_RefSB_data[0,i,j]

254 575 913
10801 13612 10669
gdal_translate 'HDF4_EOS:EOS_SWATH:"MOD021KM.A2012062.0315.006.2014220082743.hdf":MODIS_SWATH_Type_L1B:EV_250_Aggr1km_RefSB' -b 1  band1.tif -ot Float64  -scale 254 10801 0 1
gdal_translate 'HDF4_EOS:EOS_SWATH:"MOD021KM.A2012062.0315.006.2014220082743.hdf":MODIS_SWATH_Type_L1B:EV_500_Aggr1km_RefSB' -b 2  band2.tif -ot Float64  -scale 575 13612 0 1
gdal_translate 'HDF4_EOS:EOS_SWATH:"MOD021KM.A2012062.0315.006.2014220082743.hdf":MODIS_SWATH_Type_L1B:EV_500_Aggr1km_RefSB' -b 1  band3.tif -ot Float64  -scale 913 10669 0 1

gdalbuildvrt band.vrt -separate band1.tif band2.tif band3.tif 


gdalwarp band1.tif out1.tif -te -177929.00 -172559.00  133488.60   55758.43 -tr 250 250  -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'
gdalwarp band2.tif out2.tif -te -177929.00 -172559.00  133488.60   55758.43 -tr 250 250  -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'
gdalwarp band3.tif out3.tif -te -177929.00 -172559.00  133488.60   55758.43 -tr 250 250  -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'

gdalwarp band1.tif out1.tif -te 64 -67.5 72 -64.5 -tr 0.007 0.003  -t_srs '+proj=longlat +ellps=WGS84'
gdalwarp band2.tif out2.tif -te 64 -67.5 72 -64.5 -tr 0.007 0.003 -t_srs '+proj=longlat +ellps=WGS84'
gdalwarp band3.tif out3.tif -te 64 -67.5 72 -64.5 -tr 0.007 0.003 -t_srs '+proj=longlat +ellps=WGS84'

gdalwarp band1.tif out1.tif  -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'
gdalwarp band2.tif out2.tif -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'
gdalwarp band3.tif out3.tif  -t_srs '+proj=laea +lon_0=69 +lat_0=-67 +ellps=WGS84'


dd <- stack(file.path(dp, c("out1.tif", "out2.tif", "out3.tif")))
dd <- stack(file.path(dp, c("band1.tif", "band2.tif", "band3.tif")))

plotRGB(dd* 255, axes = TRUE)

library(rworldxtra)
data(countriesHigh)
w <- spTransform( subset(countriesHigh, SOVEREIGNT == "Antarctica"), CRS(projection(dd)))

ddsub <- crop(dd, e)
lonsub <- crop(loni, e)
latsub <- crop(lati, e)
col <- rgb(values(ddsub[[1]]), values(ddsub[[2]]), values(ddsub[[3]]))
plot(values(lonsub), values(latsub), col = col, pch = 16)

l3 <- readL3(oc$fullname[findInterval(as.POSIXct(as.Date("2012062", "%Y%j")), oc$date)])
l3$chla <- chla(l3, algo = "johnson", sensor = "MODISA")
xy <- bin2bounds(l3$bin_num, 4320)
##xy1 <- project(do.call(cbind, xy), proj)
plot(g)
points(values(lonsub), values(latsub), col = col, pch = 15, cex = (cos(values(latsub) * pi / 180)) * 2.6)
rect(xy$west, xy$south, xy$east, xy$north, col = chl.pal(l3$chla, alpha = 0.2), border = NA)


reflectance_offsets <- c(0, 0, 0)
reflectance_scales <- c(5.239028542e-05, 4.033463483e-05, 5.260160106e-05)

cellStats(dd, min); cellStats(dd, max)
for (i in 1:3) dd[[i]] <- (dd[[i]] + reflectance_offsets[i]) * reflectance_scales[i]


# EV_250_Aggr1km_RefSB (1)
reflectance_offsets=-0, -0
reflectance_scales=5.239028542e-05, 3.161556378e-05

#EV_500_Aggr1km_RefSB (2, 1)
#reflectance_offsets=-0, -0, -0, -0, -0
#reflectance_scales=5.260160106e-05, 4.033463483e-05, 3.798048783e-05, 3.418054985e-05, 2.786293771e-05

reflectance_offsets <- c(0, 0, 0)
reflectance_scales <- c(5.239028542e-05, 4.033463483e-05, 5.260160106e-05)
dd <- stack(file.path(dp, c("out1.tif", "out2.tif", "out3.tif")))
cellStats(dd, min); cellStats(dd, max)
for (i in 1:3) dd[[i]] <- (dd[[i]] + reflectance_offsets[i]) * reflectance_scales[i]
plotRGB(dd * 255)

reflectance_offsets <- c(-0, -0, -0, -0, -0)
reflectance_scales <- c(5.260160106e-05, 4.033463483e-05, 3.798048783e-05, 3.418054985e-05, 2.786293771e-05)


rgbi <- c(1, 4, 3)
a <- subset(r, rgbi)
for (i in 1:3) a[[i]] <- (a[[i]] + reflectance_offsets[rgbi[i]]) * reflectance_scales[rgbi[i]]
cellStats(a, min); cellStats(a, max)
plotRGB(a * 255)


plot(cdex + 5, type = "n")
points(as.vector(values(loni)), as.vector(values(lati)), pch = ".", 
     col = grey(seq(0, 1, length = 256))[scl(values(rs)) * 255 + 1])

