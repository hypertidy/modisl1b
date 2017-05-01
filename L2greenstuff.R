## this describes the crux of the issue: 

##http://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?tid=5367
dp <- "/rdsi/PRIVATE/data/ladsweb.nascom.nasa.gov/__orders"

library(roc)
source('readL2.R')
fs <- list.files(file.path(dp, "ocL1_L2"), pattern = "L2_LAC_OC")
i <- 1
sdsnames <- sprintf(c('HDF4_SDS:UNKNOWN:"%s/%s":11', 'HDF4_SDS:UNKNOWN:"%s/%s":12', 'HDF4_SDS:UNKNOWN:"%s/%s":27'), file.path(dp, "ocL1_L2"), fs[i])
rr <- stack(sdsnames)
plot(values(rr[[1]]), values(rr[[2]]), col = chl.pal(values(rr[[3]])), pch = ".")

