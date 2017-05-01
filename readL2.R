.L2swathnames <- function() {
  c("longitude", "latitude", "aot_865", "angstrom", "Rrs_412", 
    "Rrs_443", "Rrs_490", "Rrs_510", "Rrs_555", "Rrs_670", "chlor_a", 
    "Kd_490", "pic", "poc", "cdom_index", "par", "l2_flags")
}

.L2metanames <- function() {
  c( "orb_vec", 
     "sun_ref", "att_ang", "sen_mat", "scan_ell", "nflag", "tilt_ranges"
  )
}

.L2template <- function() {
  'HDF4_SDS:%s_L2:"%s":%i'
}
l2flags <- structure(list(Bit = c("01", "02", "03", "04", "05", "06", "07", 
                                  "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
                                  "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
                                  "30", "31", "32"), Name = c("ATMFAIL", "LAND", "PRODWARN", "HIGLINT", 
                                                              "HILT", "HISATZEN", "COASTZ", "spare", "STRAYLIGHT", "CLDICE", 
                                                              "COCCOLITH", "TURBIDW", "HISOLZEN", "spare", "LOWLW", "CHLFAIL", 
                                                              "NAVWARN", "ABSAER", "spare", "MAXAERITER", "MODGLINT", "CHLWARN", 
                                                              "ATMWARN", "spare", "SEAICE", "NAVFAIL", "FILTER", "SSTWARN", 
                                                              "SSTFAIL", "HIPOL", "PRODFAIL", "spare"), Description = c("Atmospheric correction failure", 
                                                                                                                        "Pixel is over land", "One or more product warnings", "High sun glint", 
                                                                                                                        "Observed radiance very high or saturated", "High sensor view zenith angle", 
                                                                                                                        "Pixel is in shallow water", "spare", "Straylight contamination is likely", 
                                                                                                                        "Probable cloud or ice contamination", "Coccolithofores detected", 
                                                                                                                        "Turbid water detected", "High solar zenith", "spare", "Very low water-leaving radiance (cloud shadow)", 
                                                                                                                        "Derived product algorithm failure", "Navigation quality is reduced", 
                                                                                                                        "possible absorbing aerosol (disabled)", "spare", "Aerosol iterations exceeded max", 
                                                                                                                        "Moderate sun glint contamination", "Derived product quality is reduced", 
                                                                                                                        "Atmospheric correction is suspect", "spare", "Possible sea ice contamination", 
                                                                                                                        "Bad navigation", "Pixel rejected by user-defined filter", "SST quality is reduced", 
                                                                                                                        "SST quality is bad", "High degree of polarization", "Derived product failure", 
                                                                                                                        "spare")), .Names = c("Bit", "Name", "Description"), row.names = c(NA, 
                                                                                                                                                                                           -32L), class = "data.frame")


readL2 <- function(file, vartype = c("swath", "meta"), data_frame = TRUE, filter0 = TRUE) {
  vartype <- match.arg(vartype)
  sds <- switch(vartype, 
                swath = .L2swathnames(), 
                meta = .L2metanames())
  sdsoffset <- switch(vartype, 
                      swath = 10, 
                      meta = 35)
  bx <- basename(file)
  sdspaths <- sprintf(.L2template(), roc:::.filesensor(bx), file, seq_along(sds) + sdsoffset)
  
  s1 <- stack(sdspaths, quick = TRUE)  
  names(s1) <- sds
  
  if (data_frame) {
    ## s1 <- values(s1)
    ##  s1 <- lapply(seq(ncol(s1)), function(x) s1[,x])
    ##  names(s1) <- sds
    s1 <- lapply(seq(nlayers(s1)), function(x) values(s1[[x]]))
    names(s1) <- sds
    s1 <- as_data_frame(s1)
    if (filter0) {
      wsub <- which(Reduce('|', lapply(select(s1, -longitude, -latitude, -l2_flags), ">=",  0)))
      s1 <- slice(s1, wsub)
    }
  }
  s1
}



