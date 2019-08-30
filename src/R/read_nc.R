
read_nc<-function(file,
                 stat="mean",
                 timesteps=c(1:364),
                 varno=3,
                 unitfactor=1,
                 cleanNA=T
                 ){
    
  # open NetCDF file
  ncin <- nc_open(file)
  print(names(ncin$var))
  
  # get the data for Lon and Lat
  vlon<-ncin$var[[1]] # lon
  vlat<-ncin$var[[2]] # lat
    
  # get the data for the observation variable
  var<-ncin$var[[varno]]
  
  lat <- ncvar_get(ncin, vlat$name, verbose = F)
  lon <- ncvar_get(ncin, vlon$name, verbose = F)
  
  nlat <- dim(lat)
  nlon <- dim(lon)
  
  ndims<-var$ndims
  varname<-var$name
  
  dlname <- ncatt_get(ncin, varname, "long_name")
  dunits <- ncatt_get(ncin, varname, "units")
  
  print(paste0(varname,"[",dunits$value,"] [ndims=",ndims,"] ",dlname$value))
  varsize <- var$varsize
  
  nt<- varsize[ndims] # number of timesteps
  
  var<- ncvar_get(ncin,varname,verbose=F)
  var<-var[,,timesteps]
  
# aggregate data depending on the chosen statistic
  if(stat=="90pc"){
    df<-apply(var, c(1,2), quantile,0.90,na.rm=T)
    err1 <- apply(var, c(1,2), quantile,0.99,na.rm=T)
    err2 <- apply(var, c(1,2), quantile,0.80,na.rm=T)
  }else{
    df<-apply(var, c(1,2), mean, na.rm=T)
    err1 <- apply(var, c(1,2), quantile,0.25,na.rm=T)
    err2 <- apply(var, c(1,2), quantile,0.75,na.rm=T)
  }

  lat<-as.vector(lat)
  lon<-as.vector(lon)
  value<-as.vector(df)
  err1<-as.vector(err1)
  err2<-as.vector(err2)
  value <- value * unitfactor
  err1 <- err1 * unitfactor
  err2 <- err2 * unitfactor
  
  df<-data.frame(id=1:length(value),lat,lon,value,err1,err2)
 
  # if clean NA values option is selected, then remove lines with NA or NaN
  if(cleanNA==T){
    df<-df %>% 
      filter(!is.na(value)) %>%
      filter(!is.nan(value))
  }
  
  return(df)
}