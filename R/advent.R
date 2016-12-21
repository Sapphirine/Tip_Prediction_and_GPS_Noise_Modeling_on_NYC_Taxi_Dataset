
init <- function(
  
){
  library(ggplot2)
  library(ggmap)
  library(dplyr)
  library(reshape2)
  library(zoo)
  library(scales)
  library(extrafont)
  library(grid)
  library(RPostgreSQL)
  library(rgdal)
  library(maptools)
  library(gpclib)
  library(sp)
  library(lubridate)
  gpclibPermit() # support for gpclib will be withdrawn from maptools at the next major release
  if ( Sys.info()['sysname'] == 'Darwin' ){
      #source("/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyc")
       libDir <- '/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyc-taxi-data'
      rootDir <- '/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyctaxi'
  } else {
      source("/home/PCUser/nyc-taxi-data/analysis/helpers.R")
       libDir <- '/home/PCUser/nyc-taxi-data'
      rootDir <- '~/nyctaxi'
  }

  
  assign('gcl',
         list(
           lib = libDir,
           root = rootDir,
           trips20160613_19allRData = paste0(rootDir,'/data/trips.20160613_19.all.RData')
         ), envir = .GlobalEnv)
  return(setwd(gcl$lib))
}

generateNewYorkMap <- function(
  
){
  tracts = sp::spTransform(rgdal::readOGR(dsn="./nyct2010_15b", layer = "nyct2010"),
                           sp::CRS("+proj=longlat +datum=WGS84"))
  # the +towgs84 tag should be used where needed to make sure that datum transformation does take place
  tracts@proj4string
  tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
  tracts.points = ggplot2::fortify(tracts, region = "id") # FIXME: library(broom) is preferred
  # if rror: isTRUE(gpclibPermitStatus()) is not TRUE, library(gpclib)
  tracts.map = inner_join(tracts.points, tracts@data, by = "id")
  
  nyc_map = tracts.map
  ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")
  manhattan_map = filter(tracts.map, BoroName == "Manhattan")
  

  return( list(nyc = nyc_map, mt = manhattan_map, ex = ex_staten_island_map,
               mtg = list(geom_polygon(data = manhattan_map, aes(x = long, y = lat, group = group), fill = "#080808", color = "#080808"))) )  
}

# function for dplyr
inManhattan <- function(
  lonv, latv,
  predefined=c('manhattan')[1],
  slope=1, intercept=114.69
){
  if (predefined == 'manhattan')
    return( ifelse(1 * lonv + 114.69 - latv < 0, TRUE, FALSE) )

  return( slope * lonv + intercept - latv < 0, TRUE, FALSE )
}
#inTimesSquare <- function(px,py) return(-83.775 < px & px < -83.765 & -10.82 < py & py < -10.81)
#in11thAve     <- function(px,py) return(-83.7856 < px & px < -83.78495 )

isConstant <- function(ratiov) {
    # see uws-pred.R for criteria reasoning
  return(
      ifelse(( .1975 < ratiov & ratiov < .2025),
             "20%",
             ifelse(( .2440 < ratiov & ratiov < .2510),
                    "25%",
                    ifelse(( .2945 < ratiov & ratiov < .30025),
                           "30%",
                           "other")))
  )
}

assign('gcl', # global constant list
       list(
           # translation bounds From: http://www.spatialreference.org/ref/epsg/2908/
           EPSG2908 = list( lonmin = -74.2700, lonmax = -71.7500,
                             latmin =  40.4700, latmax =  41.3100,
                             lonoff =   978000, latoff =   190000, # offset for meter (confirmed by "eyeballs")
                             theta = -.503)
       ),
       envir = .GlobalEnv)
message("ASSIGNED: gcl")


latlon2meter <- function(lon, lat) {
    mysp <- spTransform(SpatialPoints(cbind(lon,lat),
                                      proj4string=CRS("+ellps=WGS84 +datum=WGS84 +proj=longlat")),
                        CRS("+init=epsg:2908"))
    
    mysp@coords[,'lon'] <- mysp@coords[,'lon'] - EPSG2908$lonoff
    mysp@coords[,'lat'] <- mysp@coords[,'lat'] - EPSG2908$latoff
    return( mysp )
}
rotateManhattanY <- function(latm, lonm, theta)  cos(theta) * latm  - sin(theta) * lonm
rotateManhattanX <- function(latm, lonm, theta)  sin(theta) * latm  + cos(theta) * lonm

inUWS <- function(px2, py2) {
    return( 32000 < py2 & py2 < 44000 & -7500 < px2 & px2 < -4000 )
}
h2ts <- function(h) {
    return(
        ifelse( 1<=h & h<= 5,"A] 1 - 5 AM",
                ifelse( 6<=h & h<=10,"B] 6 - 10 AM",
                        ifelse(11<=h & h<=14,"C] 11 AM - 2 PM",
                               ifelse(15<=h & h<=18,"D] 3 - 6 PM",
                                      ifelse(19<=h & h<=21,"E] 7 - 9 PM","F] 10 PM - 12 AM")))))
    )
}

#h2tz <- function(h) ifelse(h<=2 | 18<=h, "NIGHT", ifelse(3<=h & h <=5, "VERYEARLY",ifelse(6<=h&h<=8,"EARLY",ifelse(h<=11, "MORNING",ifelse(12<=h & h<=14,"DAYTIME", "LATEDAY")))))
h2tz <- function(h) ifelse(h%in%c(22,23,0,1,2),"MIDNIGHT",ifelse(h%in%c(3,4,5),"VERYEARLY",ifelse(h%in%c(6,7,8,9),"MORNING",ifelse(h%in%c(10,11,12),"NOON",ifelse(h%in%c(13,14,15),"EARLYDAY",ifelse(h%in%c(16,17,18),"EVENING","NIGHT"))))))