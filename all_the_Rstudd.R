library(jsonlite)
library(htmltools)
library(leaflet)
library(httr)
library(bitops)
library(tidyverse)
library(geojson)

token_strava = "94e2f8a9d2e9ed6e8a738f35f6ac536fc70606b9"
url = "https://www.strava.com/api/v3/routes/"
route_id = c("13689670", "11842759","12680474","14727019")
uri = paste(url,route_id,"?access_token=",token_strava, sep = "")
app_name="groupridedb_sandbox"
app_client_id="28823"
google_key ="AIzaSyD7gfkVIcvJ4NOYD2-VwUrz8mK5qdy2gi4"

d = list()
for (i in 1:length(route_id)){
  uri = paste(url,route_id[i],"?access_token=",token_strava, sep = "")
  d[[i]] = fromJSON(uri)
}
# stream=httr::GET(paste(url,route_id, sep=""),
#            query = list(access_token = token_strava))
# d = fromJSON(content(stream, "text"))
# 

decode_Polyline <- function(Google_polyline){
  
  vlen <- nchar(Google_polyline)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(Google_polyline, vindex, vindex))) - 63
      }
      
      vresult <- bitops::bitOr(vresult, bitops::bitShiftL(bitops::bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitops::bitAnd(vresult, 1)
      , -(bitops::bitShiftR(vresult, 1)+1)
      , bitops::bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(Google_polyline, vindex, vindex))) - 63
      }
      
      vresult <- bitops::bitOr(vresult, bitops::bitShiftL(bitops::bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitops::bitAnd(vresult, 1)
      , -(bitops::bitShiftR(vresult, 1)+1)
      , bitops::bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords <- tidyr::unite(coords, latlon, c(lat, lon), sep = ',')
  return(coords)
}

y =lapply(d, function(x) {
  decoded = decode_Polyline(x$map$summary_polyline)
  ll=separate(decoded, latlon, c('lat','lon'), sep = ',', convert = TRUE)
  list(name = x$name, distance = paste(round(x$distance/1000, 3), "kms", sep = " "), coord = ll[,c(2,1)])})#use summary_polyline will be smaller
# latlong=separate(y, latlon, c('lat','lon'), sep = ',', convert = TRUE)
# longlat = latlong[,c(2,1)]

labs = list(poly = latlong,
            label=paste0('<strong>title: </strong>',
                         "peepoop",
                         '<br><strong>distance:</strong>: ',
                         111))


# making geojson file -----------------------------------------------------

#geojsonio::geojson_write(SpatialPolygons(list(Polygons(list(Polygon(latlong)), "1"))), file = "Desktop/myfile.geojson")
#needs to be longitude latitude
gl = SpatialLines(list(Lines(list(Line(longlat)), "a")))
ggll = as.geojson(gl)
ggll = properties_add(ggll, .list=list(`Ride Name` = "Big Dicks Wild Ride", `Elevation Gain` = "A million fucking feet"))
geojsonio::geojson_write(ggll, file="samharbison.github.io/myfile.geojson")
properties_add(as.geojson(SpatialLines(list(Line(x[[3]])))), .list=list(`Ride Name` = x$name, `Distance` = x$distance ))

g=lapply(y, function(x){
    Line(x$coord)
  })
gl = SpatialLines(list(Lines(g,"a")))
# making the map ----------------------------------------------------------

map=leaflet(data = labs)  %>%
  addTiles() %>%
  addPolylines(lng = ~poly$lon,
               lat = ~poly$lat,
               label = ~HTML(label))








# making gpx file ---------------------------------------------------------
install.packages("rgdal")
library(rgdal)

install.packages("rgbif")
library(rgbif)

install.packages("pgirmess")
library("pgirmess")

route_data = labs$poly
names(route_data) = c("decimalLatitude", "decimalLongitude")
route_w_el = elevation(route_data, google_key)
waypoints = 1:nrow(route_w_el)
route_w_el = cbind.data.frame(waypoints,route_w_el[,c(2,1,3)])
writeGPX(route_w_el, "~/temp/file.gpx","t")