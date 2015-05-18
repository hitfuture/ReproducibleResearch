library(stats)
library(dplyr)
library(ggplot2)
library(ggmap)
library(manipulate)
locations <- read.csv("data/locations.csv")

buildClusters <- function ( total.clusters) {
  
  geo.dist = function(df) {
          require(geosphere)
          require(dplyr)
          d <- function(i,z) {
                  # z[1:2] contain long, lat
                  
                  dist <- rep(0,nrow(z))
                  dist[i:nrow(z)] <-
                          distHaversine(z[i:nrow(z),13:14],z[i,13:14])
                  return(dist)
          }
          dm <- do.call(cbind,lapply(1:nrow(df),d,df))
          return(as.dist(dm))
  }
  km <- kmeans(geo.dist(locations),centers = total.clusters)
  hc <- hclust(geo.dist(locations))
  
  locations$clustHc <<- cutree(hc,total.clusters)
  locations$clustKm <<- km$cluster

}
mapRegionByCluster <-
        function (lon.lat = "Boise, Idaho", zoom = 3, curr.span,mssg = "") {
                a.map <- qmap(lon.lat, zoom = zoom, 
                              base_layer = ggplot(aes(x = lon, y = lat), data = c1))
                a.map <- a.map +
                        geom_point(aes(
                                x = lon, y = lat,size = 4,color = location_type
                        ), data = c1) +
                        theme(
                                legend.position = "none", axis.title = element_blank(),
                                text = element_text(size = 12)
                        ) +
                        labs(
                                title = paste("Sites",nrow(c1),
                                        "\nMap", i,"zoom",zoom, "lon span", round(curr.span[1],1),"lat span",round(curr.span[2],1),"\n",mssg, sep = ": "
                                )
                        )
                print(a.map)
        }


spans <- NULL
 
notMapped <- function(warn.message) {
        val <- 0
        m <- regexec("^Removed.*([0-9]+)",warn.message)
        matches <- regmatches(warn.message,m)
        a <- matches[[1]]
        len <- length(a)
        if (len > 1) {
                val <- as.integer(a[2])
        }
        val
}


zoomJumpBasedOnWarning <- function(warn.message) {
        row.count <- notMapped(warn.message)
        if (row.count > 10) {
                return(8)
        }
        if (row.count > 7) {
                return(4)
        }
        if (row.count > 3) {
                return(3)
        }
        if (row.count > 0) {
                return(1)
        }
        return(0)
        
        
}
#        next.seed <-sample(1:100000,1)
#        last.seed <- next.seed
#        set.seed(next.seed)
        tot.clusters <- 8
        buildClusters(tot.clusters)
                

for (i in 1:max(locations$clustHc)) {
        c1 <- locations %>% filter(clustHc == i)
        c1.lon.lat <- c1 %>% select(lon,lat)
        c1.centroid <- centroid(c1.lon.lat)
        c1.span <- span(as.matrix(c1.lon.lat),fun = max)
        zoom <- min(max(min(round((1 / log(
                sum(c1.span)
        ) * 100),0),15) + 1,3),20)
        spans <- rbind(spans, c(i,c1.span))
        tryCatch({
                mapRegionByCluster(c1.centroid,zoom = zoom,curr.span = c1.span)
        },
        warning = function(warn) {
                message(paste("got first warning: ",warn))
                zoom <- zoom - zoomJumpBasedOnWarning(warn$message)
                
                
        tryCatch({
                mapRegionByCluster(
                        c1.centroid,zoom = zoom,curr.span = c1.span,mssg = paste("WARNING 1:",warn)
                )
        },
        warning = function(warn) {
                message(paste("got second warning: ",warn))
                
                
                zoom <- zoom-zoomJumpBasedOnWarning(warn$message)
                tryCatch(
                        {
                                mapRegionByCluster(c1.centroid,zoom = zoom,curr.span = c1.span,mssg = paste("WARNING 2:",warn))
                        } ,  warning = function(warn) {
                                message(paste(
                                        "got third warning: ",warn
                                ))
                                zoom <- zoom-zoomJumpBasedOnWarning(warn$message)
                                
                               tryCatch( {
                                        mapRegionByCluster(c1.centroid, zoom = zoom,curr.span = c1.span,mssg = paste("WARNING 3:",warn))},
                                        warning = function(warn) {
                                                message(paste(
                                                        "got fourth warning: ",warn
                                                ))
                                               # zoom <- zoom-zoomJumpBasedOnWarning(warn$message)
                                                mapRegionByCluster(c1.centroid, zoom = 4,curr.span = c1.span,mssg = paste("WARNING 4:",warn))
                                                
                                        }
                                )
                                
                        })
        })
        }
        )


}


       