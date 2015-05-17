library(stats)
library(ggplot2)
library(ggmap)
locations <- read.csv("data/locations.csv")

geo.dist = function(df) {
        require(geosphere)
        require(dplyr)
        d <- function(i,z){         # z[1:2] contain long, lat
                
                dist <- rep(0,nrow(z))
                dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),13:14],z[i,13:14])
                return(dist)
        }
        dm <- do.call(cbind,lapply(1:nrow(df),d,df))
        return(as.dist(dm))
}
km <- kmeans(geo.dist(locations),centers=7) 
hc <- hclust(geo.dist(locations))
plot(hc)    
km$cluster
locations$clustHc <-cutree(hc,5)
locations$clustKm <- km$cluster
set.seed(1532)
lrg <- 1:nrow(locations)
index1 <- sample(lrg,size = 1)
index2 <- sample(lrg,size = 1)

point1<-c(locations[index1,]$lon,locations[index1,]$lat)
point2<-c(locations[index2,]$lon,locations[index2,]$lat)

dist<-distHaversine(point1,point2)
library(dplyr)
spans <- NULL
for(i in 1:max(locations$clustKm)) {
        c1 <- locations%>%filter(clustKm==i)
        c1.lon.lat <- c1%>%select(lon,lat)
        c1.centroid <- centroid(c1.lon.lat)
   #     c1.span <- span(as.matrix(c1.lon.lat),fun=max)
   #     spans <- cbind(spans, c(i,c1.span))
        
        c1.map<-qmap(c1.centroid,zoom=8,color="bw",
                     base_layer = ggplot(aes(x=lon, y=lat), data = c1))
        c1.map <- c1.map + 
                geom_point(aes(x = lon, y = lat,size=4,color=clustKm), data = c1 ) +
                theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) 
        print(c1.map)
}

c2 <- locations%>%filter(clustKm==2)
c2.centroid <- centroid(c2%>%select(lon,lat))


c2.map<-qmap(c2.centroid,zoom=8,color="bw",
             base_layer = ggplot(aes(x=lon, y=lat), data = c2))
c2.map + 
        geom_point(aes(x = lon, y = lat,size=4,color="red"), data = c2 ) +
           theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) 


locations%>%group_by(clust)%>%summarize(count=n())
locations%>%group_by(clustKm)%>%summarize(count=n())

#test.data<-clean.storm.data[sample(1:nrow(clean.storm.data),200000,replace = FALSE),]
USA.map<-qmap(c1.centroid,zoom=7,color="bw",
              base_layer = ggplot(aes(x=lon, y=lat), data = locations))
USA.map + 
        geom_point(aes(x = lon, y = lat,size=4,shape=clust,color=clust), shape=22,data = locations ) +
        #  stat_density2d(aes(x = INTPTLONG, y = INTPTLAT, fill = ..level.., alpha = ..level..),
        #                 geom="polygon",
        #                 data = test.data) +
        # scale_fill_gradient(low = "green", high = "red")+  
        theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))+
        facet_wrap(~clust,scales="free")
