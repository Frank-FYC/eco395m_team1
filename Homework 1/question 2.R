library(plyr)
library(ggplot2)
library(maptools)
library(maps)
library(mapproj)
library(usmap)


abia <- read.csv('ABIA.csv')
airports <- read.csv('airports.csv')

# Let's visualize

abia <- select(abia,Origin,Dest)
airports <- select(airports,iata_code,name,latitude_deg,longitude_deg)

abia <- count(abia, vars = c("Origin", "Dest"))

origin_xy <- merge(abia, airports, by.x="Origin", by.y="iata_code")
names(origin_xy) <- c("origin", "destination","trips", "o_name", "oX", "oY")

dest_xy <-  merge(origin_xy, airports, by.x="destination", by.y="iata_code")
names(dest_xy) <- c("origin", "destination","trips", "o_name", "oY", "oX","d_name", "dY", "dX")

dest_xy <- dest_xy[- grep("Erase", dest_xy$o_name),]#erase the damn island of nowhere
dest_xy <- dest_xy[- grep("Erase", dest_xy$d_name),]#erase the damn island of nowhere

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

usa <- map_data("usa") # we already did this, but we can do it again
state <- map_data("state")

plot_1 <- ggplot()+
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")+
  geom_polygon(data = state,aes(x = long, y = lat, fill = region, group = group), color = "black")+
  guides(fill=FALSE)+  # do this to leave off the color legend
  geom_segment(data=dest_xy,aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="black")+
  scale_alpha_continuous(range = c(0.09, 0.9))+
  theme(panel.background = element_rect(fill='white',colour='white'))+
  quiet+
  coord_fixed(1.3)+
  geom_point(data=dest_xy,aes(x=oX, y=oY) ,color="blue", size=1)+
  geom_point(data=dest_xy,aes(x=dX, y=dY) ,color="red", size=1)+
  ggtitle("Flights to and from Austin International Airport in 2008")+
  theme(plot.title = element_text(hjust = 0.5))
  
