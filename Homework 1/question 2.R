library(plyr)
library(ggplot2)
library(maptools)
library(maps)
library(mapproj)
library(usmap)


abia <- read.csv('ABIA.csv')
airports <- read.csv('airports.csv')

# Let's visualize

abia_1 <- select(abia,Origin,Dest)
airports <- select(airports,iata_code,name,latitude_deg,longitude_deg)

abia_1 <- count(abia_1, vars = c("Origin", "Dest"))

origin_xy <- merge(abia_1, airports, by.x="Origin", by.y="iata_code")
names(origin_xy) <- c("origin", "destination","trips", "o_name", "oX", "oY")

dest_xy <-  merge(origin_xy, airports, by.x="destination", by.y="iata_code")
names(dest_xy) <- c("origin", "destination","trips", "o_name", "oY", "oX","d_name", "dY", "dX")

#dest_xy <- dest_xy[- grep("Erase", dest_xy$o_name),]#erase the damn island of nowhere
#dest_xy <- dest_xy[- grep("Erase", dest_xy$d_name),]#erase the damn island of nowhere

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
  
# distance histogram
hist(abia$Distance,
       main = 'Distribution of flight distances',
       xlab = 'Distance [miles]',
       col = 'skyblue',
       border = 'skyblue4')
#I add a line indicating the mean of the group.
abline(v=mean(abia$Distance), 
       col = 'red',
       lwd = 3)
# I add a line indicating the median of the group.
abline(v= median(abia$Distance), 
       col = 'blue',
       lty = 5,
       lwd = 3)
legend('topright',
       legend = c('mean', 'median'), 
       lty = c(1,5),
       lwd = c(3,3),
       col = c('red', 'blue'))

plot_2 <- plot(abia$Distance, 
               abia$ArrDelay, 
               xlab = 'Distance [miles]', 
               ylab = 'Arrival Delay [min]', 
               main = 'Relationship between Distance and Arrival Delay', 
               pch = 20, 
               col = 'purple')

# Seasonal delays

abia$Season <- abia$Month

#recoding the spring months
abia$Season [abia$Season == 3] <- 100
abia$Season [abia$Season == 4] <- 100
abia$Season [abia$Season == 5] <- 100

#recoding the summer months
abia$Season [abia$Season == 6] <- 200
abia$Season [abia$Season == 7] <- 200
abia$Season [abia$Season == 8] <- 200

#recoding the fall months 
abia$Season [abia$Season == 9] <- 300
abia$Season [abia$Season == 10] <- 300
abia$Season [abia$Season == 11] <- 300

#recoding the winter months
abia$Season [abia$Season == 12] <- 400
abia$Season [abia$Season == 1] <- 400
abia$Season [abia$Season == 2] <- 400

plot_3 <- boxplot(formula = DepDelay ~ Season,
        data = abia,
        main = 'Departure delay by season',
        xlab = 'Season',
        ylab = 'Departure delay [min]',
        border = c('springgreen', 'gold', 'orange', 'skyblue'),
        names = c('Spring', 'Summer', 'Fall', 'Winter'))