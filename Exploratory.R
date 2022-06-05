#importing the necessary libraries used throughout the dataset.
library(tidyverse)
install.packages("maps")
library(maps)
install.packages("plotrix")
library(plotrix)
library(ggplot2)

#options(max.print=999999)


install.packages("knitr")
library(knitr)
install.packages("rmarkdown")







#Loading the Dataframe
load("Hurdat2_tidy.RData")

cle#Viewing the Dataframe
view(hurrs)

#Dataframe with details of SANDY hurricane
famous_hurrs <-hurrs[hurrs$name == "SANDY" , ]
famous_hurrs








#A Function that takes the hurrs dataframe as an input and returns a List
Hurdat2_summary <- function(hurrs){
  # Finding the total number of storms 
  hurrs %>% group_by(name,year) %>%summarise(count = n())  -> hurrs_storms
  hurrs_storms %>% group_by(name) %>% summarise(count=n())%>% summarise(sum(count)) ->total_hurrs_storm
  
  # The range of years covered
  range(hurrs$year) -> year_range

  # A vector containing the number of track points for each storm
  
  track_points <- paste(hurrs$latitude, ":",  hurrs$longitude) 
  
  
  # a version that has been filtered to contain only storms from a single year
  hurrs%>%filter(year=="1991") -> Storms_filter_year
  
  #Creating a list to return the values
  named_list = c(total_hurrs_storm, year_range, track_points)
  return(named_list)
}



# Function Test by running it with three cases

# Function test by full hurrs dataframe
Hurdat2_summary(hurrs)
# Function test by a version that has been filtered to contain only storms from a single year
hurrs%>%filter(year=="1991") -> Storms_filter_year
Hurdat2_summary(Storms_filter_year)

# Function test by the one-hurricane dataframe  made in (a)
Hurdat2_summary(famous_hurrs)








track_points <- paste(hurrs$latitude,  hurrs$longitude )

hurrs%>% group_by(latitude, longitude)%>% summarise(count = n()) -> trackk
hist(trackk$longitude)







# Finding names of named hurricane by removing the unnamed hurricane storms
hurrs %>% filter(name!= "UNNAMED") -> named_hurrs
# Arranging the dataframe in ascending order with respect to year
named_hurrs%>%arrange(year) -> named_year_hurrs

#Extracting the first year of a named hurricane
named_year_hurrs[1,'year']





 ###################################################################################









#Defining  Function hurricaneBasemap
hurricaneBasemap = function() {
  #importing 
  library(maps)
  #Getting the world map coordinates using map_world
  world = map_data("world")
  
  #Creating a ggplot and initializing it to basemap
  basemap = ggplot() +
    # Defining the minimum and maximum latitude and longitude
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    # A geom that draws a polygon with group as world and aesthetic as longitude and latitude 
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  #returning basemap 
  return(basemap)
}
hurricaneBasemap() +
  # geom that draws a path with grouping dataframe hurrs and aesthetic as its latitude and longitude and coloring based on wind speed
  geom_path(data=hurrs, aes(x=longitude,y=latitude,group=id,color=windspeed)) + 
  #  overriding the existing scale
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd")








# A function that takes a year as input
year_hurrs <- function(){
  y = readline(prompt = "Enter the year you want to see : ")
  hurrs  %>% filter(year == y)-> hurrs_y
  return(hurrs_y)
}
  

#Function that takes a year as input and makes plot for only the hurricanes during that year.

hurricaneBasemap = function() { 
  
  world = map_data("world")
  basemap = ggplot() +
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  return(basemap) 
}
hurricaneBasemap() +
  geom_path(data=year_hurrs(), aes(x=longitude,y=latitude,group=id,color=windspeed)) + ggtitle("Hurricanes per Year ") +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd")








#####################################################################################################











library(maps)
cities = world.cities
cities


#Question 3(a)

# Function that calculates distance between (lon1,lat1) and (lon2,lat2)
Lat_long_dist <- function(x1,y1,x2,y2){
  
  dx = (x2-x1) *cos((pi/180) * ((y1+y2)/2))
  dy = y2-y1
  Distance = (111.325 *((dx)^2 + (dy)^2)^ 0.5)
  return(Distance)
  
}
Lat_long_dist(40,60,30,60)







# Function that takes city name as input and returns its long-lat to calculate Distance

Lat_long_distance <- function(){
  
  City_Name1 = readline(prompt = "Enter first city name : ")
  as.vector(as.matrix(cities %>% filter(name == City_Name1) %>% select("long") ))-> x1
  as.vector(as.matrix(cities %>% filter(name == City_Name1) %>% select("lat"))) -> y1
  City_Name2 = readline(prompt = "Enter second city name : ")
  as.vector(as.matrix(cities %>% filter(name == City_Name2) %>% select("long")))-> x2
  as.vector(as.matrix(cities %>% filter(name == City_Name2) %>% select("lat"))) -> y2
  
  
  Distance = Lat_long_dist(x1,y1,x2,y2)
  return(Distance)
  
}

Lat_long_distance()








#function to check if the user swapped latitude and longitude and if so, provide a useful warning message

Distance_Lat_long <- function(){
  
  City_Name1 = as.numeric(readline(prompt = "Enter first city name : "))
  as.vector(as.matrix(cities %>% filter(name == City_Name1) %>% select("long") ))-> x1
  as.vector(as.matrix(cities %>% filter(name == City_Name1) %>% select("lat"))) -> y1
  City_Name2 = readline(prompt = "Enter second city name : ")
  as.vector(as.matrix(cities %>% filter(name == City_Name2) %>% select("long")))-> x2
  as.vector(as.matrix(cities %>% filter(name == City_Name2) %>% select("lat"))) -> y2
  
  if(x1 < -180 || x1 > 180 || x2 < -180 ||x2 > 180 || y1 < -90 || y1 > 90 || y2 < -90 || y2 >90 &&TRUE){
    print("You have entered a city with wrong Latitude/ Longitude. Please check and enter again")
    
  } 
  else {
    
    Distance = Lat_long_dist(x1,y1,x2,y2)
    return(Distance)
  
  }
}
Distance_Lat_long()





distancee <- function(Miami, Long, Lat ){
  dx = (Long - Miami[1])*cos((pi/180)*((Lat+Miami[2])/2))
  dx = (Long - Miami[1])*cos((pi/180)*((Lat+Miami[2])/2))
  dy = Lat - Miami[2]
  Distance = (111.325 *((dx)^2 + (dy)^2)^ 0.5)
  return(Distance)
  
}

Storms_passed <- function(){
  Storm_Range <- cities %>% filter(name == 'Miami') %>% select(lat,long)
  Range <- c(Storm_Range[1,2],Storm_Range[1,1])
  hurrs_ss<- hurrs %>% mutate(a= distancee(Range,longitude,latitude)) %>%  filter(a < 100)
  storms_passed = unique(hurrs_ss[c("name") ] )
  print(storms_passed)
  return(hurrs_ss)
}  


Storms_passed()




hurricaneBasemap = function() {
  
  library(maps)
  world = map_data("world")
  basemap = ggplot() +
    
    coord_fixed(xlim=c(-90,-40),ylim=c(10,30)) +
    
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  
  return(basemap)
}
hurricaneBasemap() +
  
  geom_path(data=Storms_passed(), aes(x=longitude,y=latitude,group=id,color=windspeed)) + 
  
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd") + geom_point(aes(x =Storm_Range[1,2], y = Storm_Range[1,1])) +
  ggtitle("Hurricanes within 100km of Miami") + xlab("Longitude") + ylab("Latitude")
















##############################################################################





# Number of hurricane_Strength storms in each year

hurr_strength_storm <- subset(hurrs, windspeed > 60)
hurr_strength_storm%>% group_by(windspeed )%>% arrange(desc(windspeed) , year) %>% ggplot() +
  geom_col(aes(year,windspeed,  fill=year), position=position_dodge()) + ggtitle("Hurricane Strength Storm Year Wise")



# World map Data visualization of Hurricane Strength Storm

hurricaneBasemap = function() {
  library(maps)
  world = map_data("world")
  basemap = ggplot() +
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  return(basemap)
}
hurricaneBasemap() +
  geom_path(data=hurr_strength_storm, aes(x=longitude,y=latitude,group=id,color=windspeed)) +ggtitle("World map of Hurricane Strength Storm") +
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd")

# On the world map we can clearly see the hurricane Strength storm precisely in the Atlantic Specific regions.
# So yes. It is true that the number of hurricane-strength storms has been clearly increasing because of climate change in the Atlantic spefically.









hurrs_Landfall <- hurrs %>% filter(isLandfall == TRUE, isHurricane == TRUE) 

hurrs_Landfall%>%mutate(Decade = hurrs_Landfall$year - (hurrs_Landfall$year%%10) ) -> hurrs_Landfall
hurrs_Landfall %>% filter(Decade == "1950") ->AA
hurrs_Landfall %>% filter(Decade == "2010") ->BB

hurricaneBasemap = function() {
  library(maps)
  world = map_data("world")
  basemap = ggplot() +
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  return(basemap)
}
hurricaneBasemap() +
  geom_path(data=AA ,aes(x=longitude,y=latitude,group=id,color=windspeed)) +
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd") + ggtitle("Landslide in early decade") + xlab("Longitude") + ylab("Latitude")

hurricaneBasemap = function() {
  library(maps)
  world = map_data("world")
  basemap = ggplot() +
    coord_fixed(xlim=c(-130,30),ylim=c(0,90)) +
    geom_polygon(data=world, aes(x=long,y=lat,group=group), color="gray50", fill="white")
  return(basemap)
}
hurricaneBasemap() +
  geom_path(data=BB ,aes(x=longitude,y=latitude,group=id,color=windspeed)) +
  scale_color_distiller(type="seq",direction=1,palette="YlOrRd") +  ggtitle("Landslide in recent decade") + xlab("Longitude") + ylab("Latitude")

#We can see from the both the plots that the landslide occuring during the stormin decade 1950 was comparatively very less when compared to landslide in recent years
#We can also conclude that where storms cross the coastline (make landfall) is expanding in recent decades.





















hurrs%>%group_by(year)%>%summarise(Windspeed=max(windspeed))%>%ggplot(aes(year,Windspeed))+
  geom_line(colour='blue')+
  labs(x='Year',y='Windspeed',title='Peak Windspeed Over Time') + geom_point()




#Peak windspeed is increasing gradually . In the year 1850 it started with 100 and and increased to 125 in the upcoming years.
#In the graph we can clearly see that windspeed has reached above 160 in recent years








# histogram of storms by month over the entire dataset 



hurrs%>%group_by(month)%>%ggplot()+geom_histogram(aes(month),binwidth=1, color="white")+
  labs(y='Count of storms',x='Month',title='Storms Each month')




# adding a decade column to the hurrs dataframe.
hurrs_decade = mutate(hurrs,Decade = hurrs$year - (hurrs$year%%10) ) 
head(hurrs_decade)







































geo_area <- hurrs %>% filter(isLandfall == TRUE, isHurricane == TRUE) 
geo_area_2 <- hurrs %>% filter(isLandfall == FALSE, isHurricane == TRUE)
install.packages("gridExtra")
library(gridExtra)

pla <- ggplot() + geom_point(aes(y=geo_area$latitude, x=geo_area$longitude), color='yellow') + xlab("longitude") + ylab('latitude') + ggtitle('hurricanes on land')
plb <- ggplot() + geom_point(aes(y=hurrs$latitude, x=hurrs$longitude), color='orange') 
plc <- ggplot() + geom_point(aes(y=geo_area_2$latitude, x=geo_area_2$longitude), color='red') + xlab("longitude") + ylab('latitude') + ggtitle('hurricanes in the sea')
grid.arrange(pla, plb)

pld <- ggplot() + geom_line(aes(y=geo_area$latitude, x=geo_area$year)) + ggtitle("Hurricanes on land") + xlab("year") + ylab('latitude')
ple <- ggplot() + geom_line(aes(y=geo_area_2$latitude, x=geo_area_2$year)) + ggtitle("hurricanes on sea") + xlab("year") + ylab('latitude')
grid.arrange(pld, ple)











