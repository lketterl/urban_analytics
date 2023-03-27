library(rgdal)
# read shapefile name variable LSOA
LSOA <- readOGR("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/06_Data_Viz_2_Mapping_Areas/data", "E06000042",
 verbose = FALSE)
# read lines shapefile
roads <- readOGR("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/06_Data_Viz_2_Mapping_Areas/data", "Road",
 verbose = FALSE)
# plot shapefile
plot(LSOA)

# load package classInt
library(classInt)
# convert imd_rank to numeric
LSOA@data$imd_rank <- as.numeric(LSOA@data$imd_rank)
# create breaks
breaks <- classIntervals(LSOA@data$imd_rank, n = 5, style = "fisher")
str(LSOA@data)
breaks
str(breaks)

# import RColorBrewer
library(RColorBrewer)
# display all palletes
display.brewer.all()
my_colors <- brewer.pal(6, "YlOrRd")
my_colors
# create a function to find the colours
colours_to_map <- findColours(breaks, my_colors)
# create a map
plot(LSOA, col = colours_to_map, border = "#0c0101", lwd = 0.1)
# create map w/o borders
plot(LSOA, col = colours_to_map, border = NA, lwd = 0.1)
# add roads
plot(roads, add = TRUE, col = "#0c0101", lwd = 0.3)

# import maptools
library(maptools)
# add legend
legend("top", legend = leglabs(breaks$brks, between = " to "),
 fill = my_colors, bty = "n", cex = 2)

# read schools csv
schools <- read.csv("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/06_Data_Viz_2_Mapping_Areas/data/Milton_Keynes_SS_13.csv")
schools
# remove schools without "Northing" or "Easting" using subset
schools <- subset(schools, !is.na(Northing) & !is.na(Easting))
# create a spatial points data frame
schools_sp <- SpatialPointsDataFrame(coords = cbind(schools$Easting, schools$Northing),
 data = schools, proj4string = CRS("+init=epsg:27700"))
plot(LSOA, col = colours_to_map, border = NA, lwd = 0.1)
plot(roads, add = TRUE, col = "#0c0101", lwd = 0.3)
plot(schools_sp, add = TRUE, pch = 19, col = "#bd16c0", cex = 0.4, lwd = 20)

# read WARD shapefile
WARD <- readOGR("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/06_Data_Viz_2_Mapping_Areas/data", "england_cmwd_2011Polygon")
WARD$name
plot(WARD)
text(coordinates(WARD)[,1], coordinates(WARD)[,2], labels = WARD@data$name, cex = 1)
# plot just "Loughton Park"
plot(WARD[WARD$name == "Loughton Park",])
plot(LSOA, col = colours_to_map, border = NA, add = TRUE)
plot(roads, add = TRUE, col = "#3a3939", lwd = 0.3)
plot(schools_sp, add = TRUE, pch = 19, col = "#050505", cex = 0.4, lwd = 10)
plot(WARD, border = "#050505", add = TRUE, lwd = 2)
text(coordinates(schools_sp)[,1], coordinates(schools_sp)[,2], labels = schools_sp@data$SCHNAME, cex = 1, pos=2)


# import ggplot2
library(ggplot2)
library(rgeos)
# Fortify
LSOA_fortified <- fortify(LSOA, region = "LSOA11CD")
head(LSOA_fortified)
# add the attribute data
LSOA_fortified <- merge(LSOA_fortified, LSOA@data, by.x = "id", by.y = "LSOA11CD")
# create a ggplot
map <- ggplot(LSOA_fortified, aes(x = long, y = lat, group = group, fill = imd_rank)) +
 geom_polygon() +
 coord_equal() +
 labs(x="Easting(m)", y="Northing(m)", title="IMD Rank", fill="IMD")
map

# fortify roads
roads_fortified <- fortify(roads)
plot1 <- c(geom_polygon(data = LSOA_fortified, aes(x = long, y = lat, group = group, fill = imd_rank)))
plot2 <- c(geom_path(data = roads_fortified, aes(x = long, y = lat, group = group), colour = "black", linewidth = 0.1))
ggplot() + plot1 + plot2 + coord_equal()
# create school plot
plot3 <- c(geom_point(data = schools, aes(x = Easting, y = Northing, colour = "school"), size = 4))
ggplot()+plot1+plot2+plot3 +coord_equal() + scale_fill_gradient( low="#473B31", high="#FFFFFF")  + theme_bw() +
  theme(axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title=element_blank(),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) + labs(fill = "IMD Rank",colour="") 


# tmap
library(tmap)
#read shapefile Leeds
Leeds <- readOGR("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/06_Data_Viz_2_Mapping_Areas/data", "E08000035")
head(Leeds@data)
# tm_shape with 5 levels

m <- tm_shape(Leeds, projection = 27700) + tm_polygons(col="imd_rank", n=10, style = "equal", border.col = "grey50", border.alpha = 0.5, title = "IMD Quintile", showNA = FALSE, palette = "YlOrRd")
m
