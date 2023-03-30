# import crimes
crimes <- read.csv("C:/Users/levin/Documents/uni/urban_analytics/git/urban_analytics/07_Data_Viz 3 _Visualizing_Point_Patterns/data/chicago_crimes_2016.csv")

# library lubridate
library(lubridate)
# parse date and time
crimes$New_Date <- ymd_hms((strptime(crimes$Date, "%m/%d/%Y %I:%M:%S %p",tz="UTC")))
# only include "BURGLARY" and there the columns ID, Latitide, Longitude, and New_Date
crimes <- crimes[crimes$Primary.Type == "BURGLARY",c("ID","Latitude","Longitude","New_Date")]

# ggplot
library(ggplot2)
# ggplot plot the burglaries by month
ggplot(data=crimes, aes(wday(crimes$New_Date,label = TRUE))) +
    geom_bar() +
    xlab("Day") + 
    ylab("Burglaries (count)")

# ggplot plot the burglaries by month
ggplot(data=crimes, aes(month(crimes$New_Date,label = TRUE))) +
    geom_bar() +
    xlab("Month") + 
    ylab("Burglaries (count)")

# ggplot facet by month
ggplot(data=crimes, aes(wday(crimes$New_Date,label = TRUE))) +
    geom_bar() +
    facet_wrap(~month(crimes$New_Date,label = TRUE)) +
    xlab("Day") + 
    ylab("Burglaries (count)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")

#Create a summary data frame of the counts of burglaries by hour time band and quarter of the year
t <- data.frame(table(hour(crimes$New_Date), quarter(crimes$New_Date)))
# rename columns
colnames(t) <- c("hour", "quarter", "count")
# ggplot plot the burglaries by hour and quarter
ggplot(data=t, aes(x=hour, y=quarter, fill=count)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="red") +
    xlab("Hour") + 
    ylab("Quarter") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot plot the burglaries by hour and quarter as lines
p <- ggplot(t, aes(x=hour, y=count, group=quarter, colour = quarter))
p + geom_line()

library(ggmap)
# getmap location chicago
chicago <- get_map(location = "chicago", zoom = 11)
# remove crimes where Longitude or Latitude is NA
crimes <- crimes[!is.na(crimes$Longitude) & !is.na(crimes$Latitude),]
# plot chicago with crimes
ggmap(chicago) + geom_point(data = crimes, aes(x = Longitude, y = Latitude))
# reduce point size and increase transparency
ggmap(chicago) + geom_point(data = crimes, aes(x = Longitude, y = Latitude), size = 0.5, alpha = 0.5)

# plot is as heatmap squares
ggmap(chicago, base_layer = ggplot(crimes, aes(x=Longitude, y=Latitude))) +
stat_bin2d(bins = 70)
# plot using hexagons instead of squares
ggmap(chicago, base_layer = ggplot(crimes, aes(x=Longitude, y=Latitude))) +
stat_binhex(bins = 70)
# tidy the map, removing the axes and gridlines and white background
ggmap(chicago, base_layer = ggplot(crimes, aes(x=Longitude, y=Latitude))) +
stat_binhex(bins = 70) +
theme_void() +
theme(legend.position="none")

# map using a density surface plot
ggmap(chicago, base_layer = ggplot(crimes)) +
  stat_density2d(aes(x = Longitude, y = Latitude,fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = crimes) +
  scale_fill_gradient(low = "black", high = "red")

# append quarter Q and day D columns to crimes
crimes$Q <- quarter(crimes$New_Date)
crimes$D <- wday(crimes$New_Date)
# create a plot for quarters
ggmap(chicago, base_layer = ggplot(crimes)) +
  stat_density2d(aes(x = Longitude, y = Latitude,fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ Q) +
  guides(alpha=FALSE) +
  theme_bw() +
theme(axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title=element_blank(),
      axis.ticks = element_blank(),
      legend.key = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())

# create a plot for days of the week
ggmap(chicago, base_layer = ggplot(crimes)) +
  stat_density2d(aes(x = Longitude, y = Latitude,fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ D) +
  guides(alpha=FALSE) +
  theme_bw() +
theme(axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title=element_blank(),
      axis.ticks = element_blank(),
      legend.key = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank())
