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
chicago <- get_map(location = "chicago", zoom = 1)

# missing api key