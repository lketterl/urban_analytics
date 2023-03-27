# create two vectors, a = the years 2010 to 2017 each year 4 times
# and b = random numbers from 32 to 40
# create a data frame with the two vectors
a <- rep(2010:2017, each = 4)
b <- runif(32, 0, 40)
df <- data.frame(a, b)
# print the head of the data frame
head(df)

#vector of numbers from 1 to 25
lisofnumbers <- 1:25
# create matrix with 5 rows and 5 columns
matrixofnumbers <- matrix(lisofnumbers, nrow = 5, ncol = 5)
# multiply the matrix by 10
matrixofnumbers * 10
# multiply by itself
matrixofnumbers * matrixofnumbers
# extract 1st row
matrixofnumbers[1, ]
# extract  3rd and 4th column
matrixofnumbers[, 3:4]
# extract first and 4th row
matrixofnumbers[c(1, 4), ]

# return values in column a
df$a
# return values in column b
df$b
colnames(df)
colnames(df) <- c("year", "value")


# Getting data into R
# read table called ACS_14_5YR_S2001_with_ann.csv in subfolder data
# and store in variable table
table <- read.csv("02_Data_Manipulation/data/ACS_14_5YR_S2001_with_ann.csv",
 header = TRUE)
colnames(table)
str(table)


# spatial data
# read shapefile
library(rgdal)
# read shapefile
shapefile <- readOGR("02_Data_Manipulation/data/", "tl_2010_06075_tract10")
# plot shapefile
plot(shapefile)
# print slot names
slotNames(shapefile)
head(shapefile@data)
