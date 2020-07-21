# This project sprung out of an intense desire to work on something that did not have ANYTHING to do with Covid-19.
# I recalled working with this dataset a year ago for a class project, and so thought of giving it a go.
# The data is sourced from the 2015 Pune City Tree Census. Pune is a large city in western India with a population of about 5 million.
# With a total of 3 million trees, the person:tree ratio is quite decent for a city of this size.
# The final visualization, made through a mix of R and ArcGIS Pro shows the blooming pattern throughout the year for the 20 most frequently
# occurring tree species in the city.
# The city does not actually look as stunning as the video might make you feel, but it is a nice town.

# Loading libraries

library(ggplot2)
library(tidyverse)
library(sf)
library(dplyr)

#Tree data for Pune 2015
punetrees <- read.csv("data_trees/trees_locations.csv")

# creating two columns: start month & end month of flowering season for each species:
punetrees$flowering <- gsub("-", " ", punetrees$flowering)
punetrees$start_month <- sapply(strsplit(as.character(punetrees$flowering),' '), "[", 1)
punetrees$end_month <- sapply(strsplit(as.character(punetrees$flowering),' '), "[", 2)

# assigning a number for each month (1 : January, 2 : February, ..... 12 : December)
punetrees$start_month <- match(punetrees$start_month,table = month.name)
punetrees$end_month <- match(punetrees$end_month,table = month.name)

#created a 0 matrix of the months
m <- matrix(0,nrow = nrow(punetrees), ncol=12)
colnames(m) <- month.name

#converted the zeros that are in the blooming intervals to 1
for (i in (1:nrow(punetrees))){
  for (j in (punetrees$start_month[i]:punetrees$end_month[i])){
    m[i,j] <- 1
  }
}

#clipped the matrix to the dataframe
punetrees <- cbind(punetrees,m)

remove(m,i,j)
# filtering to only complete cases of blooming. We dont care about non-flowering trees
punetrees <- punetrees[complete.cases(punetrees$start_month), ]
punetrees <- punetrees[complete.cases(punetrees$end_month), ]


# removing columns we dont need data from:
punetrees <- punetrees %>% dplyr::select(common_name,
                                         January,February,March,April,May,June,July,August,September,October,November,December,
                                         latitude,longitude)

# Creating a subset of the top 20 most commonly found species in Pune. 
# I will be manually assigning them colors in ArcGIS to make life easier for me:

commontrees <- punetrees %>% select(common_name) %>% 
                             group_by(common_name) %>% 
                             summarise(count = n()) %>% 
                             arrange(desc(count)) %>% 
                             head(20)

# filtering out rest of the tree species:
top20tree_floweringmap <- left_join(commontrees, punetrees, by = "common_name")

# converting csv with latitude+longitude columns to a simple features shapefile with one single geometry column.
top20tree_floweringmap <- st_as_sf(top20tree_floweringmap, 
                                   coords = c("longitude", "latitude"), 
                                   crs = 4326)

# saving the shapefile to work on with ArcGIS Pro:
st_write(top20tree_floweringmap, "arcgis_flowering/top20tree_floweringmap/top20tree_floweringmap.shp")
