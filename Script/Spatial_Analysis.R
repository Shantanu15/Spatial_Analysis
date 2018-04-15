library(rgdal)
library(dplyr)
library(ggplot2)
library(maptools)

ogr <- readOGR(dsn = "E:\\Google Drive\\R\\Maps\\maps-master\\Districts\\Census_2011", layer = "2011_Dist" ) 

##Importing Bihar data
bihar_out_data <- read.csv("E:\\Google Drive\\R\\Maps\\Data\\Bihar_Outstanding_NPA.csv", stringsAsFactors = FALSE)


bihar <- ogr[ogr$ST_NM== "Bihar",]
bihar@data$id <- rownames(bihar@data)
bihar.df <- fortify(bihar)
bihar.df <- left_join(bihar.df,bihar@data, by = "id")
bihar.df$censuscode <- as.numeric(as.character(bihar.df$censuscode))
bihar.df <- left_join(bihar.df,bihar_out_data, by = "censuscode")
bihar.df <- select(bihar.df, -District)


ggp <- ggplot(bihar.df, aes(x=long, y= lat, group= group)) + geom_polygon(aes(fill=Out_amt))
ggp <- ggp + geom_path(color = "grey", linestyle = 2) + coord_equal()
ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "blue", 
                                 space = "Lab", na.value = "grey50",
                                 guide = "colourbar")


plot(ggp)
