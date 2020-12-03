remove.packages("blogdown")
install.packages("blogdown")
library(blogdown)
library(data.table)

setwd("C:/Users/sconroy/Documents/meystingray.github.io")
getOption("blogdown.subdir", "post")
blogdown::new_site(dir = "C:/Users/sconroy/Documents/meystingray.github.io")
blogdown::new_post(title = "Murder Rates in Dallas",ext = ".Rmd",
                   subdir = "C:/Users/sconroy/Documents/meystingray.github.io/content/post")
# Use the blogdown addin to create new posts


library(ggmap)
register_google(key = "")

bikemap1 <- get_map(location = c(lon = -95.3632715, lat = 29.7632836),
                    maptype = "terrain", source = "google", zoom = 14)

DallasMap <- get_map(location = "Dallas", zoom = 15, source = "google")
ggmap(map)
dev.off()
ggmap(map) + 
    geom_point(data = starbucksNC, aes(x = Longitude, y = Latitude), color = "navy", size = 1)