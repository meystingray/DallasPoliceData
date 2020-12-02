remove.packages("blogdown")
install.packages("blogdown")
library(blogdown)

blogdown::new_site(dir = "C:/Users/sconroy/Documents/meystingray.github.io")
blogdown::new_post(title = "Test1234")
setwd("C:/Users/sconroy/Documents/meystingray.github.io")


"AIzaSyDKTY7qM26P0I3Lv4Xi196oP7-aVdVR2PU"

register_google(key = "AIzaSyDKTY7qM26P0I3Lv4Xi196oP7-aVdVR2PU")


library(ggmap)
bikemap1 <- get_map(location = c(lon = -95.3632715, lat = 29.7632836),
                    maptype = "terrain", source = "google", zoom = 14)

map <- get_map(location = "texas", zoom = 6, source = "stamen")
ggmap(map, fullpage = TRUE)

(map <- get_map("orlando, florida"))
ggmap(map)
