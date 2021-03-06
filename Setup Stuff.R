remove.packages("blogdown")
install.packages("blogdown")

library(data.table)

# Run This
# 1. Library
library(blogdown)

# 2. Use RStudio AddIn to create a New Post

# 3. Serve site
setwd("C:/Users/sconroy/Documents/meystingray.github.io")
blogdown::serve_site()

# 4. Edit the post
getOption("blogdown.subdir", "post")
blogdown::new_site(dir = "C:/Users/sconroy/Documents/meystingray.github.io")
blogdown::new_post(title = "Murder Rates in Dallas",ext = ".Rmd",
                   subdir = "C:/Users/sconroy/Documents/meystingray.github.io/content/post")


# Use the blogdown addin to create new posts
setwd("C:/Users/sconroy/Documents/DallasPoliceData")
library(ggmap)
apiKey <- fread("./APIkey.key")
apiKey <- names(apiKey)
register_google(key = apiKey)


# Setup Shiny
setwd("C:/Users/sconroy/Documents/meystingray.github.io/Shiny")
library(shiny)
library(rsconnect)
runApp()
deployApp()
