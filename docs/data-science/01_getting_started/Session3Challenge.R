# Data Science at TUHH ------------------------------------------------------
# Chapter 3 ----

# 3.1 API data retrieval ----

# 3.1.0 Load libraries

library(httr)
library(glue)
library(jsonlite)
library(keyring)

# 3.1.1 Code
#Returns information for the top 50 cities worldwide.

#keyring::key_set("token")

api_key = key_get("token")

url <- modify_url(url = "http://dataservice.accuweather.com", path = glue("/locations/v1/topcities/50?apikey={api_key}&language=en-us&details=false"))

raw_data <- GET(url)

stop_for_status(raw_data) # automatically throws an error if a request did not succeed

data_list <- raw_data %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

glimpse(data_list)

# 3.2 WEBSCRAPING ----

# 3.2.0 Load libraries

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 3.2.1 COLLECT PRODUCT FAMILIES

url_home          <- "https://www.rosebikes.de/"

# Read in the HTML for the entire home webpage
html_home         <- read_html(url_home)

# Web scrape the urls for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".main-navigation-category-with-tiles__link ") %>%
  
  # ...and extract the href information
  html_attr('href') %>%
  
  # Remove the product families sale 
  discard(.p = ~stringr::str_detect(.x,"sale$")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Save the family urls
  transmute(family_url = str_glue("https://www.rosebikes.de{family_class}"))


# 3.2.2 COLLECT Gravel Categories 

# Read in the HTML for the entire Gravel family webpage
html_family         <- read_html(bike_family_tbl$family_url[3])


bike_category_tbl <- html_family %>%
  
  #Get the nodes for categories
  html_nodes(css = ".catalog-category-bikes__picture-wrapper") %>%
  
  # ...and extract the href information
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Save the Categories urls
  transmute(position, category_url = str_glue("https://www.rosebikes.de{subdirectory}"))


# 3.2.3 COLLECT Gravel/Backroad Bike Models 

# Read in the HTML for the entire Gravel/Backroad category webpage
html_models         <- read_html(bike_category_tbl$category_url[2])

bike_model_1_tbl <- html_models %>%
  
  #Get the nodes for bike models
  html_nodes(css = ".catalog-category-model__title") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "model")


#Read models prices
model_price_1_tbl <- html_models %>%
  
  #Get the nodes for bike models prices
  html_nodes(css = ".catalog-category-model__price-current-value") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "price")

#Joining category, model and price
bike_model_1_tbl <- left_join(bike_model_1_tbl, model_price_1_tbl, by = c("position")) %>%
  tibble(category= bike_category_tbl$category_url[2]) %>% 
  separate(col    = category,
           into   = c("remove", "category"),
           sep    = "fahrräder/") %>% 
  select(position, category, model, price)






# 3.2.4 COLLECT Gravel/Backroad al Bike Models 

# Read in the HTML for the entire Gravel/Backroad category webpage
html_models         <- read_html(bike_category_tbl$category_url[1])

bike_model_2_tbl <- html_models %>%
  
  #Get the nodes for bike models
  html_nodes(css = ".catalog-category-model__title") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "model")


#Read models prices
model_price_2_tbl <- html_models %>%
  
  #Get the nodes for bike models prices
  html_nodes(css = ".catalog-category-model__price-current-value") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "price")

#Joining category, model and price
bike_model_2_tbl <- left_join(bike_model_2_tbl, model_price_2_tbl, by = c("position")) %>%
  tibble(category= bike_category_tbl$category_url[1]) %>% 
  separate(col    = category,
           into   = c("remove", "category"),
           sep    = "fahrräder/") %>% 
  select(position, category, model, price)



# 3.2.5 COLLECT Gravel/Backroad limited Bike Models 

# Read in the HTML for the entire Gravel/Backroad category webpage
html_models         <- read_html(bike_category_tbl$category_url[3])

bike_model_3_tbl <- html_models %>%
  
  #Get the nodes for bike models
  html_nodes(css = ".catalog-category-model__title") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "model")


#Read models prices
model_price_3_tbl <- html_models %>%
  
  #Get the nodes for bike models prices
  html_nodes(css = ".catalog-category-model__price-current-value") %>%
  
  #...and extract the text
  html_text()%>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "price")

#Joining category, model and price
bike_model_3_tbl <- left_join(bike_model_3_tbl, model_price_3_tbl, by = c("position")) %>%
  tibble(category= bike_category_tbl$category_url[3]) %>% 
  separate(col    = category,
           into   = c("remove", "category"),
           sep    = "fahrräder/") %>% 
  select(position, category, model, price)



#Bind 3 categories tables
bike_model_tbl <- bike_model_3_tbl %>%
  rbind(bike_model_2_tbl) %>%
  rbind(bike_model_1_tbl) %>%
  mutate(price =  price %>% str_remove_all("\n")) %>%
  mutate(model =  model %>% str_remove_all("\n"))

#Print first 10 rows
bike_model_tbl %>% head(n = 10)
