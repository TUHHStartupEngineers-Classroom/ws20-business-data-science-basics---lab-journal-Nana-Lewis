#2.2: Web Scraping ----

#Task: Scrape one of the competitor websites of canyon
#(either https://www.rosebikes.de/ or https://www.radon-bikes.de) 
#create a small database.
#database should contain the "model names" and "prices" for 
#"at least one category". 
#Print the first 10 rows of your tibbles. 


#2.2.1 Bicycle Categories ----

#assign the  URL to a variable
rosebikes_category_url <- "https://www.rosebikes.de/fahrr%C3%A4der"
xopen(rosebikes_category_url)



#read in the html from the URL
rosebikes_category_html <- rosebikes_category_url %>%
  read_html() 

# Select the correct html nodes and extract the category
rosebikes_categories_tbl <-rosebikes_category_html %>%
  html_nodes(css =".catalog-navigation__list-item > a ") %>%
  html_attr("title") %>%
  
  #turn the vector into a table
  enframe(name = "position", value = "category")
rosebikes_categories_tbl



#2.2.2 Model and Price ----

#now to get the models under "kinder"
rosebikes_kinder_url <- "https://www.rosebikes.de/fahrr%C3%A4der/kinder"
#xopen(rosebikes_kinder_url)

### read in the html from the URL
rosebikes_kinder_html <- rosebikes_kinder_url %>%
  read_html()

#get data from JSON format about the model name and price
rosebikes_kinder_json_tbl <- rosebikes_kinder_html %>%
  html_nodes(css = ".catalog-product-tile__link") %>%
  html_attr("onclick")

#remove "window.dataLayer.push(" and ")"
rosebikes_kinder_tbl <- rosebikes_kinder_json_tbl %>%
  str_remove_all("window.dataLayer.push\\(") %>%
  str_remove("\\)$") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, "ecommerce","click","products") %>% # Need purrr and expl above
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble()
rosebikes_kinder_tbl