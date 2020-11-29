# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
  library(tidyverse)
  library(lubridate)
  library(readxl)

# 2.0 Importing Files ----
  bikes_tbl      <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
  orderlines_tbl <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
  bikeshops_tbl  <- read_excel("data-science/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Joining Data ----
  bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 4.0 Wrangling Data ----
  bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
    # 4.1 Split location into state and city
    separate(col    = location,
            into   = c("city", "state"),
            sep    = ", ") %>%

    # 4.2 Add the total price (price * quantity) 
    mutate(total.price = price * quantity) %>%
  
    # 4.3 Rename columns in underscores instead of dots
    set_names(names(.) %>% str_replace_all("\\.", "_"))

# 5.0 Business Insights ----
  
  # 5.1 Sales by State
  # Step 1 - Manipulate
  sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
    # Select columns
    select(state, total_price) %>%
  
    # Grouping by state and summarizing sales
    group_by(state) %>% 
    summarize(sales = sum(total_price)) %>%
  
    # Turn the numbers into a currency format 
    # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

  # Step 2 - Visualize
  sales_by_location_tbl %>%
  
    # Setup canvas with the columns year (x-axis) and sales (y-axis)
    ggplot(aes(x = state, y = sales)) +
  
    # Geometries
    geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
    geom_label(aes(label = sales_text)) + # Adding labels to the bars
    geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
    # Formatting
    # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
    # Again, we have to adjust it for euro values
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
    labs(
      title    = "Revenue by state",
      subtitle = "",
      x = "", # Override defaults for x and y
      y = "Revenue"
    ) +
  
    #rotate x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 5.2 Sales by Year and Location

  # Step 1 - Manipulate
  sales_by_year_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
    # Select columns and add a year
    select(order_date, total_price, state) %>%
    mutate(year = year(order_date)) %>%
  
    # Group by and summarize year and main catgegory
    group_by(year, state) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
  
    # Format $ Text
    mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                      decimal.mark = ",", 
                                      prefix = "", 
                                      suffix = " €"))

  # Step 2 - Visualize
  sales_by_year_location_tbl %>%
  
    # Set up x, y, fill
    ggplot(aes(x = year, y = sales, fill = state)) +
  
    # Geometries
    geom_col() + # Run up to here to get a stacked bar plot
  
    # Facet
    facet_wrap(~ state) +
  
    # Formatting
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
    labs(
      title = "Revenue by year and location",
      subtitle = "",
      fill = "States" # Changes the legend name
    ) +

    #rotate x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


