library(DBI)
library(RODBC)
library(dplyr)
library(tidyverse)
library(lubridate)
library(scales)
set.seed(2017)
options(digits=4)

con <- dbConnect(odbc::odbc(), "msSQL", timeout = 10)

dbGetQuery(con, 'USE bike_stores')
dbGetQuery(con, 'select * FROM production.brands')

# Plot total sales by brand
total_sales_brands <- dbGetQuery(con, "SELECT brand_name, SUM(sales.order_items.list_price*quantity*(1-discount)) as total_sales from
production.brands 
JOIN production.products
ON production.brands.brand_id = production.products.brand_id
JOIN sales.order_items 
ON production.products.product_id = sales.order_items.product_id
GROUP BY brand_name" )

total_sales_brands <- total_sales_brands[order(-total_sales_brands$total_sales),]

total_sales_brands %>%
  ggplot(aes(reorder(brand_name, total_sales), total_sales)) +
  geom_bar(stat='identity') +
  xlab("Brands") + 
  geom_text(aes(label=paste('$',formatC(total_sales, big.mark=',', format = 'f', digits=0))), vjust=-1) +
  ggtitle("Total Revenue By Brand") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) +
  ylab("") +
  xlab("")

# Investigate very poor Strider sales

strider_orders <- dbGetQuery(con, "SELECT sales.orders.* 
                                   from production.brands 
                                   JOIN production.products
                                   ON production.brands.brand_id = production.products.brand_id
                                   JOIN sales.order_items 
                                   ON production.products.product_id = sales.order_items.product_id
                                   JOIN sales.orders
                                   ON sales.order_items.order_id = sales.orders.order_id
                                   WHERE brands.brand_name = 'Strider'")

# We see that Strider bikes have only recently started to be sold.
strider_orders[order(strider_orders$order_date, decreasing=T),]

# Let's look at average sales per month of each brand to get a better indication of performance
monthly_orders_df <- dbGetQuery(con, "SELECT  production.brands.brand_name, 
                                              sales.order_items.list_price,
                                              sales.order_items.quantity,
                                              sales.order_items.discount,
                                              sales.orders.order_date
                                      from production.brands 
                                      JOIN production.products
                                      ON production.brands.brand_id = production.products.brand_id
                                      JOIN sales.order_items 
                                      ON production.products.product_id = sales.order_items.product_id
                                      JOIN sales.orders
                                      ON sales.order_items.order_id = sales.orders.order_id" ) 

# Group data by month and find monthly avg
monthly_orders_df <- monthly_orders_df %>%
  group_by(brand_name, month=floor_date(as.Date(order_date), "month")) %>%
  summarize(amount=sum(list_price*quantity*(1-discount))) %>%
  group_by(brand_name) %>%
  summarize(monthly_avg=mean(amount))

# Join the monthly avg data with the total sales data
total_sales_brands <- left_join(total_sales_brands, monthly_orders_df)
total_sales_brands_tidy <- total_sales_brands %>% tidyr::pivot_longer(cols=c('total_sales', 'monthly_avg'), names_to='type', values_to="sales")

# plot the grouped bar plot
total_sales_brands_tidy %>%
  ggplot(aes(reorder(brand_name, sales), sales, fill = type)) +
  geom_bar(stat='identity', position='dodge') +
  xlab("Brands") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title="Total Sales and Average Monthly Sales by Brand", subtitle = "Log 10 Scale", x="", y="USD", fill='Sales') +
  scale_fill_manual(labels = c("Monthly Avg", "Total"), values = c("red", "blue")) 
  

# Note the log scaled x axis. The monthly average sales fall in line with the total sales and Strider remains the worst performer when adjusted for 

# Plot sales by category
# We see that childrens bikes are lagging, maybe could start some trade up program to graduate bikes for families that don't want to spend on a child that outgrows the bike
total_sales_categories <- dbGetQuery(con, "SELECT category_name, SUM(sales.order_items.list_price*quantity) as total_sales from
production.categories
JOIN production.products
ON production.categories.category_id = production.products.category_id
JOIN sales.order_items 
ON production.products.product_id = sales.order_items.product_id
GROUP BY category_name")

total_sales_categories %>%
  ggplot(aes(reorder(category_name, total_sales), total_sales)) +
  geom_bar(stat='identity', fill='cyan') +
  coord_flip() +
  labs(title = "Total Revene by Category", x = "USD", y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(name="USD", labels=dollar_format())

# See how categories aggregate by gender

gender_categories_df <- dbGetQuery(con, "
WITH temp as 
  (SELECT product_id,
    CASE
      WHEN product_name LIKE '%women%' THEN 'female'
      WHEN product_name LIKE '%girl%' THEN 'female'
      ELSE 'male'
    END AS sex
FROM production.products)

SELECT category_name, temp.sex, SUM(sales.order_items.list_price*quantity*(1-discount)) as total_sales from
production.categories
JOIN production.products
ON production.categories.category_id = production.products.category_id
JOIN sales.order_items 
ON production.products.product_id = sales.order_items.product_id
JOIN temp
ON temp.product_id = production.products.product_id
GROUP BY category_name, temp.sex
           ")

gender_categories_df$total_sales <- as.integer(gender_categories_df$total_sales)
gender_categories_df$category_name <- str_replace(gender_categories_df$category_name, "Bicycles", "Bikes")

# Plot bar plot of category sales by gender
gender_categories_df %>%
  ggplot(aes(category_name, total_sales, fill=sex)) +
  geom_bar(stat='identity', position='stack') +
  coord_flip() +
  labs(title = ("Revenue by Category Aggregated by Gender"), x="", y="") +
  scale_y_continuous(labels=dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())
  
# This info could be used to make advertising gender specific


# Compare the revenues of the 3 stores for 2018
store_revenues_df <- dbGetQuery(con, "
           SELECT stores.store_name, 
                  SUM(list_price*quantity*(1-discount)) as revenue_2018
           FROM sales.stores 
           JOIN sales.orders ON sales.stores.store_id=sales.orders.store_id
           JOIN sales.order_items ON sales.orders.order_id=sales.order_items.order_id
           WHERE sales.orders.order_date >= '2018-01-01'
           GROUP BY sales.stores.store_name
           ")

# Plot store revenues for 2018 (can probably omit this plot, next one is better)
store_revenues_df %>% ggplot(aes(store_name, revenue_2018)) +
  geom_bar(stat='identity', fill='blue') +
  geom_text(aes(label = paste('$',formatC(revenue_2018, big.mark=',', format = 'f', digits=0)), 
                fontface=2), 
            col='white', vjust=2, size=7) +
  ggtitle("2018 Store Revenues") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  scale_y_continuous(breaks = NULL) +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank()) 
  
# Find number of active employees for each store
dbGetQuery(con, "
           WITH employees_per_store as (
             SELECT store_id,
                    COUNT(staff_id) as num_employees
             FROM sales.staffs
             WHERE active != 0
             GROUP BY store_id
           )
           SELECT sales.stores.store_name,
                  num_employees
           FROM sales.stores 
           JOIN employees_per_store
           ON sales.stores.store_id = employees_per_store.store_id
           ")

staff_sales_2018 <- dbGetQuery(con, "
           WITH staff_names_per_store as (
             SELECT sales.staffs.store_id as store_id,
                    store_name,
                    sales.staffs.staff_id as staff_id,
                    first_name
             FROM sales.staffs
             JOIN sales.stores
             ON sales.staffs.store_id = sales.stores.store_id
             WHERE active != 0
           ),
           staff_sales_2018 as (
             SELECT first_name,
                    SUM(quantity*list_price*(1-discount)) as sales_2018
             FROM staff_names_per_store
             JOIN sales.orders
             ON staff_names_per_store.staff_id = sales.orders.staff_id
             JOIN sales.order_items
             ON sales.orders.order_id = sales.order_items.order_id
             WHERE sales.orders.order_date >= '2018-01-01'
             GROUP BY first_name
           )
           SELECT staff_names_per_store.store_name,
                  staff_sales_2018.first_name,
                  staff_sales_2018.sales_2018
           FROM staff_names_per_store
           JOIN staff_sales_2018 ON staff_names_per_store.first_name = staff_sales_2018.first_name
           ")
# Plot staff sales for 2018
staff_sales_2018 %>% ggplot(aes(store_name, sales_2018, fill = first_name)) +
  geom_bar(stat='identity', position = 'stack') +
  geom_text(aes(label = paste('$',formatC(sales_2018, big.mark=',', format = 'f', digits=0)),
                fontface=2), 
            position = position_stack(vjust = .5),
            col='white', 
            size=6) +
  ggtitle("2018 Store Sales by Employee") +
  labs(x="", y="", fill = 'Employee') +
  theme_minimal() +
  scale_y_continuous(breaks = NULL) +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),) 

# This pulls only the sales associates (6/10 total employees)
# Layla is underperforming compared to Kali at Rowlett Bikes while the other employees seem to be reasonably close based on total revenue for their respective stores

# Let's investigate Baldwin Bikes' high revenue and see if it's due to more customers, higher sales per customer, or other marketing strategies

# Clearly Baldwin Bikes has much more customers, over 3 times as many as Santa Cruz and over 6 times as many as Rowlett
num_customers_df <- dbGetQuery(con, "
           WITH num_customers_by_store as (
           SELECT store_id, COUNT(sales.customers.customer_id) as num_customers
           FROM sales.customers
           JOIN sales.orders ON sales.customers.customer_id = sales.orders.customer_id
           WHERE sales.orders.order_date >= '2018-01-01'
           GROUP BY store_id
           )
           SELECT store_name, num_customers
           FROM num_customers_by_store 
           JOIN sales.stores ON num_customers_by_store.store_id = sales.stores.store_id
           ")
sales_per_customer_df <- merge(num_customers_df, store_revenues_df)
sales_per_customer_df$sales_per_customer <- sales_per_customer_df$revenue_2018 / sales_per_customer_df$num_customers
sales_per_customer_df

# We see that Baldwin Bikes has the more customers for the year as well as higher avg revenue per customer
# Do the stores discounting strategies differ much, products can have different discount rates so the store or staff has the capacity to adjust discount rates

store_discounts_df <- dbGetQuery(con, "
           SELECT stores.store_name,
                  AVG(discount) as avg_discount_2018
           FROM sales.stores
           JOIN sales.orders ON sales.stores.store_id=sales.orders.store_id
           JOIN sales.order_items ON sales.orders.order_id=sales.order_items.order_id
           WHERE sales.orders.order_date >= '2018-01-01'
           GROUP BY sales.stores.store_name
           ")
store_discounts_df
# Here we see that Baldwin Bikes' average discount rate falls in the middle of the three stores. The takeaway is that they are likely not employing a different discount technique. Is it possible they are enticing customers to purchase more items at a time?

num_items_df <- dbGetQuery(con, "
           SELECT stores.store_name,
                  SUM(quantity) as num_items
           FROM sales.stores
           JOIN sales.orders ON sales.stores.store_id=sales.orders.store_id
           JOIN sales.order_items ON sales.orders.order_id=sales.order_items.order_id
           WHERE sales.orders.order_date >= '2018-01-01'
           GROUP BY sales.stores.store_name
           ")
num_items_per_customer_df <- merge(num_items_df, num_customers_df)
num_items_per_customer_df$items_per_customer <- num_items_per_customer_df$num_items / num_items_per_customer_df$num_customers
num_items_per_customer_df
# We see that yes, Baldwin Bikes does sell more items per customer, which is a factor in their high sales.
# Finally, is it possible that they are just selling more expensive items (through targeted efforts or perhaps due to customer demographics)

store_sales_distribution <- dbGetQuery(con, "
           SELECT store_name,
                  quantity*list_price*(1-discount) as net_sale
           FROM sales.stores
           JOIN sales.orders ON sales.stores.store_id = sales.orders.store_id
           JOIN sales.order_items ON sales.orders.order_id = sales.order_items.order_id
           WHERE order_date >= '2018/01/01'
           ")

# plot all three histograms together
store_sales_distribution %>% 
  ggplot() +
  geom_histogram(aes(net_sale, fill=store_name), binwidth=750, alpha=.5) +
  ggtitle("Density of Store Sales") +
  labs(x="Dollar Amount of Order After Discount", fill='Store Name')

# Plot the three density plots together
store_sales_distribution %>% 
  ggplot() +
  geom_density(aes(net_sale, fill=store_name), alpha=.5) +
  ggtitle("Density of Store Sales") +
  labs(x="Dollar Amount of Order After Discount", fill='Store Name')

# This plot shows the breakdown of how large orders are for the three stores. Baldwin and Santa Cruz mirror the shape of each other for smaller orders, while Rowlet captures more mid-range orders. Baldwin has a larger percentage of orders in the $5,000-$7,000 range and $13,000+ range

