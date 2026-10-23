install.packages(c(
  "tidyverse",
  "readxl",
  "lubridate",
  "janitor",
  "DBI",
  "RSQLite",
  "renv"
))
# Inspect the dataset
dim(retail)
names(retail)
head(retail)
glimpse(retail)
# Check missing values
colSums(is.na(retail))
# Check Duplicate rows
sum(duplicated(retail))
# Check data range
range(retail$invoice_date, na.rm = TRUE)
# Summary statistics
summary(retail)
#============================Data Cleaning=======================================
# 1. Remove exact duplicate rows
retail_dedup= retail |>
  distinct()

# 2. Create an initial data-quality summary
quality_summary= retail_dedup |>
  summarise(
    total_rows = n(),
    missing_invoice = sum(is.na(invoice_no)),
    missing_stock_code = sum(is.na(stock_code)),
    missing_customer_id = sum(is.na(customer_id)),
    missing_quantity = sum(is.na(quantity)),
    missing_unit_price = sum(is.na(unit_price)),
    negative_quantity_rows = sum(
      quantity < 0, na.rm = TRUE
    ),
    nonpositive_price_rows = sum(
      unit_price <= 0, na.rm = TRUE
    ),
    cancelled_invoice_rows = sum(
      grepl("^C", as.character(invoice_no)),
      na.rm = TRUE
    )
  )

print(quality_summary)

# 3. Prepare the transaction fields
retail_prepared=  retail_dedup |>
  mutate(
    invoice_no = as.character(invoice_no),
    stock_code = as.character(stock_code),
    invoice_date = as.POSIXct(invoice_date),
    is_cancelled = grepl("^C", invoice_no),
    line_revenue = quantity * unit_price
  )

# 4. Preserve rows requiring separate review
returns_review=  retail_prepared |>
  filter(
    !is_cancelled,
    !is.na(quantity),
    quantity < 0
  )
require(lubridate)
# 5. Build the valid sales-line dataset
retail_clean=  retail_prepared |>
  filter(
    !is_cancelled,
    !is.na(invoice_no),
    !is.na(stock_code),
    !is.na(invoice_date),
    !is.na(quantity),
    !is.na(unit_price),
    quantity > 0,
    unit_price > 0
  ) |>
  mutate(
    line_revenue = quantity * unit_price,
    sales_date = as.Date(invoice_date),
    sales_month = floor_date(
      sales_date, unit = "month"
    )
  )

# 6. Review the results
cat("Original rows:", nrow(retail), "\n")
cat("After deduplication:", nrow(retail_dedup), "\n")
cat("Valid sales lines:", nrow(retail_clean), "\n")
cat("Rows flagged for returns review:",
    nrow(returns_review), "\n")

summary(retail_clean$line_revenue)

#=============================Save the cleaned data=============================
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  "reports",
  recursive = TRUE,
  showWarnings = FALSE
)
library(readr)
write_csv(
  retail_clean,
  "data/processed/retail_sales_clean.csv"
)

write_csv(
  returns_review,
  "data/processed/returns_review.csv"
)

write_csv(
  quality_summary,
  "reports/data_quality_summary.csv"
)

#=======================EDA script====================================
library(tidyverse)
library(lubridate)
library(scales)

# Create output folders if needed
dir.create("reports/figures",
           recursive = TRUE,
           showWarnings = FALSE)

# Load cleaned sales data
retail_clean <- readr::read_csv(
  "data/processed/retail_sales_clean.csv",
  show_col_types = FALSE
)

# Check that the data loaded correctly
dim(retail_clean)
glimpse(retail_clean)

#=============================Analyze overall sales performance========================
# Overall sales KPIs
sales_kpis= retail_clean |>
  summarise(
    total_revenue = sum(line_revenue, na.rm = TRUE),
    total_units = sum(quantity, na.rm = TRUE),
    total_orders = n_distinct(invoice_no),
    unique_customers = n_distinct(
      customer_id[!is.na(customer_id)]
    ),
    average_order_value = total_revenue / total_orders,
    average_units_per_order = total_units / total_orders
  )

print(sales_kpis)

readr::write_csv(
  sales_kpis,
  "reports/sales_kpis.csv"
)
#===========================Monthly revenue trends==============================
monthly_sales= retail_clean |>
  mutate(
    sales_month = floor_date(
      as.Date(invoice_date),
      unit = "month"
    )
  ) |>
  group_by(sales_month) |>
  summarise(
    revenue = sum(line_revenue, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    units_sold = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(sales_month)

print(monthly_sales)

# Visualize monthly revenue
p_monthly= ggplot(
  monthly_sales,
  aes(x = sales_month, y = revenue)
) +
  geom_line(linewidth = 1, color = "#2878B5") +
  geom_point(size = 2, color = "#E07A32") +
  scale_y_continuous(labels = label_number(
    prefix = "£",
    big.mark = ","
  )) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(
    title = "Monthly Retail Revenue",
    subtitle = "Revenue from qualifying positive sales lines",
    x = "Month",
    y = "Revenue (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_monthly)

ggsave(
  "reports/figures/monthly_revenue.png",
  plot = p_monthly,
  width = 10,
  height = 6,
  dpi = 300
)

#===============================Top 10 products by revenue============================
top_products= retail_clean |>
  mutate(
    product_name = coalesce(
      na_if(str_trim(description), ""),
      paste("Stock code", stock_code)
    )
  ) |>
  group_by(stock_code, product_name) |>
  summarise(
    revenue = sum(line_revenue, na.rm = TRUE),
    units_sold = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(revenue)) |>
  slice_head(n = 10)

print(top_products)

p_products=  ggplot(
  top_products,
  aes(
    x = reorder(product_name, revenue),
    y = revenue
  )
) +
  geom_col(fill = "#2878B5") +
  coord_flip() +
  scale_y_continuous(labels = label_number(
    prefix = "£",
    big.mark = ","
  )) +
  labs(
    title = "Top 10 Products by Revenue",
    x = NULL,
    y = "Revenue (£)"
  ) +
  theme_minimal()

print(p_products)

ggsave(
  "reports/figures/top_products.png",
  plot = p_products,
  width = 10,
  height = 7,
  dpi = 300
)

#===============================Country-level performance=======================
country_sales= retail_clean |>
  group_by(country) |>
  summarise(
    revenue = sum(line_revenue, na.rm = TRUE),
    orders = n_distinct(invoice_no),
    customers = n_distinct(
      customer_id[!is.na(customer_id)]
    ),
    units_sold = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(revenue))

print(country_sales, n = 15)

top_countries=  country_sales |>
  slice_head(n = 10)

p_countries= ggplot(
  top_countries,
  aes(x = reorder(country, revenue), y = revenue)
) +
  geom_col(fill = "#299D8F") +
  coord_flip() +
  scale_y_continuous(labels = label_number(
    prefix = "£",
    big.mark = ","
  )) +
  labs(
    title = "Top 10 Countries by Revenue",
    x = NULL,
    y = "Revenue (£)"
  ) +
  theme_minimal()

print(p_countries)

ggsave(
  "reports/figures/country_performance.png",
  plot = p_countries,
  width = 10,
  height = 7,
  dpi = 300
)

#========================Customer purchasing behavior============================
# Revenue and order metrics by customer
customer_sales= retail_clean |>
  filter(!is.na(customer_id)) |>
  group_by(customer_id) |>
  summarise(
    total_revenue = sum(line_revenue, na.rm = TRUE),
    total_orders = n_distinct(invoice_no),
    total_units = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    revenue_per_order = total_revenue / total_orders
  ) |>
  arrange(desc(total_revenue))

# Top 10 customers by revenue
top_customers= customer_sales |>
  slice_head(n = 10)

print(top_customers)

# Distribution of revenue per customer
p_customers = ggplot(
  top_customers,
  aes(
    x = reorder(as.character(customer_id), total_revenue),
    y = total_revenue
  )
) +
  geom_col(fill = "#8064A2") +
  coord_flip() +
  scale_y_continuous(labels = label_number(
    prefix = "£",
    big.mark = ","
  )) +
  labs(
    title = "Top 10 Customers by Revenue",
    x = "Customer ID",
    y = "Revenue (£)"
  ) +
  theme_minimal()

print(p_customers)

ggsave(
  "reports/figures/top_customers.png",
  plot = p_customers,
  width = 10,
  height = 7,
  dpi = 300
)

#===========================Save the analysis results==========================
readr::write_csv(
  monthly_sales,
  "reports/monthly_sales.csv"
)

readr::write_csv(
  top_products,
  "reports/top_products.csv"
)

readr::write_csv(
  country_sales,
  "reports/country_sales.csv"
)

readr::write_csv(
  customer_sales,
  "reports/customer_sales.csv"
)

#+++++++++++++Verify the KPI calculations++++++++++++++++++++++
retail_clean |>
  summarise(
    revenue_check = sum(line_revenue, na.rm = TRUE),
    order_count = n_distinct(invoice_no),
    customer_count = n_distinct(
      customer_id[!is.na(customer_id)]
    ),
    first_date = min(invoice_date, na.rm = TRUE),
    last_date = max(invoice_date, na.rm = TRUE)
  )

# Find the highest- and lowest-revenue months
monthly_sales |>
  slice_max(revenue, n = 1)

monthly_sales |>
  slice_min(revenue, n = 1)

# Show every month
print(monthly_sales, n = Inf)

#+++++++++++++++++++++++one important data quality check+++++++++++++++++++++++++++++++
retail |>
  summarise(
    total_rows = n(),
    cancelled_invoices = sum(
      grepl("^C", as.character(invoice_no)),
      na.rm = TRUE
    ),
    negative_quantity_rows = sum(
      quantity < 0,
      na.rm = TRUE
    ),
    zero_price_rows = sum(
      unit_price == 0,
      na.rm = TRUE
    ),
    missing_customer_ids = sum(
      is.na(customer_id)
    )
  )

#=============================Set up of  SQL database==============================
# Install these packages
install.packages(c("DBI", "RSQLite", "readr", "dplyr"))

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# Load the cleaned data
retail_clean= read_csv(
  "data/processed/retail_sales_clean.csv",
  show_col_types = FALSE
)

# Standardize date fields for SQLite
retail_clean= retail_clean |>
  mutate(
    invoice_date = as.character(invoice_date),
    sales_date = as.character(sales_date),
    sales_month = as.character(sales_month),
    is_cancelled = as.integer(is_cancelled)
  )

# Create the database folder
dir.create("data/processed",
           recursive = TRUE,
           showWarnings = FALSE)

# Connect to SQLite
con= dbConnect(
  SQLite(),
  "data/processed/retail_sales.db"
)

# Load cleaned transactions into a table
dbWriteTable(
  con,
  "retail_sales",
  retail_clean,
  overwrite = TRUE,
  row.names = FALSE
)

# Confirm that the table exists
dbListTables(con)
dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_rows FROM retail_sales"
)

# Close the connection
dbDisconnect(con)


#======================SQL queries==========================================
library(DBI)
library(RSQLite)
library(readr)

con= dbConnect(
  SQLite(),
  "data/processed/retail_sales.db"
)

# 1. Overall sales KPIs
sql_kpis= dbGetQuery(con, "
  SELECT
    ROUND(SUM(line_revenue), 2) AS total_revenue,
    SUM(quantity) AS total_units,
    COUNT(DISTINCT invoice_no) AS total_orders,
    COUNT(DISTINCT customer_id) AS unique_customers
  FROM retail_sales
")

print(sql_kpis)

# 2. Monthly revenue
sql_monthly= dbGetQuery(con, "
  SELECT
    substr(sales_date, 1, 7) AS sales_month,
    ROUND(SUM(line_revenue), 2) AS revenue,
    COUNT(DISTINCT invoice_no) AS orders,
    SUM(quantity) AS units_sold
  FROM retail_sales
  GROUP BY substr(sales_date, 1, 7)
  ORDER BY sales_month
")

print(sql_monthly)

# 3. Top 10 products
sql_products= dbGetQuery(con, "
  SELECT
    stock_code,
    description,
    ROUND(SUM(line_revenue), 2) AS revenue,
    SUM(quantity) AS units_sold
  FROM retail_sales
  GROUP BY stock_code, description
  ORDER BY revenue DESC
  LIMIT 10
")

print(sql_products)

# 4. Top 10 countries
sql_countries= dbGetQuery(con, "
  SELECT
    country,
    ROUND(SUM(line_revenue), 2) AS revenue,
    COUNT(DISTINCT invoice_no) AS orders,
    COUNT(DISTINCT customer_id) AS customers
  FROM retail_sales
  GROUP BY country
  ORDER BY revenue DESC
  LIMIT 10
")

print(sql_countries)

# 5. Top 10 customers
sql_customers= dbGetQuery(con, "
  SELECT
    customer_id,
    ROUND(SUM(line_revenue), 2) AS revenue,
    COUNT(DISTINCT invoice_no) AS orders
  FROM retail_sales
  WHERE customer_id IS NOT NULL
  GROUP BY customer_id
  ORDER BY revenue DESC
  LIMIT 10
")

print(sql_customers)

# Export SQL outputs
dir.create("reports", showWarnings = FALSE)

write_csv(sql_kpis, "reports/sql_kpis.csv")
write_csv(sql_monthly, "reports/sql_monthly_sales.csv")
write_csv(sql_products, "reports/sql_top_products.csv")
write_csv(sql_countries, "reports/sql_country_sales.csv")
write_csv(sql_customers, "reports/sql_top_customers.csv")

dbDisconnect(con)

#==============================Investigate the missing order==========================
library(DBI)
library(RSQLite)

con= dbConnect(
  SQLite(),
  "data/processed/retail_sales.db"
)

# Check for missing or blank invoice numbers
dbGetQuery(con, "
  SELECT
    COUNT(*) AS rows_checked,
    SUM(
      CASE
        WHEN invoice_no IS NULL
          OR TRIM(invoice_no) = ''
        THEN 1 ELSE 0
      END
    ) AS missing_or_blank_invoices
  FROM retail_sales
")

# Inspect invoice numbers with surrounding spaces
dbGetQuery(con, "
  SELECT DISTINCT invoice_no
  FROM retail_sales
  WHERE invoice_no <> TRIM(invoice_no)
     OR invoice_no IS NULL
  LIMIT 20
")

dbDisconnect(con)

#======================Fix the order-count comparison==============================
# R: Count only rows with valid invoice numbers
r_orders_valid= retail_clean |>
  filter(
    !is.na(invoice_no),
    trimws(invoice_no) != ""
  ) |>
  summarise(
    total_orders = n_distinct(invoice_no)
  )

print(r_orders_valid)

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  "data/processed/retail_sales.db"
)

sql_orders_valid= DBI::dbGetQuery(con, "
  SELECT COUNT(DISTINCT invoice_no) AS total_orders
  FROM retail_sales
  WHERE invoice_no IS NOT NULL
    AND TRIM(invoice_no) <> ''
")

#+++++++++++++Reconnect to the database
library(DBI)
library(RSQLite)
library(dplyr)

# Open a fresh connection
con= dbConnect(
  SQLite(),
  "data/processed/retail_sales.db"
)

# Check the SQL order count
sql_orders_valid= dbGetQuery(con, "
  SELECT COUNT(DISTINCT invoice_no) AS total_orders
  FROM retail_sales
  WHERE invoice_no IS NOT NULL
    AND TRIM(invoice_no) <> ''
")

print(sql_orders_valid)

# Close the connection after the query
dbDisconnect(con)

print(sql_orders_valid)

DBI::dbDisconnect(con)