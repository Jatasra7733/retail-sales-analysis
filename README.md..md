**Retail Sales Analysis**



An end-to-end retail sales analytics project using **R, SQL, and Power BI** to clean transaction data, explore sales performance, and build an interactive dashboard.



**Project Overview**



This project analyzes the UCI Online Retail transaction dataset to understand sales performance, customer purchasing behavior, product performance, and country-level trends.



The workflow covers data quality checks, data cleaning, exploratory data analysis (EDA), SQL-based analysis, and Power BI visualization.



**Business Questions**



\- What is the overall sales revenue and sales volume?

\- How do sales change over time?

\- Which products generate the most revenue?

\- Which countries contribute most to sales?

\- Who are the highest-value customers?

\- What data quality issues should be considered when interpreting the results?



**Tools and Technologies**



\- R: Data cleaning, transformation, and exploratory analysis

\- SQL / SQLite: Querying transaction data and calculating summaries

\- Power BI: Interactive dashboard and KPI reporting

\- GitHub: Version control and project documentation



**Project Structure**



```text

P1\_Retail\_Sales\_Analysis/

├── R/                  # R analysis scripts

├── sql/                # SQL queries

├── data/

│   ├── raw/            # Original source data (not uploaded)

│   └── processed/      # Cleaned data and local database (not uploaded)

├── powerbi/            # Power BI dashboard

├── reports/            # Summary CSVs and figures

├── screenshots/        # Dashboard screenshots

├── .gitignore

└── README.md

```

**Data Source**



**UCI Machine Learning Repository — Online Retail**



The dataset contains transaction records from a UK-based online retailer spanning 2010–2011.



Source: https://archive.ics.uci.edu/dataset/352/online+retail



Please refer to the source page for the dataset's terms and usage conditions.



**Data Preparation**



The cleaning workflow checks for missing values, invalid quantities, invalid unit prices, and cancelled invoices.



For the main sales analysis, cancelled invoices and rows with non-positive quantities or unit prices are excluded. Records with missing customer IDs may remain in overall sales totals but cannot be reliably attributed to individual customers.



A separate returns-review output is retained for further investigation of negative-quantity records.



**Metric definition:** Revenue in the main analysis is calculated as quantity multiplied by unit price for qualifying sales lines. It should not be interpreted as net revenue after returns, discounts, or other accounting adjustments unless those factors are explicitly modeled.



**Analysis and Dashboard**



The project is designed to report:



\- Revenue, order count, units sold, and identifiable customers

\- Monthly sales trends

\- Top products by revenue

\- Revenue by country

\- Highest-value customers

\- Data quality checks and limitations



The Power BI dashboard is saved in the `powerbi/` directory when included in the published repository. Dashboard screenshots can be added to `screenshots/`.



**Key Findings**



To be completed after validating the final R and SQL outputs and the Power BI dashboard.



**How to Reproduce**



1\. Download the Online Retail dataset from the UCI source linked above.

2\. Place the original Excel file in `data/raw/`.

3\. Open the project in RStudio.

4\. Install any required R packages that are not already installed.

5\. Run the R scripts in their intended order, following the file paths used by the scripts.

6\. Run the SQL setup and analysis scripts to create and query the SQLite database.

7\. Open the `.pbix` file in Power BI Desktop to explore the dashboard.



Exact script filenames and execution order should be confirmed against the files in the repository.



**Limitations**



\- Missing customer IDs limit customer-level analysis.

\- Excluding cancelled invoices and non-positive sales lines changes the population included in the main sales metrics.

\- Revenue calculations are analytical sales-line totals and may differ from accounting revenue.

\- Results depend on the cleaning rules and date interpretation used in the scripts.



**Author**



Amit Kumar

