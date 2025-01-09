README
================
2025-01-09

# **List of packages to install to run all the scripts**

``` r
# If for any of the packages the installation doesnt work 
# try BiocManager::install('package_name')

install.packages('tidyverse')
install.packages('data.table')
install.packages('stringr')
install.packages('readr')
install.packages('shiny')
install.packages('shinydashboard')
install.packages('plotly')
install.packages('umap')
install.packages('metap')
install.packages('packcircles')
install.packages('RMySQL')
# RMariaDB also works if RMySQL does not work for you or if you want to use the more updated version as RMySQL is a legacy package
install.packages('RMariaDB')
```

# **Clean the Data**

Run the data_collation.R to get the collated_raw_data.csv

Run the analysis_data_cleaning.Rmd to get the analysis_data_clean.csv

Run the Disease_info_cleaning.R to get Disease_information_clean.csv

Run the procedure_cleaning.R to get IMPC_procedure_clean.csv

Run the parameter_dRunescription_cleaning.Rmd to get
IMPC_parameter_description_clean.csv

# **Add parameters (Optional)**

Run unique_parameters.R to get unique_parameters.csv

Run parameter_grouping.R with parameters_to_groups.csv to get
analysis_data_grouped

This step is not required for database creation, it is needed if you
want to create the Shiny app from a csv.

# **Create the SQL database**

Run database_cleaning.R to get analysis_table.csv, parameter_table.csv,
gene_table.csv, procedures_table.csv, diseaseGene_table.csv,
disease_table.csv, group_list.csv

Run Sql_db_creation.sql to create the database

To query the database in R - use SQL_to_R.Rmd

# **R Shiny**

Run the shiny_app.R!

To query the database in R - use SQL_to_R.Rmd
