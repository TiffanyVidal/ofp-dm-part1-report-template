###################################################
## Part 1, Addendum and Artisanal Annual Reports ## 
##                                               ##  
## Author: SPC                                   ##
## Year: 2026                                    ##
###################################################

# Step 1: Load libraries ####
rm(list = ls())
source("utils.R")
get_all_country_codes()

# Step 2: Define params ####
country_codes <- get_all_country_codes() # "VU"
r_year <- 2025
report_author <- "Jessica LS"
rewrite_files <- FALSE # TRUE if you want to make sure the data is updated.
reports_list <- c("artisanal")#, "addendum", "part1")

# Step 3: Read/download data ####

# Use the params above to download data from T2 and Ikasavea or read data if available in your computer
for (country_code in country_codes) {
  process_country_data(country_code = country_code, 
                       r_year = r_year,
                       rewrite_files = rewrite_files)
}

# Step 4: Generate reports ####
res <- build_reports(country_codes = country_codes,
                     max_year = r_year,
                     author = report_author,
                     reports = reports_list)
