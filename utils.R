# install.packages("sf") # recommended when using RCloud
library("readr")
library("quarto")
library("tidyverse")
library("tidyr")
library("viridis")
library("data.table")
library("stringr")
library("processx")
library("dotenv")
library("jsonlite")
library("flextable")
library("knitr")
library("scales")
library("maps")
library("sf")
library("janitor")
library("httr")

### Never change/update the values below:
report_ids_list = c(2918, 2986, 3222, 2917, 3602, 3317, 3315, 3314, 3612, 3513, 3513, # addendum
               3615, 3527, 3614,                                          # artisanal
               3605, 2953, 3608)#, 3619)                                    # Part1

report_ids_ikasavea_list = c("b1559368-b7a3-464e-883a-34fe3d2cd7c0") 

# baseurl ikasavea
baseurl_ika="https://www.spc.int/coastalfisheries/"

get_all_country_codes <- function(x){
  res <- c("VU" ,"CK", "FJ", "FM", "KI", "MH", "NC", "NR", "NU", "PF", "PG", "PW", "SB", "TK", "TO", "TV", "WS")
  return(res)
}

download_ikasavea_data <- function(country_code, report_ids, folder_path, r_year){
  
  # Step 1: Authenticate with the SPC Coastal Fisheries API
  connection <- POST(
    paste0(baseurl_ika, "account/SignIn"),
    body = list(
      username = Sys.getenv("IKA_USER_NAME"),
      password = Sys.getenv("IKA_PASSWORD")
    ),
    encode = "form"
  )
  
  # Check if authentication was successful
  if (status_code(connection) == 200 && length(content(connection)) > 0) {
    # Store session cookies for reuse in other API calls
    session_cookies <- cookies(connection)
  } else {
    cat("❌ Authentication failed!\n")
    cat("Please check your credentials in the .env file\n")
    cat(paste("Status Code:", status_code(connection), "\n"))
    stop("Authentication for Ikasavea failed")
  }
  
  # Step 2: get authorities
  auths <- read.csv("config/list_authorities.csv") |>
    filter(flag %in% tolower(country_code)) 
  if (nrow(auths) == 0) {
    cat(paste0("No data from Ikasavea for country_code: "), country_code)
    return(invisible(NULL))   
  }
  
  # if auths, extract ids
  authority_ids <- auths |>
    dplyr::pull(Id)
  
  for (report_id in report_ids){
    body <- list(
      reportId           = report_id,
      includeIdColumns   = "false",
      includeIgnoredData = "false"
    )
    
    for (i in seq_along(authority_ids)) { body[[sprintf("authorityIds[%d]", i - 1)]] <- authority_ids[i] }
    
    # Request data export
    resp <- POST( paste0(baseurl_ika, "FieldSurveys/LdsStatistics/ExportDataAsJson"), 
                  body = body, 
                  encode = "form" )
    
    # Parse the JSON response
    response_data <- content(resp, "parsed")
    
    # Convert the data to a data frame for analysis
    if ("data" %in% names(response_data)) {
      df_result <- bind_rows(response_data$data) #|>
        # filter(Year == r_year)
      
      cat(paste("IKASAVEA: Retrieved", nrow(df_result), "data records for report", report_id, "year ", r_year, "\n"))
      
      
      if (report_id == "b1559368-b7a3-464e-883a-34fe3d2cd7c0") {
        report_name <- "trips_landing_site"
      } else {
        report_name <- report_id
      }
      
      filename_csv = paste0(folder_path, "/ikasavea/", tolower(country_code), "_", report_name, ".csv")
      write.csv(df_result, file = filename_csv, row.names = FALSE)

    } else {
      cat(paste0("⚠️ IKASAVEA: No 'data' field found in response for report", report_id, "year ", r_year, "\n"))
    }
  }
}

# Custom functions
# -- lazy for as.numeric()
an = function(x) as.numeric(x)
# -- "not in" function
'%nin%' <- function(x, y) !(x %in% y)

load_dot_env(file = ".env")

# function to generate a token
generate_token <- function(user_name, country_code){
  
  result <- run(
    "curl",
    args = c(
      "-X", "POST",
      "https://www.spc.int/ofp/tufman2api/api/ApiAccess/GetToken",
      "-H", "Content-Type: application/json",
      "-H", paste0("TufInstance: ", country_code),
      "-H", paste0("TufUser: ", user_name),
      "-d", sprintf('{"userEmail": "%s", "password": "%s"}', user_name, Sys.getenv("TUF_PASSWORD"))
    )
  )
  
  if (result$stdout == ""){
    stop("There was an issue with your request. Maybe you used the wrong country code?")
  }
  
  resp_content <- fromJSON(result$stdout)
  
  # Save both tokens with creation timestamp
  token_data <- list(
    access = resp_content$access_token,
    refresh = resp_content$refresh_token,
    created_at = Sys.time()
  )
  
  token <- token_data$access
  
  if (is.null(token)){
    stop("The token generated is empty, this might mean you don't have permissions to generate a token. Please,
         contact the DM SPC team for help: 'ofpdmpro@spc.int'")
  }else{
    
    saveRDS(token_data, 'token.RData')
    cat("New token saved successfully!\n")
    
    return(token)
  }
  
}  

# function to check if token exits and is valid, otherwise create one
load_token <- function(user_name, country_code){
  
  if (file.exists("token.RData")) {
    token_data <- readRDS('token.RData')
    
    # Check if token has expired (3600 seconds = 1 hour)
    time_elapsed <- as.numeric(difftime(Sys.time(), token_data$created_at, units = "secs"))
    
    if (time_elapsed < 3600) {
      cat("Token loaded and still valid (", round(3600 - time_elapsed), " seconds remaining)\n")
      token <- token_data$access
    } else {
      cat("Token expired...\n")
      token <- generate_token(user_name = user_name, 
                              country_code = country_code)
    }
  }else{
    token <- generate_token(user_name = user_name, 
                            country_code = country_code)
  }

  return(token)
}

#' Process and download report data for a country
#'
#' param country_code Character. Country code (e.g., "VU", "FJ")
#' param r_year Numeric. Reporting year
#' param report_ids Vector. Report IDs to download/process
#' param rewrite_files Logical. Whether to force redownload existing files
#' param user_name Character. Username for authentication (default: from Sys.getenv)
#' return List containing processed dataframes and missing file info

process_country_data <- function(country_code, 
                                 r_year, 
                                 report_ids = report_ids_list, 
                                 report_ids_ikasavea = report_ids_ikasavea_list,
                                 rewrite_files = FALSE,
                                 user_name = Sys.getenv("USER_NAME"),
                                 overwrite = TRUE) {
  
  print(country_code)

  this_yr_folder <- paste0("./data/report_", r_year, "_", tolower(country_code), "/")
  dir.create("./reports", showWarnings = FALSE, recursive = TRUE)
  dir.create(this_yr_folder, showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(this_yr_folder, "/additional_files"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(this_yr_folder, "/ikasavea"), showWarnings = FALSE, recursive = TRUE)
  
  # Get all report file names
  report_files <- paste0(tolower(country_code), "_", report_ids, ".csv")
  
  # Check if all files exist
  files_exist <- any(file.exists(file.path(this_yr_folder, report_files)))
  
  # Initialize results
  processed_data <- list()
  missing_files <- c()
  
  # If any files are missing or rewrite is requested, download data
  if (!files_exist | rewrite_files) {
    attrs <- list(
      flag_code = country_code,
      year = r_year
    )

    token <- load_token(user_name, country_code)

    download_report_data(
      token,
      user_name,
      country_code,
      filtered_reports = report_ids,
      attrs = attrs,
      record_current_date = FALSE,
      overwrite = overwrite,
      save_folder = this_yr_folder
    )
    
    if (length(report_ids_ikasavea) > 0 & !is.null(report_ids_ikasavea)){
      download_ikasavea_data(country = country_code, 
                             report_ids = report_ids_ikasavea, 
                             folder_path = this_yr_folder, 
                             r_year = r_year)  
    }
    

  }
  
  # Process existing files
  for (i in seq_along(report_files)) {
    df_name <- paste0("df_", tolower(country_code), "_", report_ids[i])
    file_csv <- file.path(this_yr_folder, report_files[i])
    
    if (file.exists(file_csv)) {
      df <- read.csv(file_csv)
      df_clean <- data_wrangling(df, report_id = readr::parse_number(report_files[i]))
      
      # Store in list
      processed_data[[df_name]] <- df_clean
      
      # Also assign to global environment (optional)
      assign(df_name, df_clean, envir = .GlobalEnv)
    } else {
      missing_files <- append(missing_files, report_ids[i])
    }
  }
  
  # Display missing files summary
  if (length(missing_files) > 0) {
    message("Missing T2 report IDs for ", country_code, ": ", 
            paste(missing_files, collapse = ", "))
  }
  
  # Return results
  invisible(list(
    country_code = country_code,
    processed_data = processed_data,
    missing_files = missing_files,
    folder = this_yr_folder
  ))
}



# top function that calls get_list_of_t2_reports and get_reports
download_report_data <- function(token, 
                                 user_name, 
                                 country_code, 
                                 filtered_reports, 
                                 attrs,
                                 base_url = "https://www.spc.int/ofp/tufman2api/api/ReportDefinition/DownloadResults",
                                 lang = "en",
                                 record_current_date = FALSE, 
                                 overwrite = FALSE,
                                 save_folder = "data/"){
  
  reports_selection <- get_list_of_t2_reports( token, 
                                         country_code, 
                                         user_name, 
                                         overwrite = overwrite,
                                         save_folder = save_folder,
                                         list_reports = filtered_reports)
  
  # select key reports
  # reports_selection <- all_reports |>
  #   filter(user_report_id %in% report_ids)
  
  # download data
  report_data <- get_reports(
    token = token, 
    user_name = user_name, 
    country_code = country_code, 
    filtered_reports = reports_selection, 
    attrs = attrs,
    record_current_date = record_current_date,
    overwrite = overwrite,
    save_folder = save_folder
  )         
  
}

# function to list the reports users have access to
get_list_of_t2_reports <- function(token, 
                                   country_code, 
                                   user_name, 
                                   overwrite = FALSE,
                                   save_folder = "data/",
                                   list_reports = c("all")){
  
  if (overwrite){
    filename_csv <- paste0(save_folder, "/list_of_t2_reports_", tolower(country_code), ".csv")
    
  }else{
    
    filename_csv <- paste0(save_folder, "/list_of_t2_reports_", tolower(country_code), ".csv")
    
    # Check if file exists
    if(file.exists(filename_csv)){
      print(paste0("Returning list of exisiting reports in Tufman2 created on : ", as.character(file.info(filename_csv)$ctime)))
      
      res <- read.csv(filename_csv)
      return(res)
    }
  }
  # Get the full list of reports user has available and the attributes that each of them require ####
  result_reports <- run(
    "curl",
    args = c(
      "-X", "GET",
      "--max-time", "2",        # timeout after 2 seconds

      "https://www.spc.int/ofp/tufman2api/api/ReportDefinition/AllSimple",
      "-H", "accept: application/json, text/plain, */*'",
      "-H", paste0("authorization: Bearer ", token),
      "-H", "content-type: application/json",
      "-H", paste0("tufinstance: ", country_code),
      "-H", "tufmodule: Reports",
      "-H", paste0("tufuser: ", user_name)
    )
  )
  
  if (result_reports$stdout == ""){
    stop("There was an issue with your request. Maybe you used the wrong country code?")
  }
  
  all_reports <- fromJSON(result_reports$stdout) |>
    select(Guid, 1:9, OptionLabels, LastModifiedDateTime)

  # if only certain reports were requested, retrieve only those ones
  if (!("all" %in% list_reports && length(list_reports) == 1)){
    all_reports <- all_reports |>
      filter(UserReportId %in% list_reports)
  }
  
  # Get reports attributes and combine with full list of reports ####
  attributes_per_report <- data.frame()
  for(i in 1:nrow(all_reports)) {
    
    # inform 
    print(paste0("Requesting attrs for report ", i, " from ", nrow(all_reports), ":", all_reports$Title[i]))
    
    sel_guid = all_reports$Guid[i]
    
    result_attributes <- run(
      "curl",
      args = c(
        "-X", "GET",
        paste0("https://www.spc.int/ofp/tufman2api/api/ReportDefinition/ByGuid?guid=", sel_guid),
        "-H", "accept: application/json, text/plain, */*'",
        "-H", paste0("authorization: Bearer ", token),
        "-H", "content-type: application/json",
        "-H", paste0("tufinstance: ", country_code),
        "-H", "tufmodule: Reports",
        "-H", paste0("tufuser: ", user_name)
      )
    )
    
    
    if (result_attributes$stdout == ""){
      stop("There was an issue with your request. Maybe you used the wrong country code?")
    }
    
    attributes_report <- jsonlite::fromJSON(result_attributes$stdout, flatten = TRUE)
    attributes_report_df <- bind_rows(attributes_report)
    
    # dive into the df and get the required attribute for the guid
    attrs_all <- attributes_report_df$Options[[1]] |>
      data.frame() |>
      filter(StatusId > 0) |>
      pull(Name) |>
      paste0(collapse=", ")
    
    attrs_guid <- data.frame(guid = sel_guid, 
                             report_attrs = attrs_all, 
                             sql_query = attributes_report_df$Sql[[1]])
    
    # get group by if exist 
    attrs_group_by <- attributes_report_df$Options$GroupBys |>
      data.frame()
    
    if (nrow(attrs_group_by)>0 ){
      attrs_group_by_res <- attrs_group_by |>
        slice(1) |>
        pull(Key)
      
      attrs_guid$report_group_by <- attrs_group_by_res 
      
    }else{
      attrs_guid$report_group_by <- NA
    }
    
    attributes_per_report <- rbind(attributes_per_report, attrs_guid)
    
  }
  
  all_reports_with_attrs <- all_reports |>
    janitor::clean_names() |>
    left_join(attributes_per_report) |>
    select(1:2, report_attrs, everything()) |>
    mutate(option_labels = sapply(option_labels, function(x) paste(x, collapse = ", "))) |>
    data.frame()
  
  sapply(all_reports_with_attrs, is.list)
  
  print(paste0("Saving reports available as a new csv: ", filename_csv))
  write.csv(all_reports_with_attrs, file = filename_csv)
  
  return(all_reports_with_attrs)
  
}


# function to get the data from selected reports
get_reports <- function(token, user_name, country_code, filtered_reports, attrs,
                        base_url = "https://www.spc.int/ofp/tufman2api/api/ReportDefinition/DownloadResults",
                        lang = "en",record_current_date = TRUE, overwrite = FALSE,
                        save_folder = "data/"){

    reports_selected <- filtered_reports |>
      pull(title)
    
    api_calls <- vector("character", length(reports_selected))
    for (i in seq_along(reports_selected)) {
      
      # Get guid and attributes for this report
      report_info <- filtered_reports |>
        filter(title == reports_selected[i]) |>
        select(guid, report_attrs, report_group_by, user_report_id, title)
      
      guid <- report_info$guid
      user_id <- report_info$user_report_id
      group_by <- report_info$report_group_by
      
      if (record_current_date) {
        filename_csv <-  paste0(save_folder,
                               tolower(country_code), "_",
                               user_id, "_", Sys.Date(), ".csv")
      } else {
        filename_csv <-  paste0(save_folder,
                               tolower(country_code), "_",
                               user_id, ".csv")
      }
      

      if (file.exists(filename_csv) && !overwrite) {
        print(paste0("The csv data from report ", report_info$title,
                     " already exists in your computer: ", filename_csv))
        next
      } else {
        
        report_attr_names <- strsplit(report_info$report_attrs, ",") |> 
          unlist() |> trimws()
        
        # Build runParams only for attributes relevant to this report
        params_list <- attrs[names(attrs) %in% report_attr_names]
        
        # Add group_by if not NA or empty
        if (!is.null(group_by) && !is.na(group_by) && nzchar(group_by)) {
          params_list$group_by <- group_by
        }
        
        # Convert runParams list to JSON and URL encode it
        runParams_json <- jsonlite::toJSON(params_list, auto_unbox = TRUE)
        runParams_encoded <- utils::URLencode(runParams_json, reserved = TRUE)
        
        # Build the full curl URL
        api_url <- glue::glue(
          "{base_url}?guid={guid}&lang={lang}&runParams={runParams_encoded}"
        )
        
        ret <- run(
          "curl",
          args = c(
            "-X", "GET",
            api_url,
            "-H", "accept: application/json, text/plain, */*'",
            "-H", paste0("authorization: Bearer ", token),
            "-H", "content-type: application/json",
            "-H", paste0("tufinstance: ", country_code),
            "-H", "tufmodule: Reports",
            "-H", paste0("tufuser: ", user_name)
          )
        )
        
        if (is.null(ret$stdout) || trimws(paste(ret$stdout, collapse = "")) == "") {
          message(
            paste0(
              "No data available for report: ", report_info$title,
              " and attributes: ", as.character(runParams_json),
              ", or your token has expired. Skipping..."
            )
          )
          next
        }
        
        if (grepl("^[{\\[]", trimws(ret$stdout))) {
          # JSON case
          ret_df <- jsonlite::fromJSON(paste(ret$stdout, collapse = ""), flatten = TRUE)$Rows |>
            data.frame()
        } else {
          # CSV case
          ret_df <- read.csv(text = ret$stdout, stringsAsFactors = FALSE)
        }
        
        
        if(length(ret_df) == 0){
          print(paste0("No data available for report: ", report_info$title, " and attributes: ",
                       as.character(runParams_json), ", skipping..."))
          next
        }
        
        ret_df <- ret_df |>
          dplyr::mutate(
            report_id = user_id,
            attrs_query = runParams_json,
            guid = guid
          ) |>
          dplyr::select(guid, attrs_query, dplyr::everything())
          
        print(paste0("Saving data from report ", report_info$title, " as csv: ", filename_csv))
        write.csv(ret_df, file = filename_csv, row.names = FALSE)
        
      }
      
      
      api_calls[i] <- api_url
      
    }
    return(api_calls)
    
}

# function for basisc data_wrangling
data_wrangling <- function(df, report_id){
  
  ret <- df |>
    janitor::clean_names() |>
    dplyr::select(-guid, -attrs_query, -report_id)
    
  
  return(ret)
}

# Function to safely read a CSV file, returning empty data if file doesn't exist
read_csv <- function(path) {
  if (file.exists(path)) {
    read.csv(path)
  } else {
    message(paste("File not found:", path))
    # Return an empty data frame
    data.frame()
  }
}

read_and_clean <- function(this_yr_folder, country_code, r_code, t2_dataset = TRUE){
  
  if (t2_dataset){
    data <- read_csv(str_c(this_yr_folder, country_code, "_", r_code, ".csv"))
    
    if (nrow(data) > 0) {
      
      data <- data |>
        data_wrangling(report_id = r_code) 
      
      if("yr" %in% colnames(data)){
        data <- data |>
          rename(year = yr)
      }
    }
    
  }else{
    data <- read_csv(str_c(this_yr_folder, "additional_files/", r_code, ".csv"))
  }
  

  return(data)
}

safe_read_and_clean <- function(folder, country, r_code, post_process = NULL) {
  tryCatch({
    data <- read_and_clean(folder, country, r_code = r_code) |>
      mutate(across(where(is.character), tolower)) |>
      filter(year %in% yrs_long)
    
    # Apply any additional post-processing if provided
    if (!is.null(post_process)) {
      data <- post_process(data)
    }
    
    return(data)
  }, error = function(e) {
    message(paste0("Warning: Could not load data for r_code ", r_code, ". Creating empty tibble."))
    message(paste0("Error details: ", e$message))
    return(data.frame())
  })
}

build_reports <- function(country_codes, 
                          max_year,
                          author, 
                          reports = c("addendum", "part1", "artisanal")){
  
  for (country_code in country_codes){
    
    country_folder = paste0("./data/report_", as.character(r_year),"_", tolower(country_code), "/")
    
    
    if ("part1" %in% reports){
      params_part1 <- list(
        country_code = country_code,
        year = r_year,
        author = report_author
      )
      
      # define output filenames
      output_filename_part1 = paste0("part1_report_", tolower(country_code), "_", r_year, ".pdf")
      
      
      # Render the document with parameters
      quarto_render(
        input = "template_part1.qmd",
        execute_params = params_part1,
        output_file = output_filename_part1
      )
      
      file.rename(
        from = output_filename_part1,
        to = file.path("./reports", output_filename_part1)
      )
      
      
      
    }
    
    if ("addendum" %in% reports){
      
      # Define parameters for each report
      params_addendum <- list(
        country_code = country_code,
        country_folder = country_folder,
        max_year = r_year,
        author = report_author
      )
        
      output_filename_addendum = paste0("addendum_report_", tolower(country_code), "_", r_year, ".docx")
        
      # Render the document with parameters
      quarto_render(
        input = "template_addendum.qmd",
        execute_params = params_addendum,
        output_file = output_filename_addendum
      )
      
      # Move the rendered file to the reports directory
      file.rename(
        from = output_filename_addendum,
        to = file.path("./reports", output_filename_addendum)
      )
      
    }
    
    if ("artisanal" %in% reports){
      
      # Define parameters for each report
      params_art <- list(
        flag = country_code,
        year = r_year,
        author = report_author
      )
      
      output_filename_art = paste0("artisanal_report_", tolower(country_code), "_", r_year, ".html")
      
      # Render the document with parameters
      quarto_render(
        input = "template_artisanal.qmd",
        execute_params = params_art,
        output_file = output_filename_art
      )
      
      # Move the rendered file to the reports directory
      file.rename(
        from = output_filename_art,
        to = file.path("./reports", output_filename_art)
      )
    }
  }
  return(list(reports))
  }
