
#####!!!Below are functions under development!!! #####
library(dplyr)

read_filing_info <- function(file_path) {
  # Read the text content of the file
  txt_content <- readLines(file_path)
  
  # Extract key information
  
  date <- sub(".*CONFORMED PERIOD OF REPORT:\\s*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", txt_content[grep("CONFORMED PERIOD OF REPORT:", txt_content)])
  trade_date <- as.Date(sub(".*\\t(\\d{8})", "\\1", date), format = "%Y%m%d")
  
  ticker <- sub(".*<issuerTradingSymbol>([^<]+)</issuerTradingSymbol>.*", "\\1", txt_content[grep("<issuerTradingSymbol>", txt_content)])
  
  insider_name <- sub(".*<rptOwnerName>([^<]+)</rptOwnerName>.*", "\\1", txt_content[grep("<rptOwnerName>", txt_content)])
  
  # Extract the transactionCode from the transactionCoding section
  # Given text
  transaction_coding_index <- grep("<transactionFormType>4</transactionFormType>", txt_content)
  transactionCode <- sub(".*<transactionCode>([^<]+)</transactionCode>.*", "\\1", txt_content[transaction_coding_index+1])
  
  # Convert trade_code to trade_type based on the provided information
  
  #Filter only Purchase info
  trade_type <- transactionCode
  if ("P" %in% transactionCode) {
    purchase_flag <- "Yes"
    sell_flag <- "No"
  } 
  else if("S" %in% transactionCode) {
    purchase_flag <- "No"
    sell_flag <- "Yes"
  }
  else {
    sell_flag <- "No"
    purchase_flag <- "No"
  }
  
  # Extract the Price and Qty fields differently for each transaction
  transaction_price_index <- grep("<transactionPricePerShare>", txt_content)
  price <- sub(".*<value>([^<]+)</value>.*", "\\1", txt_content[transaction_price_index+1])

  
  transaction_qty_index <- grep("<transactionShares>", txt_content)
  qty <- sub(".*<value>([^<]+)</value>.*", "\\1", txt_content[transaction_qty_index+1])

  clean_price <- as.numeric(price)
  clean_price[is.na(clean_price)] <- 0
  clean_price <- ifelse(trade_type == "S", -clean_price, clean_price)

  
  
  value=sum(as.numeric(clean_price)*as.numeric(qty))
  # Create a list to store the extracted information
  insider_info <- list(
    Trade_Date = format(trade_date, "%Y-%m-%d"),
    Ticker = ticker,
    Insider_Name = insider_name,
    Trade_Type = trade_type,
    Insider_buy = purchase_flag,
    Insider_sell = sell_flag,
    Price = unlist(clean_price),
    Qty = unlist(suppressWarnings(as.numeric(qty))),
    Value =value
  )
  
  return(insider_info)
}

all_data<- construct_in_buy_df()


construct_insider_df <- function(project_folder=getwd()){
  
  all_info <- list()
  
  # Set the path to the parent folder containing the subfolders
  parent_folder <- paste0(project_folder,"/Edgar filings_full text/Form 4/")
  
  # Get a list of subfolder names in the parent folder
  subfolder_names <- list.dirs(parent_folder, recursive = FALSE)
  
  # Loop through each subfolder
  for (subfolder in subfolder_names) {
    # Get a list of file names in the subfolder
    file_names <- list.files(subfolder, pattern = "\\.txt$", full.names = TRUE)
    
    # Loop through each file and call read_filing_info
    for (file in file_names) {
      info <- read_filing_info(file)
      all_info <- c(all_info, list(info))
      
    }
  }
  
  # Combine the list of results into a data.frame
  info_list <- do.call(rbind, all_info)
  info_data <- as.data.frame(info_list)
  
  
  return(info_data)
}

insider_buy_filter <- function(all_data){
  filtered_df <- subset(all_data, all_data$Insider_buy == "Yes")
  filtered_df <- filtered_df[, !(names(filtered_df) %in% "Insider_sell")]
  return(filtered_df)
}
insider_sell_filter <- function(all_data){
  filtered_df <- subset(all_data, all_data$Insider_sell == "Yes")
  filtered_df <- filtered_df[, !(names(filtered_df) %in% "Insider_buy")]
  return(filtered_df)
}

###Examples###
all_data<- construct_insider_df()
insider_buy_df <- insider_buy_filter(all_data)
insider_sell_df <- insider_sell_filter(all_data)
