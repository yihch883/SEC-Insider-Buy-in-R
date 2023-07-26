library(edgar)
ticker_map <- read.table("ticker_cik_map.txt")
all_ticker <- ticker_map[,1]


#####Ticker/cik.no convertor #####
get_cik_number <- function(ticker, data=ticker_map) {
  
  ticker_index <- which(ticker_map[,1]==tolower(ticker)) #convert ticker to cik.no
  cik <- ticker_map[ticker_index,2]
  return(cik)
}


#####Function to get insider trading information for a given company#####
get_Form4_filings <- function(ticker,user_agent,time_period) {
  current_year <- format(Sys.Date(), "%Y")
  cik <- get_cik_number(ticker)
  filings <- getFilings(cik.no = cik,form.type="4" , filing.year = as.numeric(current_year),useragent=user_agent, downl.permit = "y")
  return()
}

#####loop to download all filing within the stock list#####
download_filings <- function(stock_list="KDP",user_agent="YourEmail@sample.what",time_period=format(Sys.Date(), "%Y")){
  
  for (i in 1:length(stock_list)){
    ticker <- stock_list[i]
    print(paste0("Now downloading:",ticker))
    get_Form4_filings(ticker,user_agent=user_agent,time_period=time_period)
    Sys.sleep(2)#To avoid downloading too much files in a short time period
  }
  
} 

#####Examples#####
download_filings("KDP")

my_stock_list <- c("AAPL","GOOG","KO","TSM","QCOM")
download_filings(my_stock_list,user_agent="123@ins.buy",time_period=format(Sys.Date(), "%Y"))

