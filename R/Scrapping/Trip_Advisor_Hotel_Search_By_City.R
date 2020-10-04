# For RSeleium to work in same mchine as browser sits, requires Selenium Server to run locally, 
# please download latest Selenium server jar from - https://goo.gl/rW9Yvk (also included in zip file)
# and run (double click .jar file or run from command prompt java -jar selenium-server-standalone-x.xx.x.jar
# current version i have used is selenium-server-standalone-3.10.0 which is included in zip file)

# The code below searches for all hotels in the first page of search results (usually 30)
#Dates must be provided in YYYY-M-D format, eg 2018-5-1 and 2018-5-5 for search

suppressPackageStartupMessages({
  if (!require(rvest)){ install.packages("rvest")}; library(rvest)
})

suppressPackageStartupMessages({
  if (!require(RSelenium)){ install.packages("RSelenium")}; library(RSelenium)
})

suppressPackageStartupMessages({
  if (!require(lubridate)){ install.packages("lubridate")}; library(lubridate)
})

suppressPackageStartupMessages({
  if (!require(zoo)){ install.packages("zoo")}; library(zoo)
})


getcalclicktimes <- function(dt1, dt2)
{
  dt2 <- as.Date(dt2)
  intMon <- as.integer(format(dt2, "%m"))
  
  intcurrMon <- as.integer(format(as.Date(dt1), "%m"))
  
  return(intMon - intcurrMon)
}

getFormattedDates <- function(dt)
{
  
  CInDt <- as.Date(dt) %m-% months(1)
  strMon <- format(CInDt, "%m")
  
  if(substr(strMon,1,nchar(strMon)-1) == "0"){ strMon <- gsub("0", "", strMon)}
  strDay <- format(CInDt, "%d")
  if(substr(strDay,1,nchar(strDay)-1) == "0"){ strDay <- gsub("0", "", strDay)}
  CInStr<- paste(format(CInDt, "%Y"), "-", strMon, "-", strDay, sep="" )
  return(CInStr)
}


startHotelSearch <- function(City, InDt, Outdt)
{
  #RSelenium::rsDriver()# if needed
  remDr <- remoteDriver(browserName = "chrome")
  print("Launch browser")
  remDr$open()
  remDr$maxWindowSize()
  baseurl <- "https://www.tripadvisor.in"
  remDr$navigate(baseurl)
  
  Sys.sleep(5)
  
  elem <- remDr$findElement(using ='css', ".typeahead_input")
  elem$clearElement()  
  #City <- "Panjim"
  hotel_name <- City
  elem$sendKeysToElement(list(hotel_name))  
  
  dtpicker <-remDr$findElement(using ='css', ".ui_picker_field")
  
  dtpicker$clickElement()
  
  
  
  #Because of the way the calendar is implemented in TripAdvisor
  #the calendar data still shows April in html inspection after arrow click to bring May in front
  #so we input April dates to search for May
  
  elemdtarrow <- remDr$findElement(using ='xpath', "//div[@class = 'dsdc-next ui_icon single-chevron-right-circle']")
  
  stdtclicktimes <- getcalclicktimes(Sys.Date(), InDt)
  
  enddtclicktimes <- getcalclicktimes(InDt, Outdt)
  
  InDt <- getFormattedDates(InDt)
  Outdt <- getFormattedDates(Outdt)
  
  #InDt = "2018-4-1"
  #Outdt = "2018-4-5"
  
  if (stdtclicktimes > 0)
  {
    for(i in 1:stdtclicktimes)
    {
      elemdtarrow$clickElement()
      Sys.sleep(1)
    }
  }
  
  strXpathdt<- paste("//span[@data-date = '" , InDt, "']", sep="" )
  
  elemstdtcal <- remDr$findElement(using ='xpath', strXpathdt)
  elemstdtcal$clickElement()
  
  Sys.sleep(2)
  
  elemdtarrow <- remDr$findElement(using ='xpath', "//div[@class = 'dsdc-next ui_icon single-chevron-right-circle']")
  
  if (enddtclicktimes > 0)
  {
    for(i in 1:enddtclicktimes)
    {
      elemdtarrow$clickElement()
      Sys.sleep(1)
    }
  }
  
  Sys.sleep(1)
  
  strXpathdt<- paste("//span[@data-date = '" , Outdt, "']", sep="" )
  
  elemenddtcal <- remDr$findElement(using ='xpath', strXpathdt)
  elemenddtcal$clickElement()
  
  Sys.sleep(2)
  
  elemSubmit <- remDr$findElement(using ='xpath', "//button[@id = 'SUBMIT_HOTELS']")
  
  elemSubmit$clickElement()
  
  print("Start Hotels Search...")
  
  Sys.sleep(7)
  
  webElem <- remDr$findElement("css", "body")
  
  #Sys.sleep(10)
  for (i in 1:20)
  {
    
    webElem$sendKeysToElement(list(key = "page_down"))
  }
  
  Sys.sleep(5)
  
  search_hotels_url<-read_html (remDr$getPageSource()[[1]])
  
  R_Hotel_List<-search_hotels_url %>% html_nodes(xpath = "//div[@class='listing']")
  Hotel_names <- vector()
  Hotel_urls <- vector()
  Hotel_prices <- vector()
  Hotel_savings <- vector()
  Hotel_No_reviews <- vector()
  Hotel_avg_ratings <- vector()
  Hotel_facilities <- vector()
  Hotel_address <- vector()
  print("Hotels Search in progress...")
  for (hotel in R_Hotel_List)
  {
    Hotel_names <- c(Hotel_names,hotel %>% html_nodes("a.property_title") %>%
                       html_text())
    
    hurl <- hotel %>% html_nodes("a.property_title") %>% html_attr("href")
    Hotel_urls<-c(Hotel_urls,hurl)
    
    
    pricetag <- hotel %>% html_nodes("div.price")
    
    if (length(pricetag) == 0)
    {
      Hotel_prices<- c(Hotel_prices,"NA")
    }
    else
    {
      Hotel_prices<- c(Hotel_prices,(pricetag %>% '[['(1) %>% html_text()))
    }
    
    Hotel_prices <- gsub("\\???", "", Hotel_prices)
    
    savings <- hotel %>% html_nodes("div.save") %>% html_text()
    
    if (length(savings) == 0)
    {
      Hotel_savings<- c(Hotel_savings,"NA")
    }
    else
    {
      Hotel_savings<- c(Hotel_savings,savings)
    }
    
    Hotel_savings <- gsub("\\???", "", Hotel_savings)
    
    reviewscnt <- hotel %>% html_nodes("a.review_count") %>% html_text()
    
    if (length(reviewscnt) == 0)
    {
      Hotel_No_reviews <- c(Hotel_No_reviews,"NA")
    }
    else
    {
      Hotel_No_reviews<- c(Hotel_No_reviews,strsplit(reviewscnt, " ")[[1]][1])
    }
    avg_ratings<- hotel %>% html_nodes("span.ui_bubble_rating")
    
    if (length(avg_ratings) == 0)
    {
      Hotel_avg_ratings <- c(Hotel_avg_ratings,"NA")
    }
    else
    {
      avgrat <- avg_ratings %>% '[['(1) %>% html_attr("alt")
      Hotel_avg_ratings<- c(Hotel_avg_ratings,strsplit(avgrat, " ")[[1]][1])
    }
    
    facilitieslist <- hotel %>% html_nodes(xpath = ".//div[@class='label ']")
    facilities = ""
    z =""
    for(facility in facilitieslist)
    {
      facil <- facility %>% html_text()
      z <- paste(z, facil, sep = ", ")
    }
    if (length(z) > 0) {z <- substr(z,2,nchar(z))} #
    
    special_offers <- hotel %>% html_nodes(xpath = ".//a[@class='taLnk hvrIE6']") %>% html_text()
    
    if(length(special_offers) > 0){z <- paste(z, special_offers, sep = ", ")}
    
    Hotel_facilities<- c(Hotel_facilities,z)
    z =""
    
    hotelpage <- read_html(paste(baseurl,hurl, sep=""))
    addrhotel <- vector()
    addrtemp <- hotelpage %>% html_nodes(xpath = "//span[@class='street-address']")
    
    if(length(addrtemp) > 0)
    {
      addr0 = addrtemp %>% '[['(1) %>% html_text()
      if(!is.null(addr0)){addrhotel <- paste(addrhotel, addr0, sep = ", ")}
    }
    addrtemp <- hotelpage %>% html_nodes(xpath = "//span[@class='locality']")
    
    if(length(addrtemp) > 0)
    {
      addr1 = addrtemp %>% '[['(1) %>% html_text()
      if(!is.null(addr1)){addrhotel <- paste(addrhotel, addr1, sep = ", ")}
    }
    
    addrtemp <- hotelpage %>% html_nodes(xpath = "//span[@class='country-name']")
    
    if(length(addrtemp) > 0)
    {
      addr2 = addrtemp %>% '[['(1) %>% html_text()
      if(!is.null(addr2)){addrhotel <- paste(addrhotel, addr2, sep = "")}
    }
    print("Individual Hotel Search in progress...")
    if (length(addrhotel) > 0) {addrhotel <- substr(addrhotel,2,nchar(addrhotel))}
    Hotel_address <- c(Hotel_address,addrhotel)
    
  }
  print("Creating Data Frame...")
  
  Hotel_urls<-paste("https://www.tripadvisor.in",gsub(" ","",Hotel_urls), sep="")
  
  dfhotels <- data.frame(Hotel_names, Hotel_prices, Hotel_urls, Hotel_savings,Hotel_No_reviews, Hotel_avg_ratings,Hotel_facilities,Hotel_address, stringsAsFactors = F)
  print("Saving Data Frame in CSV ...")
  filename<- paste("Hotels", "_", hotel_name, "_", format(Sys.time(), "%H%M%S"), ".csv", sep = "" )
  write.csv(dfhotels, file = filename,row.names=FALSE)
  print(paste("Saved Data Frame in",filename,". Search completed."))
  
}

checkdates <- function (CheckinDate, CheckoutDate)
{
  CheckinDate <- as.Date(CheckinDate)
  CheckoutDate <- as.Date(CheckoutDate)
  
  diff_in_days <- CheckoutDate - CheckinDate
  sysdate <- Sys.Date()
  
  if(as.integer(diff_in_days) <= 30 && (CheckinDate < CheckoutDate) && (CheckinDate > sysdate) && (CheckoutDate > sysdate))
  {
    return (TRUE)
  }else{
    return (FALSE)
  }
}
getDataforSearch <- function()
{
  
  CityName <- readline("Please enter city name to search: ")
  
  CheckInDate <- readline("Please enter checkin date (in format YYYY-M-D): ")
  
  CheckOutDate <- readline("Please enter checkout date (in format YYYY-M-D): ")
  if(checkdates(CheckInDate, CheckOutDate)){
    startHotelSearch(CityName,CheckInDate,  CheckOutDate)
  }else{
    print("Please check dates, difference should be 30 or less or can not be less than sys dates or out dates can not be less than in date")
  }
  
}

if(interactive()) getDataforSearch()