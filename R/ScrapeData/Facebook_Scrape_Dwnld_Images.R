# For RSeleium to work in same machine as browser sits, requires Selenium Server to run locally, 
# please download latest Selenium server jar from - https://goo.gl/rW9Yvk (also included in zip file)
# and run (double click .jar file or run from command prompt java -jar selenium-server-standalone-x.xx.x.jar

#The R code below logs in to facebook with supplied credentials, searches for supplied keyword
#and picks the first or top link from results and goes to its page, navigates to photos and starts to 
#download low res photos form loaded page(usually 28, to download more the page can be scrolled by uncommenting
#the for loop for down key) and then one by one attempts to download one by one 
#high resolution images. 

# The program works better if browser window launched is not minimized.

# The program skips some high resolution photos if they fail to laod in 10 seconds, the sleep time can be 
# increased to download more images
suppressPackageStartupMessages({
  if (!require(rvest)){ install.packages("rvest")}; library(rvest)
})

suppressPackageStartupMessages({
  if (!require(RSelenium)){ install.packages("RSelenium")}; library(RSelenium)
})

suppressPackageStartupMessages({
  if (!require(getPass)){ install.packages("getPass")}; library(getPass)
})
suppressPackageStartupMessages({
  if (!(require(stringr))) {install.packages("stringr");library(stringr)}})

scrapefbforimages <- function(email, pwd, searchtext)
{
  print("Launching browser..")
  require(RSelenium)
  
  remDr <- remoteDriver(browserName = "chrome")
  print("Launched browser")
  remDr$open()
  remDr$maxWindowSize()
  baseurl <- "https://www.facebook.com"
  remDr$navigate(baseurl)
  
  print("Loaded FB..")
  
  Sys.sleep(3)
  
  elem <- remDr$findElement(using ='css', "#email")
  elem$clearElement()  
  Emailid <- email
  elem$sendKeysToElement(list(Emailid))  
  
  elempwd <- remDr$findElement(using ='css', "#pass")
  elempwd$clearElement()  
  Pwd <- pwd
  elempwd$sendKeysToElement(list(Pwd))  
  
  elemlogin <- remDr$findElement(using ='xpath', "//input[@data-testid = 'royal_login_button']")
  
  elemlogin$clickElement()
  
  remDr$click()
  
  print("Login to FB completed..")
  
  Sys.sleep(4) 
  

  elemsearch <- remDr$findElement(using ='css', "._1frb")
  elemsearch$clearElement()  
  
  elemsearch$sendKeysToElement(list(searchtext)) 
  
  elemsearch$sendKeysToElement(list(key = "enter"))
  remDr$click()
  print("Search of keyword started..")
  Sys.sleep(3)
  
  elemSearchPgLink <- remDr$findElement(using ='xpath', "//a[@class = '_1ii5 _2yez']")
  elemSearchPgLink$clickElement()
  
  print("Waiting for searched page link to load..")
  
  Sys.sleep(15)

  
  elemPhotosLink <- read_html (remDr$getPageSource()[[1]])
  
  elemPhotostemp <- elemPhotosLink %>% html_nodes(xpath = "//div[@data-key = 'tab_photos']")
  
  elemPhotosLks <- elemPhotostemp[[1]] %>% html_nodes(xpath = "//a[@class = '_2yau']") 
  #if program fails here, kindly increase sleep time above from 15 as links can take time to load depeding on internet speed
  i<- 1
  eplink <- ""
  for(elemPhotsLk in elemPhotosLks)
  {
    eplink <- elemPhotsLk %>%  html_attr("href")
    if(grepl("photos", eplink, fixed = TRUE))
    {
      break
    }
    i <- i +1
  }
  
  remDr$navigate(paste(baseurl, eplink, sep=""))
  
  Sys.sleep(2)
  
  picspage <- remDr$findElement("css", "body")
  
  picspage$sendKeysToElement(list(key = "page_down"))

  Sys.sleep(1)
  #uncomment code below if you want to download more pictures
  # for(j in 1:5)
  # {
  #   picspage$sendKeysToElement(list(key = "page_down"))
  #   
  #   Sys.sleep(1)
  # }
  
  search_phtos_html<-read_html (remDr$getPageSource()[[1]])
  
  picdivs <- search_phtos_html %>% html_nodes(xpath = "//div[@class=' _2eea']")
  print("Downloading low resolution photos..")
  i <- 1

  lowres_pic_url <- picdivs %>% html_nodes(xpath = "//img[@class='img' and @height='200']") %>% html_attr("src")
  high_pic_url <- picdivs %>% html_nodes(xpath = "//a[@rel='theater']") %>% html_attr("href")
  
  for(lowresurl in lowres_pic_url)
  {
    if(grepl("https", lowresurl, fixed = TRUE))
    {
      download.file(lowresurl ,paste("image_low_res_", i, ".jpg", sep=""), mode = 'wb')
      i <- i + 1
    }
  }
  print("Downloading high resolution photos..")
  i <- 1
  for(highresurl in high_pic_url)
  {
    if(grepl("https", highresurl, fixed = TRUE))
    {
      highresurl <- paste(highresurl, "&theater", sep="")
      remDr$navigate(highresurl)
      Sys.sleep(10) # increase this if you dont want to skip any high resolution photos
      highreshtml <- read_html (remDr$getPageSource()[[1]])
      highresurl <- highreshtml %>% html_nodes(xpath = "//img[@class='spotlight']") %>% html_attr("src")
      if(!identical(highresurl, character(0)))
      {
        download.file(highresurl, paste("image_high_res_", i, ".jpg", sep=""), mode = 'wb')
      }else{
        print("Skipping image as taking too long.")
      }
      i <- i + 1
    }
  }
  print("Download completed Yayyy !!!")
  remDr$close()
}

getLoginandsearchtext <- function()
{
  
  email_id <- readline("Please enter email for login: ")
  
  pwd <- getPass::getPass("Please enter the password: ")
  
  searchtext <- readline("Please enter search text: ")
  
  scrapefbforimages(email_id,pwd,  searchtext)

}

if(interactive()) getLoginandsearchtext()