suppressPackageStartupMessages({
  if (!require(stringdist)){ install.packages("stringdist")}; library(stringdist)
})
suppressPackageStartupMessages({
if (!(require(tesseract))) {install.packages("tesseract");library(tesseract)}})
suppressPackageStartupMessages({
if (!(require(stringr))) {install.packages("stringr");library(stringr)}})
suppressPackageStartupMessages({
  if (!(require(magick))) {install.packages("magick");library(magick)}})

levenshtienmatch <- function(arrNames, strPassName)
{
  strNametoReturn = ""
  for(i in 1:length(arrNames[[1]])){
    if(str_detect(strPassName," ")){
      arrpassname <- strsplit(strPassName, " ", fixed = T)
      for(j in 1:length(arrpassname[[1]])){
        if(!is.na(amatch(toupper(arrNames[[1]][i]),arrpassname[[1]][j], maxDist=2 )))
        {
          strNametoReturn <- paste(strNametoReturn,arrNames[[1]][i], sep =" " )
        }
      }
      
    }else{
      if(!is.na(amatch(toupper(arrNames[[1]][i]),strPassName, maxDist=2 ))){
        strNametoReturn <- arrNames[[1]][i]
      }
    }
   
  }
  return(str_trim(strNametoReturn))
}

CID <- vector()
Given_Name <- vector()
Surname <- vector()
GivenNamebefLav <- vector()
SurnamebefLav <- vector()
file.names <- dir(getwd())
for(i in 1:length(file.names))
{
  
  arrFileNames <- strsplit(file.names[i], "_")
  
  arrNamesSplit <- strsplit(arrFileNames[[1]][1], " ",fixed = TRUE)
  
  strCID <- arrFileNames[[1]][3]
  strCID <- strsplit(strCID, ".",fixed = TRUE)[[1]][1]
  CID <- c(CID, strCID)   
  
  image <- image_read(file.names[i])
  #print(file.names[i])
  imagecropstring <- paste(image_info(image)[[2]], "x",image_info(image)[[3]] ,"+0+",round((as.integer(image_info(image)[[3]])-115),digits = 0), sep="" )
  #print(imagecropstring)
  Given_Name <- vector()
  Surname <- vector()
  Passport.text <- ocr (image_crop(image, imagecropstring)) 
  # print(Passport.text)
  # print("Now going in loop")
  for(j in 1:3)
  {
    if(grepl("\nP", Passport.text, fixed = TRUE) || grepl("\nF", Passport.text, fixed = TRUE) || grepl("\nB", Passport.text, fixed = TRUE)){
      break
    }else if(!grepl("\nP", Passport.text, fixed = TRUE) || !grepl("\nF", Passport.text, fixed = TRUE) || !grepl("\nB", Passport.text, fixed = TRUE))
    {
      image <- image_rotate(image, 90)
      imagecropstring <- paste(image_info(image)[[2]], "x",image_info(image)[[3]] ,"+0+",round((as.integer(image_info(image)[[3]])-115),digits = 0), sep="" )
      Passport.text <- ocr (image_crop(image, imagecropstring)) 
      #print(imagecropstring)
      # print(j)
      # print(Passport.text)
    }
    
  }
  #print(Passport.text)
  Passport.text <- str_replace(Passport.text, "\nF", "\nP")
  Passport.text <- str_replace(Passport.text, "\nB", "\nP")
  arrFileNames <- strsplit(Passport.text, "\nP", fixed=TRUE)

  arrFileNames <- strsplit(arrFileNames[[1]][2], "<<", fixed=TRUE)

  strGivenName <- arrFileNames[[1]][2]

  strGivenName <- gsub("<"," ",strGivenName)

  strSurName <-arrFileNames[[1]][1]

  strSurName <- substr(strSurName, 5, nchar(strSurName))

  strSurName <- gsub("<"," ",strSurName)
  # print(strGivenName)
  # print(strSurName)
  GivenNamebefLav <- c(GivenNamebefLav, strGivenName)
  SurnamebefLav <- c(SurnamebefLav, strSurName)

  if(!is.na(strGivenName))
  {
     strGivenName <- levenshtienmatch(arrNamesSplit, toupper(strGivenName))
  }
  if(!is.na(strSurName))
  {
    strSurName <- levenshtienmatch(arrNamesSplit, toupper(strSurName))
  }

  Given_Name <- c(Given_Name, strGivenName)

  Surname <- c(Surname, strSurName)

  
}
df_names_Bef_lav <-  data.frame(CID, GivenNamebefLav, SurnamebefLav, stringsAsFactors = F)
df_names_after_lav <-  data.frame(CID, Given_Name, Surname, stringsAsFactors = F)

filename<- paste("NamesFromPassports", "_", format(Sys.time(), "%H%M%S"), ".csv", sep = "" )
write.csv(df_names_after_lav, file = filename,row.names=FALSE)
#setwd("C:/Users/XXXX/Documents/Passport_scans")
