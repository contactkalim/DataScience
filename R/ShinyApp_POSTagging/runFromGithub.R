#As the shiny app also contains udpipe english file, it takes time to download and run
# specially over slow internet, please wait after running for a while :-)
if (!require(shiny)){install.packages("shiny")}
library(shiny)
runGitHub("ShinyApp_POSTagging", "contactkalim") 
