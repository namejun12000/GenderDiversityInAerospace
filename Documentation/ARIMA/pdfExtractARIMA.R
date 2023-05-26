# import library
library(pdftools)
# unzip file
zipFile =  unzip("Data/Aerospace.zip")
# Women in Aviation  A Workforce Report 2021 Edition.pdf
pdfFile1 <- zipFile[19]

# function of extracting text in pdf (Women in aviation by year 2005-2020)
arimaData <- function(pdfFile)
{
  # data frame per page (one row for each textbox)
  data <- pdf_data(pdfFile1)
  
  # set page 11
  page <- data[[11]]

  # extract only required text and list of each items
  # year
  year <- page$text[c(46,46,45,45,44,44,43,43,42,42,41,41,40,40,39,39,
                      38,38,37,37,36,36,35,35,34,34,33,33,32,32,31,31)]
  # gender
  gender <- page$text[c(48,47,48,47,48,47,48,47,48,47,48,47,48,47,48,47,
                        48,47,48,47,48,47,48,47,48,47,48,47,48,47,48,47)]
  # pilot 
  pilot <- page$text[c(84,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,
                       64,63,62,61,60,59,58,57,56,55,54,53,52,51,49)]

  # create data frame
  aviationYear <- data.frame(year, gender, pilot)
  # convert data type
  aviationYear$gender <- as.factor(aviationYear$gender)
  aviationYear$pilot<- gsub(",", "", aviationYear$pilot)     
  aviationYear$pilot <- as.numeric(aviationYear$pilot)
  
  # change column names
  names(aviationYear) <- c("Year", "Gender", "Pilot")
  # create csv file and saving
  dfYear <- write.csv(aviationYear, "Final Product/ShinyApp/CleanData/aviationYear_ARIMA.csv", row.names=FALSE)
}

dt <- arimaData(pdfFile1)


