library(shiny)
library(ggplot2)
library(plotly)
library(dplyr) # group_by, summarize
library(forecast) # arima

## load dataset and convert the data type
pcgFull <- read.csv("./CleanData/pcgFull.csv")
pcgFull$GENDER <- as.factor(pcgFull$GENDER)
pcgFull$FieldOfStudyLabel <- as.factor(pcgFull$FieldOfStudyLabel)
pcgFull$SalaryGroup <- as.factor(pcgFull$SalaryGroup)
pcgFull$JobSatisfaction <- as.factor(pcgFull$JobSatisfaction)
pcgFull$JobTitle <- as.factor(pcgFull$JobTitle)
pcgFull$StemStudy <- as.factor(pcgFull$StemStudy)
pcgFull$StemJob <- as.factor(pcgFull$StemJob)
pcgFull <- pcgFull[,-c(1)]

aerospaceGenderEarn <- read.csv("./CleanData/genderEarn.csv")
aerospaceGenderEarn <- aerospaceGenderEarn[,-c(1,3)]
aerospaceGenderEarn$PUMS_Occupation <- as.factor(aerospaceGenderEarn$PUMS_Occupation)
aerospaceGenderEarn$Sex <- as.factor(aerospaceGenderEarn$Sex)

stemRaceEarn <-read.csv("./CleanData/raceEarn.csv")
stemRaceEarn <- stemRaceEarn[,-c(1)]
stemRaceEarn$Race <- as.factor(stemRaceEarn$Race)
stemRaceEarn$Gender <- as.factor(stemRaceEarn$Gender)

aviationRatio <- read.csv("./CleanData/aviationRatio.csv")
aviationRatio <- aviationRatio[,-c(1)]
aviationRatio$Gender <- as.factor(aviationRatio$Gender)
aviationRatio$Occupation <- as.factor(aviationRatio$Occupation)

aviationLeadership <- read.csv("./CleanData/leadershipRatio.csv")
aviationLeadership <- aviationLeadership[,-c(1)]
aviationLeadership$Gender <- as.factor(aviationLeadership$Gender)
aviationLeadership$LeadershipPosition <- as.factor(aviationLeadership$LeadershipPosition)

pilotYear <- read.csv("./CleanData/aviationYear_ARIMA.csv")
pilotYear$Gender <- as.factor(pilotYear$Gender) 

# create category list for age in pcgFull dataset
pcgFull$AgeLabel <- ifelse(pcgFull$AGE >= 10 & pcgFull$AGE < 30, "Teenager & Twenites",
                              ifelse(pcgFull$AGE >= 30 & pcgFull$AGE < 50, "Thirites & Fourties",
                                     ifelse(pcgFull$AGE >= 50 & pcgFull$AGE < 70, "Fifties & Sixties", "Over Seventies"))) 
# convert data type of new column (AgeLabel)
pcgFull$AgeLabel<- as.factor(pcgFull$AgeLabel)

# Shiny Server
function(input, output, session) {
  
  ## Home
  # return picture in home
  output$home_img <- renderImage(
    {
      list(src = "homeImage.png",
           width = "70%",
           height = "100%")
    }, deleteFile = F
  )
  
  ## Data
  # return the requested dataset
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "pcgFull" = pcgFull,
           "Aerospace Engineer Earn" = aerospaceGenderEarn,
           "Race Earn" = stemRaceEarn,
           "Occupation Ratio" = aviationRatio,
           "Leadership Ratio" = aviationLeadership,
           "Pilot Distributioin Year" = pilotYear)
  }, ignoreNULL = FALSE)
  
  # return the summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # return the requested observations
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })

  ## Analysis
  
  ##### Problem 1
  # problem1 tab1
  
  # filter the ratio of only stem job and all study by gender
  all <- pcgFull %>% 
    group_by(GENDER) %>% 
    filter(StemJob == "Stem") %>% 
    tally() %>% 
    mutate(ratio = round(n/sum(n)*100,2),
           percent = paste(ratio,'%'))
  
  # filter the ratio of only stem job and stem study by gender
  stem_study <- pcgFull %>% 
    group_by(GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Stem") %>% 
    tally() %>% 
    mutate(ratio = round(n/sum(n)*100,2),
           percent = paste(ratio,'%'))
  
  # filter the ratio of only stem job and non stem study by gender
  nonstem_study <- pcgFull %>% 
    group_by(GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Non-Stem") %>% 
    tally() %>% 
    mutate(ratio = round(n/sum(n)*100,2),
           percent = paste(ratio,'%'))
  
  # return the requested dataset
  OptionInput1 <- eventReactive(input$updateP1, {
    switch(input$StemStudy,
           "All" = all,
           "Stem Study" = stem_study,
           "Non-Stem Study" = nonstem_study)
  }, ignoreNULL = FALSE)
  
  # output problem1 tab1
  output$Plot1 <- renderPlot({
    option <- OptionInput1()
    ggplot(option, aes(x='', y=n, fill=GENDER)) +
      geom_bar(stat='identity', width=1, color="white") +
      theme_void() + # remove backgroud, grid, numeric labels
      coord_polar('y', start=0) +
      theme(legend.title = element_text(size=6.5, face='bold'),
            plot.title = element_text(face='bold')) +
      geom_text(aes(label=format(percent,
                                 drop0trailing = TRUE)),
                position=position_stack(vjust=0.5))
  })
  
  # problem1 tab2
  
  yr_genderAll <- pcgFull %>% 
    group_by(Year, GENDER) %>% 
    filter(StemJob == "Stem") %>% 
    tally()
  
  yr_genderStem <- pcgFull %>% 
    group_by(Year, GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Stem") %>% 
    tally()
  
  yr_genderNonStem <- pcgFull %>% 
    group_by(Year, GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Non-Stem") %>% 
    tally()
  
  # return the requested dataset
  OptionInput1.1 <- reactive({
    switch(input$StemYear,
           "yearAll" = yr_genderAll,
           "yearStem" = yr_genderStem,
           "yearNonStem" = yr_genderNonStem)
  })
  
  # output problem1 tab2
  output$Plot1.1 <- renderPlotly({
    option <- OptionInput1.1()
    graph1.1 <- ggplot(option, aes(x = Year, y = n, colour = GENDER)) +
      geom_line() +
      geom_point() +
      theme(plot.title = element_text(face="bold", size=8, vjust=1),
            legend.title = element_text(size=8, face='bold')) +
      labs(x = "Year", y = "Count", fill = "Gender") +
      scale_x_continuous(breaks = c(2003, 2010, 2013, 
                                    2015, 2017, 2019)) +
      theme_bw()
    ggplotly(graph1.1)
  })
  
  ##### Problem 2
  
  yr_wageStem <- pcgFull %>% 
    group_by(Year, GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Stem") %>% 
    summarize(AverageSalary = mean(SALARY), .groups= 'drop')
  
  yr_wageNonStem <- pcgFull %>% 
    group_by(Year, GENDER) %>% 
    filter(StemJob == "Stem" & StemStudy == "Non-Stem") %>% 
    summarize(AverageSalary = mean(SALARY), .groups= 'drop')
  
  # return the requested dataset
  OptionInput2 <- reactive({
    switch(input$StemWageYear,
           "wageYearStem" = yr_wageStem,
           "wageYearNonStem" = yr_wageNonStem)
  })
  
  # output problem2
  output$Plot2 <- renderPlot({
    option <- OptionInput2()
    ggplot(option, aes(x = Year, y = AverageSalary, colour = GENDER)) +
      geom_line() +
      geom_point() +
      theme(plot.title = element_text(face="bold", size=8, vjust=1),
            legend.title = element_text(size=8, face='bold')) +
      labs(x = "Year", y = "Count", fill = "Gender") +
      scale_x_continuous(breaks = c(2003, 2010, 2013, 
                                    2015, 2017, 2019)) +
      theme_bw()
  })
  
  # return the table
  output$viewP2 <- renderTable({
    OptionInput2()
  })
  
  # output problem2.1
  output$Plot2.1 <- renderPlot({
    option <- OptionInput2() %>% 
      group_by(GENDER) %>% 
      summarize(avg = mean(AverageSalary))
    ggplot(option, aes(x = GENDER,y=avg, fill = GENDER)) +
      geom_col() +
      geom_text(aes(label = paste0("$", format(avg))),
                vjust=4) +
      theme_bw()
  })
  
  ##### Problem 3
  # problem 3 tab1
  
  wageGroupGender <- pcgFull %>% 
    rename(x = GENDER)
  
  wageGroupAge <- pcgFull %>% 
    rename(x = AgeLabel)
  
  # return the requested dataset
  OptionInput3 <- eventReactive(input$updateP3, {
    switch(input$wageDifference,
           "Gender" = wageGroupGender,
           "Age" = wageGroupAge)
  }, ignoreNULL = FALSE)
  
  # output problem3 Tab1
  output$Plot3 <- renderPlotly({
    option <- OptionInput3()
    graph3 <- ggplot(option) +
      geom_bar(mapping = aes(x= SalaryGroup, fill=x), position="fill") +
      labs(x = "Group", y = "Ratio (%)", fill = "Gender") +
      scale_x_discrete(limits = c("<50K", ">=50K, <100K",
                                  ">=100K, <150K", ">=150K")) +
      theme_bw()
    ggplotly(graph3)
  })
  
  # Problem 3 tab2
  
  wageAerospaceEn <- aerospaceGenderEarn %>% 
    filter(PUMS_Occupation == "Aerospace engineers") %>% 
    group_by(Sex,Year) %>% 
    summarise(AvgEarning = mean(Average_Wage),
              .groups = 'drop') %>% 
    mutate(x = Sex) %>% 
    rename(Gender = Sex)
  
  wageRace <- stemRaceEarn %>% 
    rename(x = Race) %>% 
    mutate(Gender = ifelse(Gender == "men", "Male", "Female"))
  
  # return the requested dataset
  OptionInput3.1 <- eventReactive(input$updateP3.1, {
    switch(input$wageDifference2,
           "Race" = wageRace,
           "Aerospace Engineer" = wageAerospaceEn)
  }, ignoreNULL = FALSE)
  
  # output problem3 Tab2
  output$Plot3.1 <- renderPlotly({
    option <- OptionInput3.1()
    graph3.1 <- ggplot(data = option, aes(x=x,y=AvgEarning, fill=Gender)) +
      geom_col(position = "dodge") +
      labs(x="") +
      theme(plot.title = element_text(face="bold", size=11, vjust=1),
            legend.title = element_text(size=8, face='bold'),
            axis.text = element_text(size=6)) +
      scale_fill_manual(values=c('Male' = '#0072B2', 'Female' = '#FF9999')) +
      theme_bw()
    ggplotly(graph3.1)
  })
  
  ##### Problem 4
  
  leadershipOpt <- aviationLeadership %>% 
    rename(x = LeadershipPosition)
   
  occupationOpt <- aviationRatio %>% 
    rename(x = Occupation)
  
  # return the requested dataset
  OptionInput4 <- reactive({
    switch(input$positionOption,
           "aviationChoice" = occupationOpt,
           "leadershipChoice" = leadershipOpt)
  })
  
  # output problem4
  output$Plot4 <- renderPlotly({
    option <- OptionInput4()
    graph4 <- ggplot(option, aes(x= x, y=Ratio, fill=Gender)) +
      geom_col(position = "dodge") +
      coord_flip() +
      theme(axis.text = element_text(size=7),
            plot.title = element_text(face="bold", hjust=1, size=9)) +
      scale_fill_manual(values=c('#0072B2','#FF9999')) +
      theme_bw()
    ggplotly(graph4)
  })
  
  ##### Model: ARIMA
  
  # filter the distribution of women by year
  predictFit <- pilotYear %>% 
    filter(Gender == 'Women')
  # convert time series 
  tsdata <- ts(predictFit$Pilot, start = 2005, frequency=1)
  # set best model for predict
  model <- arima(tsdata, order = c(1,2,0))
  
  # output plot of Model
  output$PlotModel <- renderPlot({
    predict <- forecast(model, h=input$predictN)
    plot(predict, col="darkgreen", lwd=2,
         flty=1, flwd=3, fcol="royalblue",
         shadecols=c("mistyrose", "salmon"),
         xlab="Year", ylab="Flow", main="")
    # return the requested observations
    output$viewPredict <- renderTable({
      predict
    })
  })
}
