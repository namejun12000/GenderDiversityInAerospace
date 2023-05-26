library(shiny)
library(shinythemes) 
library(shinycssloaders) # withSpinner()
library(shinydashboard) # sidebar menu
library(plotly)

# Shiny UI
navbarPage(
  # Theme of Shiny app
  theme = shinytheme("sandstone"),
  title = ("Team 11"),
  # Home Tab
  tabPanel("Home",
           h1("Gender and Diversity in the Aerospace Industry",style = "font-weight: bold; color: darkblue;"),
           withSpinner(imageOutput("home_img")),
           br(),
           h4("The Shiny App is currently engaged in a project that aims to delve 
              into the topic of gender and diversity in the aerospace industry. 
              The primary objective of this project is to draw attention to the gender 
              distribution rates across various genders in the aerospace field, 
              particularly focusing on differences between males, females,
              and other genders."),
           br(),
           h4("By thoroughly analyzing and examining the trends and patterns 
              between men and women in the aerospace industry, the project 
              strives to provide valuable insights to companies operating in 
              STEM-related fields. These insights can assist companies in 
              understanding the existing gender disparities and taking proactive
              measures to ensure equal employment opportunities without any gender
              or race biases. Overall, the project is intended to contribute to
              the development of a more inclusive and diverse workforce
              within the aerospace industry."),
           br(),
           h4("Key business results:", style = "font-weight: bold;"),
           h4("1. Ratio of women to men in aerospace"),
           h4("2. Difference between men and women graduating from STEM fields in aerospace"),
           h4("3. Difference in how women and men are treated in aerospace (including others)"),
           h4("4. Workplace productivity change based on the ratio or treatment of women within the workplace"),
           h4("5. Predictions and projections of the number of female employees in the Aerospace industry (small subset: Pilot) in the next years.")
           ),
  
  # Data Tab
  # summarize of data
  navbarMenu("Data",
             
      tabPanel("Data Info.",
               mainPanel(
                 h2("Information of Datasets",style = "font-weight: bold;"),
                 br(),
                 h4("A total of 6 datasets will be used (Data 3 to 6 are
                    CSV files listing values obtained by extracting specific words from PDF documents):"),
                 br(),
                 h4("Data 1: pcgFull",style = "font-weight: bold;"),
                 h5("Survey data set for STEM field workers or non-STEM field workers"),
                 br(),
                 h4("Data 2: Aerospace Engineer Earn",style = "font-weight: bold;"),
                 h5("Salary for gender aerospace engineers"),
                 br(),
                 h4("Data 3: Race Earn",style = "font-weight: bold;"),
                 h5("Average earnings by race and gender in the stem fields"),
                 br(),
                 h4("Data 4: Occupation Ratio",style = "font-weight: bold;"),
                 h5("Occupation position ratio by gender in the aerospace industry"),
                 br(),
                 h4("Data 5: Leadership Ratio",style = "font-weight: bold;"),  
                 h5("Leadership position ratio by gender in the aerospace industry"),
                 br(),
                 h4("Data 6: Pilot Distribution Year",style = "font-weight: bold;"),
                 h5("Aircraft pilot gender by year")
               )
               ),
      
      tabPanel("Summary & Observations",
               # Sidebar layout with input and output definitions
               sidebarLayout(
                # Sidebar panel for inputs
                 sidebarPanel
                 (
                   # Input: Selector for choosing dataset
                   selectInput(inputId= "dataset",
                               label = "Choose a dataset:",
                               choices = c("pcgFull",
                                           "Aerospace Engineer Earn",
                                           "Race Earn",
                                           "Occupation Ratio",
                                           "Leadership Ratio",
                                           "Pilot Distribution Year")),
                   
                   # Input: Numeric entry for number of obs to view
                   numericInput(inputId = "obs",
                                label = "View number of observations:",
                                value = 5),
                   actionButton("update", "Update View"),
                   helpText("Note: You need to click the button to apply the updated option values."),
                   width=3
                 ),
                 # Main panel for displaying outputs
                 mainPanel(
                   # Output: Verbatim text for data summary
                   h4("Summary"),
                   withSpinner(verbatimTextOutput("summary")),
                   
                   # Output: HTML table with requested number of observation
                   h4("Observations"),
                   tableOutput("view"),
                   width = 7
                 )
               )
               ) 
      ),
  
  # Analysis Tab
  navbarMenu("Analysis",
             tabPanel("Gender Distribution",
                      sidebarLayout(
                        sidebarPanel(
                          #######
                          h4("TAB 1"),
                          selectInput(inputId= "StemStudy",
                                      label = "Choose a field of study:",
                                      choices = c("All","Stem Study","Non-Stem Study")),
                          actionButton("updateP1", "Update"),
                          helpText("Note: You need to click the button to apply the updated option values."),
                          br(), br(),# extra vertical spacing
                          #########
                          h4("TAB 2"),
                          radioButtons(inputId="StemYear", 
                                       label = "Choose a field of study:",
                                       choices = c("All" ="yearAll",
                                                   "Stem Study" = "yearStem",
                                                   "Non-Stem Study" = "yearNonStem"))
                        ),
                        mainPanel
                        (
                          tabsetPanel(
                            tabPanel
                            (
                              "Tab1",
                              h4("Gender distribution in STEM"),
                              withSpinner(plotOutput("Plot1"))
                            ),
                            tabPanel
                            (
                              "Tab2",
                              h4("Gender distribution of STEM by year"),
                              withSpinner(plotlyOutput("Plot1.1"))
                            )
                          )
                          )
                      )
                      ),
             tabPanel("WAGE: GRADUATING STEM FIELD",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId="StemWageYear", 
                                       label = "Choose an Option of graduating:",
                                       choices = c(
                                                   "Stem" = "wageYearStem",
                                                   "Non-Stem" = "wageYearNonStem")),
                          width=3
                        ),
                        mainPanel
                        (
                          h4("Average annual wages after graduation by year"),
                          splitLayout(
                          withSpinner(plotOutput("Plot2", height="400px",
                                                 width = "450px")),
                          tableOutput("viewP2")),
                          h4("Average annual wages by gender"),
                          withSpinner(plotOutput("Plot2.1"))
                          )
                        )
                      ),
             tabPanel("WAGE: OTHERS",
                      sidebarLayout(
                        sidebarPanel(
                          h4("TAB 1: Gender ratio or age ratio by wage group"),
                          selectInput(inputId= "wageDifference",
                                      label = "Choose an Option:",
                                      choices = c("Gender", "Age")),
                          actionButton("updateP3", "Update"),
                          br(),br(),
                          h4("Tab 2: Aerospace wages by race or gender"),
                          selectInput(inputId = "wageDifference2",
                                      label = "Choose an Option:",
                                      choices = c("Race", "Aerospace Engineer")),
                          actionButton("updateP3.1", "Update"),
                          br(), br(),
                          helpText("Note: You need to click the button to apply the updated option values.")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                              "Tab1",
                              withSpinner(plotlyOutput("Plot3", height="600px",
                                                       width = "600px"))
                            ),
                            tabPanel(
                              "Tab2",
                              withSpinner(plotlyOutput("Plot3.1", height="600px",
                                                       width = "600px"))
                            )
                          )
                        )
                        )),
             tabPanel("Position",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId="positionOption", 
                                       label = "Choose a Position:",
                                       choices = c(
                                         "Occupation" = "aviationChoice",
                                         "Leadership" = "leadershipChoice"))
                        ),
                        mainPanel(
                          h4("Gender position ratio"),
                          withSpinner(plotlyOutput("Plot4", height="600px",
                                                     width = "600px"))
                        )
                      ))
             ),
  
  # Model Tab
  navbarMenu("Model",
             tabPanel("ARIMA (Predict)",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("predictN", "Year to predict:",
                                      min = 5, max = 50, value = 10, step = 5),
                          width = "100%"
                        ),
                        mainPanel(
                          h4("Predicting the distribution of Women pilots in Aerospace"),
                          withSpinner(plotOutput("PlotModel")),
                          tableOutput("viewPredict")
                        )
                      ))
             )
)
