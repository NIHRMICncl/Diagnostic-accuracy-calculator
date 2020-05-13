#################### ui for ShinyApp to calculate diagnostic accuracy measures ###############################

ui <- function(request) {

  navbarPage(h5(""),
   tabPanel("About", includeHTML("www/About.html")),
   tabPanel("Calculator",
               includeHTML("www/Calculator.html"),
            hr(),
               sidebarPanel(
                 fluidRow(
                   radioButtons("studyDesign", "Design of study providing data", choices = c("Case-control/don't know", "cohort"),
                                selected = NA),
                   radioButtons("dataType", "Type of test accuracy data", choices = c("sensitivity & specificity", "TP, FP, FN, TN"),
                                selected = NA),
                   hr(),
                   uiOutput("dataEntryTitle"),
                   uiOutput("dataEntry2x2"), # ? data for 2x2 table
                   uiOutput("dataEntrySS"), # ? sensitivity, specificity data
                   uiOutput("dataEntryPrevN"), # ? prevalence and study size if needed
                   checkboxInput("sorted", label = "Population sorted by presence of condition and test result",
                                     value = FALSE),
                   checkboxInput("ciFlag", label = "Show 95% confidence intervals (when sorted)", value = FALSE),
                   textInput("customTableTitle", "Optional custom title for table", value = ""),
                   textInput("customPlotTitle", "Optional custom title for plot", value = ""),
                   actionButton("GoButton", "Update the graphs and tables", class = 'middleAlign'),
                   p(),
                   bookmarkButton("Get a link to return to the currenty set of input data")
                 )),
               mainPanel(
                 span(textOutput("validtext"), style="color:red; font-style:italic"),
                 span(textOutput("table1title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                 textOutput("customTableTitle"),
                 tableOutput("dx2x2Table"),
                 tableOutput("dxStatsTable"),
                 hr(),
                 span(textOutput("predValuesPlotTitle"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                 textOutput("customPlotTitle"),
                 withSpinner(plotlyOutput("predValuesPlot")),
                 hr(),
                 span(textOutput("plot1title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                  withSpinner(plotOutput("populationPlot")),
                 hr(),
                  span(textOutput("plot2title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                  withSpinner(plotOutput("testedPlots"))
                  # withSpinner(plotOutput("distributionplots"))
                 
       # # would prefer for this to be in tabs.
       # "test",
       # tabPanel("Population", plotOutput("populationPlot"), plotOutput("testedPlots")),
       # tabPanel("Distributions", plotOutput("distributionplots"))
  
               )
    ),

    tabPanel("Data and statistics",
      column(12, align="center",
       # span(textOutput("table1title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
           # tableOutput("dx2x2Table"),
       span(textOutput("table2title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
           tableOutput("pvdf")
             )
             ),
    
  tabPanel("Distributions",
            span(textOutput("plot3title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 18px"),
             withSpinner(plotOutput("distributionplots")),
            textOutput("distributiontext")
           ),
 
    tabPanel("Download summary report",
             p("This document contains all the tables and figures generated from your input data."),
             radioButtons('format', 'Please select the document format you require',
                          c('PDF', 'HTML', 'Word'),
                          inline = TRUE),
             downloadButton('downloadReport', 'Download summary report'),
             br(), br(),
             p("NB generating the document can take some time.")
    ),


# 
# ###################################
# #
# #     credits as a running footer
# #
      tags$br(),
      tags$b("Cite as:"),
      tags$p("Joy Allen, Sara Graziadio and Michael Power."),
      tags$em("A Shiny Tool to explore prevalence, sensitivity, and specificity on Tp, Fp, Fn, and Tn"),
      tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
      tags$br(),
      tags$img(src = "nihr-logo.jpg", width = "80px", height = "28px", align = "right") # add the NIHR logo)
)
  }
