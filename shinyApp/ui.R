library(shiny)
library(shinyWidgets)


navbarPage("Modeling Bed Bug Disclosure",
           tabPanel("Main Page", 
                    fluidPage(
                      fluidRow(
                        column(3,
                               wellPanel(
                                 setSliderColor(c("#b2df8a", "#b2df8a", "#b2df8a"), sliderId=c(1,2,3)),
                                 h4(div(HTML("<em>Set costs...</em>"))),
                                 sliderInput("ctrt", "Treatment cost", 50, 
                                             2000, 1225, step=20, pre = "$"),
                                 sliderInput("ctov", "Turnover cost", 50, 2000, 1000, 
                                             step=50, pre="$"),
                                 sliderInput("cvac", "Monthly rent",
                                             50, 2000, 1000, step=50, pre="$"),
                                 hr(),
                                 h4(div(HTML("<em>Set parameter values...</em>"))),
                                 sliderInput("bp", div(HTML("Baseline prevalence (<em>p</em>)")),
                                             0, 0.2, 0.05, step=0.01),
                                 sliderInput("di", div(HTML("Renter selectivity (<em>s</em>)")), 
                                             0, 1, 0.5, step=0.1),
                                 sliderInput("i", div(HTML("Immigrant fraction (<em>i</em>)")),
                                             0, 0.4, 0, step=0.02),
                                 sliderInput("e", div(HTML("External bed bug prevalence (<em>e</em>)")), 
                                             0, 0.2, 0.05, step=0.01)
                               )
                        ),
                        column(9,
                               plotOutput("plot1", height=600),
                               br(),
                               br(),
                               p("The prevalence of bed bug infestations over the first twenty years of disclosure is shown by the solid red line, and the overall cost to landlords due to disclosure (defined as the difference in cost to landlords in the presence of disclosure compared to no disclosure) is shown by the dashed black line. The individual components of cost (due to treatment, turnover, and vacancy) are colored according to the legend on the right."))
                      )
                    )
           ),
           
           tabPanel("Cost Animation",
                    fluidPage(
                      verticalLayout(
                        plotOutput("plot2", height=500)),
                      wellPanel(
                        sliderInput("y", div(HTML("Year(s) since implementation of disclosure (<em>click blue button below to play animation</em>)")), 1, 20, 1, step=1, animate=TRUE)
                      )  
                      
                    )
                    
           ),
           
           tabPanel("Prevalence Animation",
                    fluidPage(
                      verticalLayout(
                        plotOutput("plot3", height=500)),
                      
                      wellPanel(
                        sliderInput("y2", div(HTML("Year(s) since implementation of disclosure (<em>click blue button below to play animation</em>)")), 1, 20, 1, step=1, animate=TRUE)
                      )  
                      
                    )
                    
           ),
           
           tabPanel("Code",
                    fluidPage(
                      br(),
                      p("Rscript used to make this R Shiny web application, along with all scripts used in the bed bug disclosure manuscript, is available at:"),
                      tags$a(href="http://github.com/sherriexie/bedbugdisclosure", "github.com/sherriexie/bedbugdisclosure")
                    
                    ))
)