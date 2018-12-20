library(shiny)
library(shinyWidgets)


navbarPage("Modeling Bed Bug Disclosures",
           tabPanel("Cost to Landlords",
                    fluidPage(
                      fluidRow(
                        column(3,
                               wellPanel(
                                 setSliderColor(c(rep("#b2df8a", 3), rep("", 7), rep("#b2df8a", 3)), 
                                                sliderId=1:13),
                                 
                                 #setSliderColor(c("#b2df8a", "#b2df8a", "#b2df8a"), sliderId=c(4,5,6)),
                                 h4(div(HTML("<em>Set landlord costs...</em>"))),
                                 sliderInput("ctrt", "Treatment cost", 0, 
                                             3000, 1225, step=50, pre = "$"),
                                 sliderInput("ctov", "Turnover cost", 0, 3000, 1000, 
                                             step=50, pre="$"),
                                 sliderInput("cvac", "Monthly rent",
                                             0, 3000, 1000, step=50, pre="$"),
                                 hr(),
                                 h4(div(HTML("<em>Set parameter values...</em>"))),
                                 sliderInput("bp", div(HTML("Baseline prevalence (<em>p</em>)")),
                                             0, 0.2, 0.05, step=0.01),
                                 sliderInput("di", div(HTML("Renter selectivity (<em>s</em>)")), 
                                             0, 1, 0.5, step=0.1),
                                 sliderInput("l", div(HTML("Average no. of months infested (1/γ)")), 
                                             1, 18, 6, step=1),
                                 sliderInput("k", div(HTML("Prob. of relocation transmission (<em>k</em>)")), 
                                             0, 1, 0.3, step=0.05),
                                 sliderInput("b", div(HTML("Vacancy multiplier (<em>b</em>)")), 
                                             1, 3, 1.3, step=0.1),
                                 sliderInput("i", div(HTML("Immigrant fraction (<em>i</em>)")),
                                             0, 0.4, 0, step=0.02),
                                 sliderInput("e", div(HTML("External bed bug prevalence (<em>e</em>)")), 
                                             0, 0.2, 0.05, step=0.01),
                                 actionButton("reset", "Reset all")
                                 
                               )
                        ),
                        column(9,
                               h3("Projected Financial Impact of Disclosure on Landlords, Years 1-20"),
                               plotOutput("plot1"),
                               p(div(HTML("<b>*Note: negative cost values (total and component costs that fall below $0) indicate a cost <em>savings</em> in that component for that year.</b>"))),
                               p(HTML("<b>Figure description:</b> The prevalence of bed bug infestations over the first twenty years of disclosure is shown by the solid red line, and the overall cost to landlords (defined as the difference in cost to landlords in the presence of disclosure compared to no disclosure) is shown by the dashed black line. The individual components of cost (due to treatment, turnover, and vacancy) are shown as bars representing averages over one-year periods and colored according to the legend on the right.")),
                               p(HTML("<b>User instructions:</b> Parameter values used to simulate disclosure costs to landlords can be specified through the sliders located in the left-hand panel. Default slider values are equal to the estimates given in Table 1 of the bed bug disclosure manuscript and yields results matching Figure 2. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel."))
                        )
                      )
                    )
           ),
           
           tabPanel("Cost to Renters", 
                    fluidPage(
                      fluidRow(
                        column(3,
                               wellPanel(
                                 h4(div(HTML("<em>Set renter costs...</em>"))),
                                 sliderInput("canc", "Ancillary treatment cost", 0, 
                                             3000, 800, step=50, pre = "$"),
                                 sliderInput("cmov", "Moving cost", 0, 3000, 400, 
                                             step=50, pre="$"),
                                 sliderInput("cinf", "Monthly cost of untreated infestation",
                                             0, 1000, 200, step=50, pre="$"),
                                 hr(),
                                 h4(div(HTML("<em>Set parameter values...</em>"))),
                                 sliderInput("bp2", div(HTML("Baseline prevalence (<em>p</em>)")),
                                             0, 0.2, 0.05, step=0.01),
                                 sliderInput("di2", div(HTML("Renter selectivity (<em>s</em>)")), 
                                             0, 1, 0.5, step=0.1),
                                 sliderInput("l2", div(HTML("Average no. of months infested (1/γ)")), 
                                             1, 18, 6, step=1),
                                 sliderInput("k2", div(HTML("Prob. of relocation transmission (<em>k</em>)")), 
                                             0, 1, 0.3, step=0.05),
                                 sliderInput("b2", div(HTML("Vacancy multiplier (<em>b</em>)")), 
                                             1, 3, 1.3, step=0.1),
                                 sliderInput("i2", div(HTML("Immigrant fraction (<em>i</em>)")),
                                             0, 0.4, 0, step=0.02),
                                 sliderInput("e2", div(HTML("External bed bug prevalence (<em>e</em>)")), 
                                             0, 0.2, 0.05, step=0.01),
                                 actionButton("reset2", "Reset all")
                                 
                               )
                        ),
                        column(9,
                               h3("Projected Financial Impact of Disclosure on Renters, Years 1-20"),
                               plotOutput("plot2"),
                               br(),
                               p(HTML("<b>Figure description:</b> The total reduction in bed bug prevalence from its baseline value is shown by the solid red line, and the overall financial impact of disclosure on renters (defined as the difference in cost to renters in the presence of disclosure compared to no disclosure) is shown by the dashed black line. Note that because cost to renters is lower in the presence of disclosure than in the absence of disclosure for all parameter combinations, financial impact is depicted as savings (i.e. negative costs). The individual components of savings (from decreased ancillary treatment costs, moving costs, and costs of untreated infestation) are shown as bars representing averages over one-year periods and colored according to the legend on the right.")),
                               p(HTML("<b>User instructions:</b> Parameter values used to simulate the financial impact of disclosure on renters can be specified through the sliders located in the left-hand panel. Default slider values are equal to the estimates given in Table 1 and the Supplementary Text and yields results matching Figure S7. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel."))
                        )
                        
                      )
                    )
           ),
           
           navbarMenu("Animations",
                      tabPanel("Cost to Landlords",
                               fluidPage(
                                 fluidRow(
                                   column(6,
                                          plotOutput("plot3", height=500),
                                          wellPanel(
                                            sliderInput("y", "Year(s) since implementation of disclosure", 1, 20, 1, step=1, animate=TRUE)
                                            
                                          )
                                   ),
                                   column(6,
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          h5("Description"),
                                          p(HTML("The projected per-unit cost of disclosure to landlords is presented as a function of baseline prevalence (<em>p</em>) and renter selectivity (<em>s</em>) for 1-20 years after the implementation of a disclosure policy. Cost is calculated as the sum of the total cost in the population due to vacancy, treatment, and tenant turnover, for the given 1-year interval, averaged over the total rental units. Red indicates parameter combinations where cost to landlords is higher due to disclosure, whereas blue indicates situations where costs have decreased from baseline (savings). Baseline prevalence (<em>p</em>) ranges from 0.1 to 10% and renter selectivity (<em>s</em>) ranges from 0.01 to 1. <b>All other parameter values are set equal to estimates given in Table 1 of the bed bug disclosure manuscript.</b>")),
                                          br(),
                                          h5("User instructions"),
                                          p(HTML("To play the animation, click on the small, blue button located on the bottom right corner of the slider panel. You can also select single-year frames by moving the slider to the desired year."))
                                   )
                                 )
                               )),
                      
                      tabPanel("Cost to Renters",
                               fluidPage(
                                 fluidRow(
                                   column(6,
                                          plotOutput("plot4", height=500),
                                          wellPanel(
                                            sliderInput("y2", "Year(s) since implementation of disclosure", 1, 20, 1, step=1, animate=TRUE)
                                            
                                          )
                                   ),
                                   column(6,
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          h5("Description"),
                                          p(HTML("The per-unit savings due to disclosure that are projected for renters are presented as a function of baseline prevalence and renter selectivity for 1-20 years after the implementation of a disclosure policy. Cost to renters was calculated as the difference in cost in the presence of disclosure compared to no disclosure, and the savings depicted reflect the fact that cost to renters is lower in the presence of disclosure for all parameter combinations and time-points. Darker blue colors represent parameter combinations and time-points where savings to renters due to disclosure are highest. Baseline prevalence (<em>p</em>) ranges from 0.1 to 10% and renter selectivity (<em>s</em>) ranges from 0.01 to 1. <b>All other parameter values are set equal to estimates given in Table 1 of the bed bug disclosure manuscript.</b>")),
                                          br(),
                                          h5("User instructions"),
                                          p(HTML("To play the animation, click on the small, blue button located on the bottom right corner of the slider panel. You can also select single-year frames by moving the slider to the desired year."))
                                   )
                                 ))),
                      
                      tabPanel("Prevalence",
                               fluidPage(
                                 fluidRow(
                                   column(6,
                                          plotOutput("plot5", height=500),
                                          wellPanel(
                                            sliderInput("y3", "Year(s) since implementation of disclosure", 1, 20, 1, step=1, animate=TRUE)
                                            
                                          )
                                   ),
                                   column(6,
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          h5("Description"),
                                          p(HTML("The prevalence of bed bug infestations is presented as a function of baseline prevalence (<em>p</em>) and renter selectivity (<em>s</em>) for 1-20 years after the implementation of a disclosure policy. The origin is the bottom vertex farthest to the right; therefore, the baseline case of no disclosure is represented by the line of slope 1 in the back face on the right. Baseline prevalence (<em>p</em>) ranges from 0.1 to 10% and renter selectivity (<em>s</em>) ranges from 0.01 to 1. <b>All other parameter values are set equal to estimates given in Table 1 of the bed bug disclosure manuscript.</b>")),
                                          br(),
                                          h5("User instructions"),
                                          p(HTML("To play the animation, click on the small, blue button located on the bottom right corner of the slider panel. Single-year frames can also be selected by moving the slider to the desired year."))
                                   )
                                 )
                               ))
           ),
           tabPanel("Code",
                    fluidPage(
                      br(),
                      uiOutput("tab")
                      
                    ))
           
)