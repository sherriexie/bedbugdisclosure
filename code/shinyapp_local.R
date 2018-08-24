library(shiny)
library(shinyWidgets)
library(deSolve)
library(dplyr)
library(reshape)
library(ggplot2)

setwd("/Users/sxs/Dropbox/Levy Lab/bb_properties/bb disclosure code/shinyApp/")
# -----------------------------------------------------------------------------
# SetParameters function: 
# ----------------------
# Sets parameter values equal to the point estimates given in Table 1

SetParameters <- function(){
  gamma <- 1/(6*30)
  k <- 0.3
  b <- 1.3
  m <- 1/(2*365)
  n <- 1/(2*30)
  N <- 1000
  D <- 365
  return(c(gamma=gamma, k=k, b=b, m=m, n=n, N=N, D=D))
}

# ----------------------------------------------------------------------------
# SetODEs_IMM function:
# --------------------
# This is the same as the SetODEs function with the addition of INTERMARKET
# MIGRATION, a relaxation of the closed population assumption implicit in the
# original ODEs.
#
# Intermarket migration is represented by the following additional variables:
#    i = immigration rate, i.e. rate at which tenants from external markets 
#         move into units in the market of interest
#    e = external prevalence, i.e. the average infestation rate of units
#         in external markets


SetODEs_IMM<-function(t,y,p){
  Sr <- y[1]
  Ir <- y[2]
  Sv <- y[3]
  Iv <- y[4]
  Sv2 <- y[5]
  trt <- y[6]
  tov <- y[7]
  
  # f(t) = b*Ir/(Sr+b*Ir)
  with(as.list(p),{
    dSr.dt <- -beta*Sr*Ir/N + gamma*Ir + n*( (1-i) * (1-k*b*Ir/(Sr+b*Ir)) + i*(1-k*e) )*Sv + n*(1-d)*( (1-i) * (1-k*b*Ir/(Sr+b*Ir)) + i*(1-k*e) )*Sv2 - m*Sr
    dIr.dt <- beta*Sr*Ir/N + n*k* ((1-i)*b*Ir/(Sr+b*Ir) + i*e)*Sv + n*k* (1-d)*((1-i)*b*Ir/(Sr+b*Ir) + i*e)*Sv2  + n*(1-d)*Iv - (gamma + b*m)*Ir
    dSv.dt <- m*Sr + d/D*Sv2 - n*Sv 
    dIv.dt <- b*m*Ir - gamma*Iv - n*(1-d)*Iv
    dSv2.dt <- gamma*Iv - n*(1-d)*Sv2 - d/D*Sv2
    dtrt.dt <- gamma*Ir + gamma*Iv
    dtov.dt <- n*Sv + n*(1-d)*Sv2 + n*(1-d)*Iv
    return(list(c(dSr.dt, dIr.dt, dSv.dt, dIv.dt, dSv2.dt, dtrt.dt, dtov.dt)))
  })
}

# ----------------------------------------------------------------------------
# GetBeta_IMM function: 
# ----------------
# Same as the GetBeta function but with the addition of terms i and e
GetBeta_IMM <- function(p, bprev, i, e){
  gamma <- p[1]
  k <- p[2]
  b <- p[3]
  m <- p[4]
  n <- p[5]
  N <- p[6]
  i <- i
  e <- e
  
  Ir <- N*bprev*(gamma + n) / (b*m + gamma + n)
  Iv <- N*bprev*b*m / (b*m + gamma + n)
  Sv <- (gamma*Iv + N*m - N*bprev*m) / (m + n)
  Sr <- N - Sv - N*bprev
  f <- b*Ir/(Sr + b*Ir)
  
  beta <- (n*k*((1-i)*f + i*e)*Sv + n*Iv - (gamma + b*m)*Ir)*(-N/(Sr*Ir))
  names(beta) <- "beta"
  return(beta)
}

# ----------------------------------------------------------------------------
# GetInit function
# - This function solves for the initial conditions (Sr0, Ir0, etc.) so that
#   the disclosure simulation starts "at equilibrium" in the ABSENCE of 
#   disclosure
# - Input: p.set = baseline prevalence, migration rate
# - Output: vector of Sr0, Er0, Ir0, etc. 

GetInit <- function(p){
  gamma <- p[1]
  k <- p[2]
  b <- p[3]
  m <- p[4]
  n <- p[5]
  N <- p[6]
  beta <- p[8]
  prev <- p[9]
  
  Ir <- N*prev*(gamma + n) / (b*m + gamma + n)
  Iv <- N*prev*b*m / (b*m + gamma + n)
  Sv <- (gamma*Iv + N*m - N*prev*m)/(m+n)
  Sr <- N - Sv - N*prev
  
  init.par <- c(Sr, Ir, Sv, Iv)
  return(init.par)
}

# ----------------------------------------------------------------------------
# GetCost_IMM function:
# --------------------
# Same as GetCost function with the addition of intermarket migration
GetCost_IMM <- function(p.set, bbcosts, years=1:20, i, e){
  
  # Set parameter values
  gamma <- p.set[1]
  k <- p.set[2]
  b <- p.set[3]
  m <- p.set[4]
  n <- p.set[5]
  N <- p.set[6]
  D <- p.set[7]
  beta <- p.set[8]
  d <- p.set[14]
  i <- i
  e <- e
  
  # Set bed bug related costs (treatment, turnover, and vacancy costs)
  ctrt <- bbcosts[1] 
  ctov <- bbcosts[2]
  cvac <- bbcosts[3]
  
  # Set initial conditions and time interval
  Sr0 <- p.set[10]
  Ir0 <- p.set[11]
  Sv0 <- p.set[12]
  Iv0 <- p.set[13]
  Sv20 <- 0  # Sv20 is set to 0 because we assume disclosure begins at time 0
  trt0 <- 0  # set treatment counter to 0 at time 0
  tov0 <- 0  # set turnover counter to 0 at time 0
  y0 <- c(Sr0, Ir0, Sv0, Iv0, Sv20, trt0, tov0)
  t <- seq(from=0, to=365*20+1, by=1)
  
  # We model the absence of disclosure by setting the disclosure index d = 0
  p <- list(beta=beta, gamma=gamma, b=b, d=d, D=D, k=k, m=m, n=n, N=N, i=i, e=e)
  p0 <- list(beta=beta, gamma=gamma, b=b, d=0, D=D, k=k, m=m, n=n, N=N, i=i, e=e)
  
  # We solve the ODE's for the model in the presence and absence of disclosure 
  # (out and out0, respectively)
  out <- ode(y=y0, times=t, func=SetODEs_IMM, parms=p)
  out0 <- ode(y=y0, times=t, func=SetODEs_IMM, parms=p0)
  
  # We use a for loop to calculate total and component costs for each year of 
  # the simulation
  cost <- trt <- tov <- vac <- prev <- as.numeric()
  
  for(jj in 1:20){
    
    # Get the first and last day of year jj
    first.day <- jj*365 - 364
    last.day <- jj*365 + 1
    
    # Calculate the DIFFERENCE between the disclosure and no disclosure simulations 
    # in the number of treatments (n.trt), turnovers (n.tov), and days vacant (n.vac) 
    # that fell on year jj
    n.trt <- ((out[,7][last.day] - out[,7][first.day]) - 
                (out0[,7][last.day] - out0[,7][first.day]))
    n.tov <- ((out[,8][last.day] - out[,8][first.day]) - 
                (out0[,8][last.day] - out0[,8][first.day]))
    n.vac <- ((sum(out[,4][first.day:last.day]) + sum(out[,5][first.day:last.day]) 
               + sum(out[,6][first.day:last.day])) - 
                (sum(out0[,4][first.day:last.day]) + sum(out0[,5][first.day:last.day]) 
                 + sum(out0[,6][first.day:last.day])))
    
    # Total per unit treatment cost = 
    # (# of treatments) x (avg cost of bed bug treatment) / (total # units)
    trt[jj] <- n.trt*ctrt/N
    
    # Total per unit turnover cost = 
    # (# of turnover events) x (avg cost of turnover) / (total # units)
    tov[jj] <- n.tov*ctov/N
    
    # Total per unit vacancy cost = 
    # (# months vacant) x (average monthly rent) / (total # units)
    # Note: # months = # days / 30
    vac[jj] <- (n.vac/30)*cvac/N
    
    # Total cost is equal to the sum of the component costs
    cost[jj] <- trt[jj] + tov[jj] + vac[jj]
    
    # Prevalence at the end of year jj is simply the number of units in the 
    # Ir and Iv classes on the last day of the year divided by N
    prev[jj] <- (out[,3][last.day] + out[,5][last.day])/N
    
  }
  
  df <- data.frame(Year = years, Total_Cost = cost, Treatment = trt, 
                   Turnover = tov, Vacancy = vac, Prevalence=prev)
  return(df)
}


ui <- navbarPage("Modeling Bed Bug Disclosures",
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
                                     plotOutput("plot1"),
                                     p("The prevalence of bed bug infestations over the first twenty years of disclosure is shown by the solid red line, and the overall cost to landlords (defined as the difference in cost to landlords in the presence of disclosure compared to no disclosure) is shown by the dashed black line. The individual components of cost (due to treatment, turnover, and vacancy) are colored according to the legend on the right."))
                            )
                          )
                 ),
                 
                 tabPanel("Cost Animation",
                          fluidPage(
                              verticalLayout(
                                 plotOutput("plot2", height=500)),
                                wellPanel(
                                sliderInput("y", "Year(s) since implementation of disclosure", 1, 20, 1, step=1, animate=TRUE)
                              )  
                            
                          )
                          
                 ),
                 
                 tabPanel("Prevalence Animation",
                          fluidPage(
                            verticalLayout(
                              plotOutput("plot3", height=500)),
                            
                            wellPanel(
                              sliderInput("y2", "Year(s) since implementation of disclosure", 1, 20, 1, step=1, animate=TRUE)
                            )  
                            
                          )
                          
                 )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    bprev <- input$bp
    di <- input$di
    i <- input$i
    e <- input$e
    
    preparam <- SetParameters()
    beta <- GetBeta_IMM(preparam, bprev, i, e)
    param <- c(preparam, beta, bprev)
    init <- GetInit(param)
    param.init <- c(param, init, d=di)
    cost.df <- GetCost_IMM(p.set = param.init, i = i, e = e,
                           bbcosts = c(input$ctrt, input$ctov, input$cvac))
    cost.df <- rbind(c(0, 0, 0, 0, 0, bprev), cost.df)
    
    # Separate data frame into total cost, component cost, and prevalence.
    # Component costs need to be transformed to long data format.
    totalcost.df <- cost.df[,1:2]
    prev.df <- cost.df[,c(1,6)]
    componentcost.df <- melt(cost.df[,c(1,3,4,5)], id.vars= "Year")
    
    # Convert prevalence so it's expressed in percentages
    prev.df$Prevalence <- prev.df$Prevalence*100
    
    # The following chunk of code finds the best place to position the prevalence
    # curve:
    maxcc <- max(componentcost.df$value) 
    mincc <- min(componentcost.df$value)
    maxprev <- max(prev.df$Prevalence)
    minprev <- min(prev.df$Prevalence)
    range.cost <- maxcc - mincc 
    range.prev <- maxprev - minprev
    mid.cost <- (maxcc + mincc)/2
    mid.prev <- (maxprev + minprev)/2
    m_transform <- range.prev/range.cost
    b_transform <- mid.prev - m_transform*mid.cost
    
    ggplot() +
      geom_bar(data=componentcost.df, stat = "identity", 
               aes(x=Year, y=value, fill= variable)) +
      scale_fill_manual(name="Cost component:", values=c("#1f78b4", "#b2df8a", 
                                                         "#a6cee3")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.title.x=element_text(vjust=-2), axis.ticks.x=element_blank()) +
      theme(plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      theme(plot.title = element_text(size = 20, face="bold", margin=margin(0,0,50,0)),
            axis.text=element_text(size=16),
            axis.title=element_text(size=16,face="bold"),
            legend.text = element_text(size=16),
            legend.title = element_text(size=16)) +
      ggtitle("Cost of Disclosure and Infestation Prevalence, Years 1-20") +
      labs(x="Years since implementation of disclosure", y = "Cost ($)") +
      geom_line(data=totalcost.df,aes(x=Year, y=Total_Cost, color = "black"), 
                color="black", linetype=2, size=1.3) +
      scale_color_discrete(name="", labels="Total Cost") +
      scale_y_continuous(sec.axis = sec_axis(~.*m_transform + b_transform, 
                                             name = "Prevalence (%)")) +
      theme(axis.line.y.right = element_line(color = "firebrick1"),
            axis.ticks.y.right = element_line(color = "firebrick1"),
            axis.title.y.right = element_text(color = "firebrick1"),
            axis.text.y.right = element_text(color = "firebrick1")) +
      geom_line(data=prev.df,aes(x=Year, y=(Prevalence-b_transform)/m_transform, 
                                 color = "firebrick1"), color="firebrick1", linetype=1, size=1.3)      
    
    
    
  })
  
  output$plot2 <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste0("costplot", input$y, ".png")))
    
    list(src = filename, height=500, width=500)
    
  }, deleteFile = FALSE)
  
  output$plot3 <- renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste0("prevplot", input$y2, ".png")))
    
    list(src = filename, height=500, width=500)
    
  }, deleteFile = FALSE)
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)    

