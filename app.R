###################################################

library(shiny)
library(shinyjs)
library(ggplot2)

###### Model parameters ############################################################################
#prod <- runif(1,min=2.,max=10.)
#cap <- runif(1,min=10000.,max=500000.)
sigR <<- 0.5
sigE <<- 0.2
slopeE <<- 0.1

# derived functions of parameters
#true_Smsy <- cap*(1/sqrt(prod)-1/prod)
#true_msy <- prod*true_Smsy/(1+prod*true_Smsy/cap)-true_Smsy

# indices and arrays
nyears_init <<- 10
nyears_update <<- 3
niters <<- 5
totyears <<- nyears_init+niters*nyears_update

returns <- array(dim=totyears)
simcatch <- array(dim=totyears)
spawners <- array(dim=totyears)
#spawners[1] <- cap*runif(1)

Egoal <- array(dim=(niters+1))
#Egoal[1] <- true_Smsy
#Egoal[1:(niters+1)] <- cap*runif(1)



####################################################################################################


########################### USER INTERFACE ###########################################################
ui <- fluidPage(
  
  titlePanel("Set Escapement Goals"),
  
  #Sidebar Layout 
  sidebarLayout(
    
    #Sidebar panel
    sidebarPanel(
      actionButton("restart","Start new game?"),
      br(),br(),
      numericInput("egoal","New escapement goal?",value=2000),
      actionButton("new_goal","Update escapement goal?"),
      h3(textOutput("simEnd")),
      h4(textOutput("trueSmsy")),
      h4(textOutput("expC")),
      h4(textOutput("avgC")),
      h4(textOutput("hfreq")),
      h4(textOutput("efreq")),
      h3(textOutput("score")),
      br(),
      plotOutput("RvS",height=375,width=375)
      
    ), #sidebarPanel
    
    #Main panel
    mainPanel(
#      h2("Historical data"),
#      br(),br(),
      plotOutput("time",height=200),
      plotOutput("catch",height=200)
      
    ) #mainPanel  
    
  )# sidebarLayout
  
  
) #fluidPage
#########################################################################################################

##################### SERVER ############################################################################
server <- function(input,output) {
 
  ##  start a simulation ## 
  observeEvent(input$restart, {

    #cat("restart","\n")
  
    output$simEnd <- renderText("")
    output$trueSmsy <- renderText("")
    output$expC <- renderText("")
    output$avgC <- renderText("")
    output$score <- renderText("")
    output$hfreq <- renderText("")
    output$efreq <- renderText("")
    
    iter <<- 0 #track number of updates
  
    prods <<- runif(1,min=2.,max=10.)
    cap <<- runif(1,min=10000.,max=500000.)
    true_Smsy <<- cap*(1/sqrt(prods)-1/prods)
    true_msy <<- prods*true_Smsy/(1+prods*true_Smsy/cap)*exp(sigR*sigR/2)-true_Smsy
    
    spawners[1] <<- cap*runif(1)
    Egoal[1] <<- cap*runif(1)
    #cat("0",spawners[1],Egoal[1],"\n")
    
    ####### initial years of simulation ######
    for (i in 1:nyears_init){
      iy <<- i
      alist <- one_year(prods,cap,sigR,spawners[iy],Egoal[1],sigE,slopeE)
      returns[iy] <<- alist$returns
      spawners[iy+1] <<- alist$escape
 #     cat(iy,spawners[iy],returns[iy],spawners[iy+1],"\n")
    } #initial years
    hfreq <<- 0. #frequency of not fishing
    efreq <<- 0. #frequency of not achieving escapement
    
    output$time <- renderPlot({
      #cat("restart-time",iter,iy,"\n")
      year=seq(1:iy)
      df <- data.frame(year=year,sp=spawners[1:iy],re=returns[1:iy])
      yp <- ggplot(df,aes(year,sp))+geom_col(col="blue")+expand_limits(x=0,y=0)
      yp <- yp + theme(text = element_text(size=16))
      yp <- yp + labs(title="Bars = Spawners, Line = Returns",x="Year",y="No. of Fish")
      yp <- yp +geom_point(aes(year,re),size=3,col="red")
      yp <- yp +geom_line(aes(year,re),col="red")
      yp
    }) #output$time   
    output$RvS <- renderPlot({
      #cat("restart-RvS",iter,iy,"\n")
      year=seq(1:iy)
      df2 <- data.frame(sp=spawners[1:iy],re=returns[1:iy])
      yp <- ggplot(df2,aes(sp,re))+geom_point(size=5,col="blue")+expand_limits(x=0,y=0)
      yp <- yp + theme(text = element_text(size=16))
      yp <- yp + labs(title= "Returns vs. Spawners", subtitle="(with replacement line)",x="Spawners",y="Returns")
      yp <- yp + geom_abline(slope=1,intercept=0)
      yp
    }) #output$RvS   
  })  ######## simulation start ##########

  ################ update escapement goal and advance simulation until next decision ###############
  observeEvent(input$new_goal, {
    #time to update escapement goal
    #cat("new_goal",iter,iy,"\n")
    
    iter <<- iter+1
    #cat(iter,"\n")
    Egoal[iter] <<- input$egoal
    #cat("Egoal",Egoal[iter],"\n")

    ###### end of simulation ###########
    if(iter>niters) {
      #cat("Iter = ",iter," niters = ",niters,"\n") 
      output$simEnd <- renderText("End of simulation ")
      
      cc <- as.character(format(true_Smsy,nsmall=0,big.mark=",",digits=3))
      output$trueSmsy <- renderText(c("The optimal escapement goal was ",cc))
      cd <- as.character(format(true_msy,nsmall=0,big.mark=",",digits=3))
      output$expC <- renderText(c("Which would average a catch of ",cd))
      avgC <- mean(simcatch[(nyears_init+1):totyears])
      ce <- as.character(format(avgC,nsmall=0,big.mark=",",digits=3))
      output$avgC <- renderText(c("Your average catch was ",ce))

      ch <- as.character(format(hfreq,nsmall=0,big.mark=",",digits=3))
      output$hfreq <- renderText(c("Frequency of not fishing = ",ch))
      cee <- as.character(format(efreq,nsmall=0,big.mark=",",digits=3))
      output$efreq <- renderText(c("Frequency of escapement less than half of Smsy = ",cee))
      
            
      last_score <- max(1.- abs(Egoal[niters]-true_Smsy)/true_Smsy,0) #compare last goal to Smsy
      early_score <- max(1.- mean(abs(Egoal[1:(niters-1)]-true_Smsy))/true_Smsy,0) #compare previous scores to Smsy
      freq_score <- 1. - hfreq #fishing closure score
#     goalScore <- (last_score*early_score*freq_score)^(1/3) #use geometric mean
      goalScore <- (last_score+early_score+freq_score)/3 #use arithmetic mean
      Escore <- grade(goalScore)
      cat(last_score,early_score,freq_score,goalScore,"\n")
      output$score <- renderText(c("Your grade is ",Escore))
      
      return()
    } ### sim end ######

    #increment population dynamics with new escapement goal
    for (uy in 1:nyears_update) {
      iy <<- iy+1
      #cat("uy,iy",uy,iy,"\n")
      alist <- one_year(prods,cap,sigR,spawners[iy],Egoal[iter],sigE,slopeE)
      #print(alist)
      returns[iy] <<- alist$returns
      
      simcatch[iy] <<- alist$catch
      spawners[iy+1] <<- alist$escape

      if(simcatch[iy]<0.001) hfreq <<- hfreq + 1/(nyears_update*niters) #track frequency of not fishing
      if(spawners[iy+1]<0.5*true_Smsy) efreq <<- efreq + 1/(nyears_update*niters) #track frequency of low escapements
      cat("iy,catch,escape,hfreq,efreq",iy,simcatch[iy],spawners[iy+1],hfreq,efreq,"\n")
      #     cat(iy,spawners[iy],returns[iy],"\n")

      output$time <- renderPlot({
        #cat("newgoal-time",iter,iy,"\n")
        year=seq(1:iy)
        df <- data.frame(year=year,sp=spawners[1:iy],re=returns[1:iy])
        yp <- ggplot(df,aes(year,sp))+geom_col(col="red")+expand_limits(x=0,y=0)
        yp <- yp + theme(text = element_text(size=16))
        yp <- yp + labs(title="Bars = Spawners, Line = Adult Returns from Spawners",x="Year",y="No. of Fish")
        yp <- yp +geom_point(aes(year,re),size=3,col="red")
        yp <- yp +geom_line(aes(year,re),col="red")
        yp
      }) #output$time   

      output$catch <- renderPlot({
        #cat("newgoal-time",iter,iy,"\n")
        year=seq(1:iy)
        df <- data.frame(year=year,ca=simcatch[1:iy])
        yp <- ggplot(df,aes(year,ca))+geom_col(col="green")+expand_limits(x=0,y=0)
        yp <- yp + theme(text = element_text(size=16))
        yp <- yp + labs(title="Bars = Catch",x="Year",y="No. of Fish")
        yp
      }) #output$time   
      
      output$RvS <- renderPlot({
        #cat("newgoal-RvS",iter,iy,"\n")
        year=seq(1:iy)
        df2 <- data.frame(sp=spawners[1:(iy-3)],re=returns[1:(iy-3)])
        yp <- ggplot(df2,aes(sp,re))+geom_point(size=5,col="blue")+expand_limits(x=0,y=0)
        df22 <- data.frame(sp=spawners[(iy-2):iy],re=returns[(iy-2):iy])
        yp <- yp + geom_point(data=df22,aes(sp,re),size=5,col="red")
        yp <- yp + theme(text = element_text(size=16),legend.position=c(0.95,0.05))
        yp <- yp + labs(title="Returns vs. Spawners",subtitle=" (with replacement line)",x="Spawners",y="Returns")
        yp <- yp + geom_abline(slope=1,intercept=0)
        yp
      }) #output$RvS   
      
    } # uy
  }) #observeEvent new goal
  ######################################################################################

} # end of server
#########################################################################################################

########### Model functions ############################################################################
one_year <- function(a,b,sig,S,E,sigE,slopeE) {
 #inputs:
  # a,b,sig = parameters of Beverton-Holt relationship R = aS(1+aS/b)*exp(z*sigma)
  # S = number of spawners
  # E = escapement goal
  # sigE = implementation error
  # slope of relationship between run size and escapement
  # hfunc(R,E,sigE) = function that generates a catch and escapement given returns R
 #returns:
  # a list with elements returns,escape,catch
 ##################
  #returns
  # cat("alist","\n")
  # cat("inputs",a,b,sig,S,E,sigE,slopeE,"\n")
   z <- rnorm(1,0.,sig)
   returns <- a*S/(1+a*S/b)*exp(z-sig*sig/2)
  
  #catch and escapement
   exp_esc <- min(returns,E + slopeE*(returns-E))
   z <- rnorm(1,0.,sigE)
   escape <- exp_esc*exp(z-sigE*sigE/2)
#   cat(returns,exp_esc,escape,sigE,z,"\n")
   catch <- max(0,returns-escape)
   
   #results
   alist <- list(returns=returns,escape=escape,catch=catch)
   return(alist)

} #one_year

### grade
grade <- function(vv) {
  xx <- ifelse(vv>0.97,"A+",ifelse(vv>0.92,"A",ifelse(vv>0.899,"A-",
        ifelse(vv>0.87,"B+",ifelse(vv>0.82,"B",ifelse(vv>0.799,"B-",  
        ifelse(vv>0.77,"C+",ifelse(vv>0.72,"C",ifelse(vv>0.699,"C-",  
        ifelse(vv>0.67,"D+",ifelse(vv>0.62,"D",ifelse(vv>0.599,"D-",
        "F"))))))))))))
  return(xx)
}


# Create Shiny app ----
shinyApp(ui, server)
