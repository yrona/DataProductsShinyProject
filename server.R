library(UsingR)
data(galton)

source("plot.R")
source("stats.R")

generate.explanation <- function(scenario) {
  
#   Explanation.Lower <-c(header = "Hypothesis: The population has a mean less than \\(\\mu_0\\)",
#                            explanation = "What is the likelihood that our sample was drawn from a populaton with a mean lower than \\(\\mu_0\\)?  An instance where we might ask this question is if a supermarket wanted to test that the boxes of cereal being delivered by a supplier were not overstating the amount of cereal in each box.") 
#   
#   Explanation.Higher <-c(header = "Hypothesis: The population has a mean greater than \\(\\mu_0\\)",
#                            explanation = "What is the likelihood that our sample was drawn from a populaton with a mean greater than \\(\\mu_0\\)?  An instance where we might ask this question is in examining the mean time to failure of a consumer product to ensure that their products are sufficiently robust.") 
#   
#   
#   Explanation.Different <-c(header = "Hypothesis: The population has a mean that is not \\(\\mu_0\\)",
#                             explanation = "What is the likelihood that our sample was drawn from a populaton with a mean that isn't \\(\\mu_0\\)?  An instance where we might ask this question is in examining the the weight of individual iron pigs in a casting plant.  If the pigs are too light, customers will complain (or sue).  It the pigs are too heavy, the producer's profits are negatively impacted.")
#   
  Explanation.Lower <-c(header = "Hypothesis: The population has a mean less than &mu;<sub>0</sub>",
                        explanation = "What is the likelihood that our sample was drawn from a populaton with a mean lower than &mu;<sub>0</sub>?  An instance where we might ask this question is if a supermarket wanted to test that the boxes of cereal being delivered by a supplier were not overstating the amount of cereal in each box.") 
  
  Explanation.Higher <-c(header = "Hypothesis: The population has a mean greater than &mu;<sub>0</sub>",
                         explanation = "What is the likelihood that our sample was drawn from a populaton with a mean greater than &mu;<sub>0</sub>?  An instance where we might ask this question is in examining the mean time to failure of a consumer product to ensure that their products are sufficiently robust.") 
  
  
  Explanation.Different <-c(header = "Hypothesis: The population has a mean that is not &mu;<sub>0</sub>",
                            explanation = "What is the likelihood that our sample was drawn from a populaton with a mean that isn't &mu;<sub>0</sub>?  An instance where we might ask this question is in examining the the weight of individual iron pigs in a casting plant.  If the pigs are too light, customers will complain (or sue).  It the pigs are too heavy, the producer's profits are negatively impacted.")
  
  Explanation.Used <- switch(scenario,
                  different = Explanation.Different,
                  low = Explanation.Lower,
                  high = Explanation.Higher)
  

  Explanation.Used

}

generate.directions <- function(scenario) {
  
  dir.part1 <- tags$li("Experiment with changing the mean of the actual population while leaving the expected population alone. What happens as the actual mean deviates farther and farther from the expected mean?")
  dir.part2 <- tags$li("Experiment with changing the standard deviation of the actual population while leaving the expected population alone. What happens as the actual standard deviation deviates farther and farther than the expected standard deviation?")
  dir.part3 <- tags$li("Experiment with changing the number of samples. What is the impact on the test of drawing more or fewer samples?")
  
  generate.directions <- tags$ol(
    dir.part1,
    dir.part2,
    dir.part3
  )  
}

generate.confidence.interval.offset <- function(scenario,sd0,nsample,alpha) {
  my.two.tail <- (scenario == "different")
  intervals <- confidence.interval(mu0 = 0,sd0 = sd0,nsample = nsample,alpha = alpha/100,two.tailed = my.two.tail)
  my.interval <- switch(scenario,
         different = intervals,
         low = intervals[1],
         high = intervals[2])

  my.interval
  #str(my.interval)
}

generate.result.explanation <- function(scenario,mu0,sd0,mu1,sd1,alpha,nsample) {
  my.two.tail <- (scenario == "different")
  intervals <- confidence.interval(mu0 = 0,sd0 = sd0,nsample = nsample,alpha = alpha/100,two.tailed = my.two.tail)
  my.interval <- switch(scenario,
                        different = intervals,
                        low = intervals[1],
                        high = intervals[2])
  
  if (length(my.interval) == 1) {
    if (my.interval < 0) {
      line0 <- mu0 + my.interval
      H1.prob <- pnorm(line0, mean = mu1, sd = sd1/sqrt(nsample))
      H0.prob <- 1 - H1.prob
      if (mu1 < mu0) {
        blurb <- paste("Since the actual mean is less than ",format(mu0,digits = 3),",", sep = "")
        correct.prob = H1.prob
      } else {
        blurb <- paste("Since the actual mean is greater than or equal to",format(mu0,digits = 3),",", sep = "")
        correct.prob = H0.prob
      }
      
      explanation <- paste("Under this scenario, if you were to take a sample of ", nsample, 
                          "observations,",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                          "of the time you would observe a sample mean less than or equal to ",format(line0,digits = 3),
                          ".  Therefore, you would conclude",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                          "of the time that the mean was less than",format(mu0,digits = 3),
                            "and would conclude",paste(format(H0.prob*100,digits = 3),"%",sep = ""),
                          "of the time that the mean was not less than that.", blurb, 
                          "you would be correct",paste(format(correct.prob*100,digits = 3),"%",sep = ""),"of the time.")
      
    } else {
      line0 <- mu0 + my.interval
      H1.prob <- pnorm(line0, mean = mu1, sd = sd1/sqrt(nsample), lower.tail = FALSE)
      H0.prob <- 1 - H1.prob
      if (mu1 > mu0) {
        blurb <- paste("Since the actual mean is greater than ",format(mu0,digits = 3),",", sep = "")
        correct.prob = H1.prob
      } else {
        blurb <- paste("Since the actual mean is less than or equal to",format(mu0,digits = 3),",", sep = "")
        correct.prob = H0.prob
      }
      
      explanation <- paste("Under this scenario, if you were to take a sample of ", nsample, 
                           "observations,",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                           "of the time you would observe a sample mean greater than or equal to ",format(line0,digits = 3),
                           ".  Therefore, you would conclude",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                           "of the time that the mean was greater than",format(mu0,digits = 4),
                           "and would conclude",paste(format(H0.prob*100,digits = 3),"%",sep = ""),
                           "of the time that the mean was not greater than that.", blurb, 
                           "you would be correct",paste(format(correct.prob*100,digits = 3),"%",sep = ""),"of the time.")     
    }
  } else {
    line0 <- mu0 + my.interval[1]
    line1 <- mu0 + my.interval[2]
    H0.prob <- pnorm(line1, mean = mu1, sd = sd1/sqrt(nsample)) - pnorm(line0, mean = mu1, sd = sd1/sqrt(nsample))
    H1.prob <- 1 - H0.prob 
    
    explanation <- paste("Under this scenario, if you were to take a sample of ", nsample, 
                         "observations,",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                         "of the time you would observe a sample mean less than or equal to ",format(line0,digits = 3),
                         "or greater than or equal to ",format(line1,digits = 3),
                         ".  Therefore, you would conclude",paste(format(H1.prob*100,digits = 3),"%",sep = ""),
                         "of the time that the mean differed significantly from",format(mu0,digits = 3),
                         "and would conclude",paste(format(H0.prob*100,digits = 3),"%",sep = ""),
                         "of the time that the mean was was approximately equal to it.")
    
  }  
}

display.data.choice <- function(choice) {
  treatment1 <- subset(PlantGrowth, group=="trt1")
  treatment2 <- subset(PlantGrowth, group=="trt2")
  control <- subset(PlantGrowth, group=="ctrl")
  
  display.data <- switch(choice,
                         different = control,
                         low = treatment1,
                         high = treatment2)
  
  display.data
}

shinyServer(  
  function(input, output) {
    scenario <- reactive({
       generate.explanation(input$Group)
    })
    interval <- reactive({ 
      generate.confidence.interval.offset(input$Group,input$sigma0,input$nsize,input$sig) 
      })
    
    output$Directions <- renderText(as.character(generate.directions()))
    output$ScenarioTitle <- renderText(as.character( scenario()[1]))
    output$Explanation <- renderText(as.character(scenario()[2]))
#    output$text1 <- renderText(dummy)
     output$CentralPlot <- renderPlot({
       plot1.curves <- define.curves(
                         c(input$mu0,input$mu1,input$mu1),
                         c(input$sigma0,input$sigma1,(input$sigma1/sqrt(input$nsize)) ),
                         c(100*input$sigma0,100*input$sigma1,80*input$sigma1/sqrt(input$nsize) ) ,
                         c("Hypothesized Distribution","Actual Distribution","Probability Density Function for Sample Mean"),
                         c("dotdash","dashed","solid"),
                         c(2,3,1)
                         )
       
       plot.interval <- input$mu0 + generate.confidence.interval.offset(input$Group,input$sigma0,input$nsize,input$sig)
       
       my.plot.1 <- pdf.plot(plot1.curves, plot.interval ,3,1)
       
       my.plot.1 <- mean.plot(my.plot.1,c(input$mu0,input$mu1),c("dashed","dashed"),c(2,1))
       
       
       
       my.plot.1
     })
    

    output$Stats <-  renderText( { 
      generate.result.explanation(input$Group,input$mu0,input$sigma0,input$mu1,input$sigma1, input$sig,input$nsize)
      
      } )
  }

  
)
