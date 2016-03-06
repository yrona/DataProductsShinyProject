
shinyUI(pageWithSidebar(  
  headerPanel("Stats Lesson 1: Introduction to Hypothesis Testing"),
  sidebarPanel(
    withMathJax(radioButtons('Group', "Select Hypothesis",
                             list("H1: \\(\\mu < \\mu_0\\)" = "low",
                                  "H1: \\(\\mu > \\mu_0\\)" = "high",
                                  "H1: \\(\\mu \\neq \\mu_0\\)" = "different") 
                              )
              )
    
  )
  ,
  
  mainPanel(    
#     plotOutput('myHist'),  
#     textOutput('text1')
    h3(htmlOutput('ScenarioTitle')),
    htmlOutput('Explanation'),
    #textOutput('Explanation'),
    h3("Directions"),
    withMathJax(htmlOutput('Directions')),
    #withMathJax( as.character( htmlOutput('ScenarioTitle'))),
    #htmlOutput('Explanation'),
    h4("Expected Population"),
    fluidRow(column(4,withMathJax(sliderInput('mu0',"\\(\\mu_0\\)",0,200,100))),
             column(3,withMathJax(sliderInput('sigma0',"\\(\\sigma_0\\)",1,50,5)))
             ),
    h4("Actual Population Being Sampled"),
    fluidRow(column(4,withMathJax(sliderInput('mu1',"\\(\\mu_{act}\\)",0,200,110))),
             column(3,withMathJax(sliderInput('sigma1',"\\(\\sigma_{act}\\)",1,50,7)))),
    h4("Samples Being Drawn"),
    fluidRow(column(5,withMathJax(sliderInput('nsize',"Number of samples: \\(n\\)",1,1000,5)))
             ,column(5,withMathJax(sliderInput('sig',"Significance: \\(\\alpha\\)",0,10,5, step = .1, post = "%")))
             ),
     withMathJax(plotOutput('CentralPlot')),
     withMathJax(textOutput('Stats'))
  )
))