library(shiny)
library(ggplot2)
library(xtable)

#       img(src="240px-Em_old_faithful.gif", height = 400, width = 400)

shinyUI(fluidPage(
  
  navbarPage("Expectation–Maximization Algorithm",theme = "bootstrap-cerulean.css",
             tabPanel("Simulation",
                      
                      withMathJax(),
                      fluidRow(
                        column(3,
                               div(class="row-fluid", uiOutput("nSize")),
                               div(class="row-fluid", div(class="span6", uiOutput("m1")), div(class="span6", uiOutput("sd1"))),
                               div(class="row-fluid", div(class="span6", uiOutput("m2")), div(class="span6", uiOutput("sd2"))),
                               div(class="row-fluid", uiOutput("omega")),
                               h4('Initial Values'),
                               div(class="row-fluid", div(class="span6", uiOutput("m10")), div(class="span6", uiOutput("sd10"))),
                               div(class="row-fluid", div(class="span6", uiOutput("m20")), div(class="span6", uiOutput("sd20"))),
                               div(class="row-fluid", uiOutput("omega0")),
                               div(class="row-fluid", uiOutput("iteracao"))
                        ),
                        column(9,
                               div(class="row-fluid", div(class="span8",
                                                          h4("Gaussian Mixture"),
                                                          plotOutput("distPlot")),
                                   div(class="span4",
                                       h4("Fitting Model"),
                                       tableOutput("fit"))),
                               tags$style(type='text/css', '#model {background-color: rgba(255,255,255,0.40); color: rgba(255,99,71,1);}'), 
                               div(class="row-fluid", uiOutput("model")),
                               plotOutput('convPlot'))
                        
                      )), 
             tabPanel('Case study',
                      h3('Old Faithful Geyser Data'),
                      includeMarkdown('old_faithful.md'),
                      div(class="row-fluid", div(class="span3",
                                                 img(src="Old_Faithfull-wikijpg.jpg", height = "600px", width = "300px")),
                          div(class="span9",
                              HTML('<iframe src="https://mapsengine.google.com/map/embed?mid=zVEwdVu9XPPY.kg1xCoQUcV6o" width="540" height="380"></iframe>'))
                      ),
                      h3('Data Analysis'),
                      includeMarkdown("descr.md"),
                      h3('Parameter Estimation'),
                      includeMarkdown("estimation.md"),
                      div(class="row-fluid", div(class="span12",
                                                 img(src="faithful_ggplot.gif", height = "600px", width = "600px")))
                          )
                      
                      )
))




