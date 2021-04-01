library(shiny)
library(rhandsontable)

shinyUI(
  navbarPage(title = "CMSC 150 PROJECT",
             #tab
             tabPanel("Polynomial Regression",
                      fluidPage(titlePanel(title = "Polynomial Regression"),
                                hr(),
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("pfile","Upload a csv file"),
                            numericInput("polydegree","Degree:",1),
                            numericInput("polyX","Estimate(x):",0),
                            br(),
                            actionButton("polyCompute","Compute")
                          ),
                          mainPanel(
                            fluidRow(
                              column(2,
                                     h4("Points"),
                                     tableOutput("polyInput")),
                              column(10,
                                     h4("Function:"),
                                     textOutput("polyF"),
                                     h4("Estimated Value:"),
                                     textOutput("polyOut"))
                            )
                          )
                        )
                      )
                      ),
             tabPanel("Interpolating Spline",
                      fluidPage(titlePanel(title = "Interpolating Spline"),
                                hr(),
                                sidebarLayout(
                                  sidebarPanel(
                                    fileInput("sfile","Upload a csv file"),
                                    numericInput("splineX","Estimate (x):",0),
                                    br(),
                                    actionButton("splineCompute","Compute")
                                  ),
                                  mainPanel(
                                    fluidRow(
                                      column(2,
                                             h4("Points"),
                                             tableOutput("inputSpline")),
                                      column(10,
                                             h4("Functions:"),
                                             uiOutput("funcList"),
                                             h4("Estimated Value:"),
                                             textOutput("estimate"))
                                    )
                                  )
                                )
                      )
             ),
             
             tabPanel("Simplex Method",
                      fluidPage(titlePanel(title = "Simplex Method"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                               helpText("Cost"),
                               rHandsontableOutput("table"),
                               br(),
                               actionButton("saveBtn","Change")), 
                        mainPanel(
                               helpText("Minimum Number of Shipments"),
                               tableOutput("minTable"),
                               h4("Minimum Shipping Cost"),
                               textOutput("min")
                               )
                      ),
                      tabsetPanel(
                        tabPanel("Objective function and Constraint",
                                 "Objective Function:",
                                 textOutput("obs"),
                                 "Constraints:",
                                 textOutput("con1"),
                                 textOutput("con2"),
                                 textOutput("co3"),
                                 textOutput("con4"),
                                 textOutput("con5"),
                                 textOutput("con6"),
                                 textOutput("con7"),
                                 textOutput("con8")
                                 ),
                        tabPanel("Initial Tableau",
                                 tableOutput("initialTab")),
                        tabPanel("Iterations",
                                 uiOutput("iters"))
                      )
                      )
             )
             )
)
