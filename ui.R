navbarPage("Comparison of Statistical approaches for \n subgroup effect finding!",
           ## This the code for inserting a Data-set
           #####
           tabPanel( "Data Import",     ## Name of the Tab 
                     pageWithSidebar(
                       headerPanel("R data reader"),              # Header:
                       # Input in sidepanel:
                       sidebarPanel(
                         tags$style(type='text/css', ".well { max-width: 20em; }"),
                         # Tags:
                         tags$head(
                           tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
                           tags$style(type="text/css", "select { width: 100%}"),
                           tags$style(type="text/css", "input { width: 19em; max-width:100%}")
                         ),
                         # Select filetype:
                         selectInput("readFunction", "Function to read data:", c(
                           "read.csv2","read.xlsx2","read_sav","read.table","read.delim","read.delim2",
                           "read.dta")),
                         
                         # Argument selecter:
                         htmlOutput("ArgSelect"),
                         
                         # Argument field:
                         htmlOutput("ArgText"),
                         
                         # Upload data:
                         fileInput("file", "Upload data-file:"),
                         
                         br(),
                         div(p(strong("When data loading is finished"))),
                         br(),
                         div(p(strong("Press the initiate button"))),
                         br(),
                         
                         actionButton("choice", "Initiate"),
                         
                         
                         # Variable selection:
                         htmlOutput("varselect"),
                         

                         sliderInput("number.of.observations", "observations to show" ,min = 1 , max=1000, value = 10)
                       ),
                       
                       # Main:
                       mainPanel(
                         tableOutput("table")
                       )
                     )),
           
           #####
           ## This the code for Effect size estimation
           ######
           tabPanel("Effect size estimation", # name of the Tab 
                    headerPanel(" We can calculate by hand or by indicating the parameters"),
                    sidebarPanel(
                      
                      div(p(strong("This is the effect size estimation tab"))),
                      div(p("In this section we calculate the effect size")),
                      div(p("Until now you can use the following effect sizes,")),
                      div(strong(p("Dichotomous Outcome :"))),
                      div(p("Odds-ratios, Risk-ratio,Risk difference, 
                            Arcsine square root transformed risk difference,
                            PETO odds-ratio")),
                      div(strong(p("Event Counts Outcome:"))),
                      div(p("Incidence rate ratio, 
                              Incidence rate difference,
                              Square root transformed incidence rate difference")),
                      div(strong(p("Continuous Outcome :"))),
                      div(p("Mean differences, Standardised mean difference and 
                              Ratio of means"))),
                    mainPanel(
                    tabsetPanel(type = "pills", 
                                tabPanel("Help",tags$img(src = "Figures/rstudio.png",width ="400px",height = "640px")),
                                tabPanel("Type of Outcome",
                                         selectInput("EStype",
                                                     strong("Type of Outcome"),
                                                     choices=c("make a choice","Dichotomous",
                                                               "Event-Counts",
                                                               "Continuous")),
                                         # Only show this panel if the EStype is 'Dichotomous Outcome'
                                         column(6, conditionalPanel(
                                           condition = "input.EStype == 'Dichotomous'",
                                           selectInput(
                                             "ES", "Effect Estimate",
                                             c("make a choice","Odds-ratio","Risk-ratio","Risk difference",
                                              "Arcsine square root transformed risk difference","PETO odds-ratio")))),
                                         column(6,conditionalPanel(
                                           condition = "input.EStype != 'make a choice'",
                                           selectInput(
                                             "slab", "Select the trial names", "upload a  Dataset",width = "400px"))),
                                         conditionalPanel(
                                           condition = "input.EStype == 'Dichotomous' & input.ES != 'make a choice'",
                                           column(selectInput("ai","ai: ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("bi","bi: ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("ci","ci: ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("di","di: ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("n1i","n1i:", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("n2i","n2i:", "upload a  Dataset",width = "400px"),width = 2)
                                         ),
                                         conditionalPanel(
                                           condition = "input.EStype == 'Event-Counts'",
                                           selectInput("Inci.ES", "Effect Estimate",
                                              c("make a choice","Incidence rate ratio","Incidence rate difference",
                                                "Square root transformed incidence rate difference"))),
                                         conditionalPanel(
                                           condition = "input.EStype == 'Event-Counts'",
                                           column(selectInput("x1i","the number of events (first group): ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("x2i","the number of events (second group): ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("t1i","the total person-times (first group): ", "upload a  Dataset",width = "400px"),width = 2), 
                                           column(selectInput("t2i","the total person-times (second group): ", "upload a  Dataset",width = "400px"),width = 2)
                                         ),
                                         conditionalPanel(condition = "input.EStype == 'Continuous'",
                                           selectInput("Con.ES", "Effect Estimate",
                                                       c("make a choice","Mean difference",
                                                         "Standardized mean difference",
                                                         "Standardized mean difference with heteroscedastic population variances"))),
                                         conditionalPanel(condition = "input.EStype == 'Continuous'",
                                                           column(selectInput("m1","the means (first group or time point) : ", "upload a  Dataset",width = "400px"),width = 2), 
                                                           column(selectInput("sd1","the standard deviations (first group or time point): ", "upload a  Dataset",width = "400px"),width = 2),
                                                           column(selectInput("m2","the means (second group or time point): ", "upload a  Dataset",width = "400px"),width = 2), 
                                                           column(selectInput("sd2","the standard deviations (second group or time point): ", "upload a  Dataset",width = "400px"),width = 2), 
                                                           column(selectInput("mn1","Group size A (n1i):", "upload a  Dataset",width = "400px"),width = 2), 
                                                           column(selectInput("mn2","Group size B (n2i):", "upload a  Dataset",width = "400px"),width = 2)
                                                         )
                                         ),tabPanel("ES calculator",
                                                    tableOutput("escalculator.summary")),
                                tabPanel("Meta-analysis output", 
                                         selectInput("typeofmeta","Type of meta-analysis", c("Fixed-effect","Random-effects")),
                                         conditionalPanel("input.typeofmeta =='Random-effects'",
                                                          selectInput("tau","tau^2$ estimator :", choices = c("DL", "HE", "SJ", "ML", "REML", "EB", "HS", "GENQ"))),
                                         verbatimTextOutput("meta.analysis.summary")), 
                                tabPanel("Forest-plots", 
                                         plotOutput("forest")), 
                                tabPanel("Funnel-plots", 
                                         plotOutput("funnelplot"))
                                
                                )
                    
                          )
                        ),
           tabPanel("Plots", # name of the Tab 
                    headerPanel(" In this tab we introduce plots that will help us"),
                    mainPanel(
                      
                      div(p(strong("This is the plots tab"))),
                      div(p("Forest-plots are used to graphically show")),
                      div(p("The effect sizes of the trials")),
                      
                      # Argument selecter:
                      htmlOutput("ForestSelect"),
                      
                      # Argument field:
                      htmlOutput("ForestText")))
                      )




