library(hrbrthemes)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(fresh)
library(plotly) 
library(shinydashboardPlus)
library(shinyWidgets)

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#363973"
  ),
  adminlte_sidebar(
    width = "280px",
    dark_bg = "#8082A6",
    dark_hover_bg = "#8082A6",
    dark_color = "#FEFFFF"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

sidebar <-   dashboardSidebar(sidebarMenu(id = "sidebar", # id important for updateTabItems
                                          menuItem("Attribute", tabName = "Esp1", icon = icon("line-chart"),menuItem('',
                                                                                                                               tabName = 'Esp'),conditionalPanel('input.sidebar == "Esp"',
                                                                                                                                                                 selectInput('db','Technical characteristics:',choices = c('Interview','Phone'),selected = 'Interview'),
                                                                                                                                                                 selectInput('met','Measure:',choices = c('Mean','Midpoint','Minimum','Maximum'),selected = 'Mean'),
                                                                                                                                                                 
                                                                                                                                                                 selectizeInput("age",label = "Age range:",
                                                                                                                                                                                c("All","E1","E2","E3"),multiple=FALSE,selected = "All",options = list(plugins= list('remove_button'))),
                                                                                                                                                                 selectizeInput("grade",label = "Education:",
                                                                                                                                                                                c("All", "N1","N2","N3"
                                                                                                                                                                                ),multiple=FALSE,selected = "All",options = list(plugins= list('remove_button'))),
                                                                                                                                                                 
                                                                                                                                                                 selectInput("sex", label = "Gender:", c("All", "F", "M")),
                                                                                                                                                                 selectizeInput("vowels",label = "Vowels:",
                                                                                                                                                                                c( "All",'a','e','i','o','u','ae','ai','ao','au','ea','ei','eo','eu','ia','ie','io','iu','oa','oe','oi','ou','ua','ue','ui','uo')
                                                                                                                                                                                ,multiple=FALSE,selected = "All",options = list(plugins= list('remove_button'))),
                                                                                                                                                                 
                                                                                                                                                                 selectInput("tonicidad", label = "Stress:", c("All", "A", "T")),
                                                                                                                                                                 
                                                                                                                                                                 selectInput("position", label = "Position:", c("All", "Open", "Closed")),
                                                                                                                                                                 
                                                                                                                                                                 conditionalPanel(
                                                                                                                                                                   
                                                                                                                                                                   condition = "input.position == 'Closed'",
                                                                                                                                                                   
                                                                                                                                                                   selectizeInput("cons_ant_trabada",label = "Previous consonant:",
                                                                                                                                                                                  c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP')
                                                                                                                                                                                  ,multiple=FALSE,selected = "All",options = list(plugins= list('remove_button'))),
                                                                                                                                                                   
                                                                                                                                                                   selectizeInput("cons_traba_despues",label = "Following consonant:",
                                                                                                                                                                                  c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP')
                                                                                                                                                                                  ,multiple=FALSE,selected = "All",options = list(plugins= list('remove_button')))
                                                                                                                                                                 ),
                                                                                                                                                                 
                                                                                                                                                                 conditionalPanel(
                                                                                                                                                                   condition = "input.position == 'Open'",
                                                                                                                                                                   selectizeInput("cons_libre",label = "Previous consonant:",
                                                                                                                                                                                  c("All",'COSB','COSD','COSV','COB', 'COD','COV','CFA','CFL','CFV','CFP','CL','CNB','CNA','CNP','CVS','CVM','CAP')
                                                                                                                                                                                  ,multiple=FALSE,selected = "All",options = list(plugins= list('remove_button')))),
                                                                                                                                                                 
                                                                                                                                                                 downloadButton('download',"Download data",class = 'butt'),
                                                                                                                                                                 tags$head(tags$style(".butt{background-color:white;color:black !important;} .butt{color: red;}")))
                                          )))
body <- dashboardBody(use_theme(mytheme),#tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
                      tabItems(
                        tabItem("Esp", fluidRow(width = 12, align="center",
                                                column(12, valueBox(
                                                  "CLOE México", "Speech Corpus for Mexican Spanish",
                                                  color = "light-blue",width = 12
                                                ))
                        ),tabsetPanel(
                          tabPanel("Description",
                                   fluidRow(width = 12, align="center",
                                            br(),
                                            box(
                                              title = h5("The descriptive statistics and graphics are generated from the Speech Corpus for Mexican Spanish (CLOE México). Likewise, the labels of this tool are based on the ones used for the segmentation and labelling of CLOE México."), solidHeader = FALSE, width = 12,
                                              status = "primary",align="right",
                                              strong(h5("Developed under the project DGAPA-PAPIIT IA401419"))
                                            )),
                                   withTags({
                                     div(class="header", checked=NA,
                                         #h5("Los datos a partir de los cuales se generan las estadísticas descriptivas y las gráficas de esta herramienta son del Corpus de Lengua Oral del Español de México (CLOE México). 
                                         #  Asimimo, las etiquetas de esta herramienta se basan en las que se utilizan en la segmentación y etiquetado del CLOE México."),
                                         p("The user can select the technical conditions of the recording:"),
                                         p(strong("Technical characteristics:")),
                                         li("Interview: with a bandwidth of 0 to 14000 Hz"), 
                                         li("Phone: with a bandwidth of 300 to 3400 Hz"),
                                         p(""),
                                         p("Can also include all the corpus speakers or selected by:"),
                                         strong('Measure:'),
                                         li('Mean: the acoustic parameter’s mean is extracted at the vocalic segment
'),li('Midpoint: the acoustic parameter’s value is extracted at the mid point of the vocalic segment
'),li('Minimum value: the minimum value of the acoustic parameter is extracted at the vowel segment'),li('Maximum value: the maximum value of the acoustic parameter is extracted at the vowel segment'),
                                         p(""),
                                         strong('Age range:'),
                                         li('E1: 18 to 34 years'),li('E2: 35 to 54 years'),li('E3: 55 years and upwards'),
                                         p(""),
                                         strong('Education:'),
                                         li('N1: No studies or elementary school'),li('N2: Middle school to high school'),li('N3: Bachelor’s degree upwards'),
                                         p(""),
                                         strong('Gender:'),
                                         li('F: Female'),li('M: Male'),
                                         p(""),
                                         strong('Vowels:'),
                                         p('In reference to vocalic sounds, the user can generate statistics from all the vocalic sounds uttered in the corpus, or select only those of interest.'),
                                         p('Furthermore, the user can select said sound depending on:'),
                                         strong('Stress:'),li('A: Unstressed'),li('T: Stressed'),
                                         p('Y en el contexto silábico en el que ocurren:'),
                                         strong('Position:'), li('Open'),li('Closed'),
                                         h5(p('If “open” is chosen, the user can also select the consonant(s) preceding the vocalic sound with the following selection:')),
                                         strong('Previous consonant'),
                                         h5(p('And if “closed” is chosen, the user can select, not only the consonant(s) preceding the vocalic sound, but also the consonant(s) succeeding it with the following selection:')),
                                         p(strong('Following consonant')),
                                         p('The labels of said consonants are the following:'),
                                         li('b = COSB'),
                                         li('d = COSD'),
                                         li('g = COSV'),
                                         li('p = COB'),
                                         li('t = COD'),
                                         li('k = COV'),
                                         li('s = CFA'),
                                         li('f = CFL'),
                                         li('x = CFV'),
                                         li('y = CFP'),
                                         li('l = CL'),
                                         li('m = CNB'),
                                         li('n = CNA'),
                                         li('ñ = CNP'),
                                         li('r = CVS'),
                                         li('rr = CVM'),
                                         li('ch = CAP'),
                                         p(""),
                                         strong("Note:"), p("the symbol + displays more information.")
                                     )})),
                          tabPanel("Data",br(), dataTableOutput("table")),
                          tabPanel("Descriptive statistics", br(), dataTableOutput("summary")),
                          tabPanel("Dispersion",
                                   br(),
                                   fluidRow
                                   (
                                     column(4,uiOutput('media')),
                                     column(4,uiOutput('efeuno')),
                                     column(4,uiOutput('efedos'))
                                   )
                                   ,
                                   fluidRow
                                   (
                                     column(4,uiOutput('efetres')),
                                     column(4,uiOutput('efecuatro')),
                                     column(4,uiOutput('duracion'))
                                     
                                   )
                          ),
                          tabPanel("Distribution",
                                   br(),
                                   fluidRow
                                   (
                                     column(4,uiOutput('media_d')),
                                     column(4,uiOutput('efeuno_d')),
                                     column(4,uiOutput('efedos_d'))
                                   )
                                   ,
                                   fluidRow
                                   (
                                     column(4,uiOutput('efetres_d')),
                                     column(4,uiOutput('efecuatro_d')),
                                     column(4,uiOutput('duracion_d'))
                                     
                                   )
                          ),
                          tabPanel("Comparative: Technical characteristics",
                                   fluidRow(width = 12, align="center",
                                            br(),
                                            column(6,plotlyOutput("d1")),
                                            column(6,plotlyOutput("d2"))
                                   ),
                                   fluidRow(width = 12, align="center",
                                            br(),
                                            column(6,plotlyOutput("d3")),
                                            column(6,plotlyOutput("d4"))
                                   ),
                                   fluidRow(width = 12, align="center",
                                            br(),
                                            column(6,plotlyOutput("d5")),
                                            column(6,plotlyOutput("d6"))
                                   ))
                        ))
                      ))

#tags$li(class = "dropdown", actionButton("Idioma", "English"))),
ui <-dashboardPage(
  header = dashboardHeader(title= div(h4('', style="margin: 5px;"))),
  sidebar = sidebar,
  body = body,
  #skin = 'black',
  title = 'Forensic Acoustics')