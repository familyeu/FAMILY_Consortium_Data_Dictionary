
header <- shinydashboardPlus::dashboardHeader(title=tags$a(href='https://family-project.eu/',tags$img(src='/logo.jpg', height=50)),
                                              tags$li(class = "dropdown",
                                                      tags$style(".main-header {max-height: 50px}"),
                                                      tags$style(".main-header .logo {height: 50px}")))

sidebar <- shinydashboardPlus::dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "About", icon =icon("circle-info", lib="font-awesome")),
    menuItem("Overview: families", tabName = "Overview_families", icon = icon("people-group", lib="font-awesome")),
    menuItem("Offspring", tabName = "Overview_offspring", icon = icon("child", lib="font-awesome")),
    menuItem("Parents", tabName = "Overview_parents", icon = icon("person", lib="font-awesome")),
    menuItem("Clinical metadata",tabName = "Clinical_information",icon = icon("notes-medical", lib="font-awesome")),
    menuItem("Genetics metadata",tabName = "Genetics", icon = icon("dna", lib="font-awesome")),
    menuItem("Neuroimaging metadata", tabName = "Neuroimaging",icon = icon("brain", lib="font-awesome")),
    menuItem("Calculate sample size", tabName = "sample_size",icon = icon("flask-vial", lib="font-awesome"))
  )
)

body <-  dashboardBody(
  setBackgroundImage(src='/brain_family.jpg', shinydashboard = TRUE),
  tabItems(
    tabItem(tabName = "About",
            h2(""),
            fluidPage(fluidRow(
              column(12, align = "center",
                     div(style = "display: inline-block; margin-bottom: 20px;", img(src = '/data_dict_logo.png', height = 200, width = 500))
                     ),
              column(width = 12, align = "center",
                     div(style = "margin-bottom: 20px; background-color: #F0ECFE; margin: 10 auto; padding: 10px; border-radius: 10px; width: 100%;",
                         div(class = "d-flex justify-content-center", # Center content horizontally
                             h4(style = "text-align: center; font-weight: bold; font-size: 18px;",
                                p(
                                  "The FAMILY consortium is a five-year ",
                                  "interdisciplinary, multi-site project, focused on improving ",
                                  "the lives of individuals with mental health disorders and their families."
                                ),
                                p(
                                  "By studying biological, clinical and environmental characteristics,",
                                  " and utilizing advanced statistics, FAMILY aims to better ",
                                  "understand the transmission of mental illness from parents to children.",
                                  " Furthermore, FAMILY aims to build models that predict the development",
                                  " of mental health disorders in high-risk relatives."
                                ),
                                "For more information about the consortium, you can visit our FAMILY-website",
                                a(href = "http://www.family-project.eu", "www.family-project.eu")
                             )
                         )
                     )
              ),
              column(12, align = "center",
                     div(style = "display: inline-block; margin-bottom: 20px;", img(src =  '/eu_logo.png', height = 250, width = 800))
              ),
              column(12, align = "center",
                     div(style = "display: inline-block; margin-bottom: 20px;", 
                         tags$a(href = "https://twitter.com/family_eu",img(src = '/twitter_logo.png', height = 40, width = 40)),
                         tags$a(href = "https://www.linkedin.com/company/88905532", img(src = '/linkedin_logo.png', height = 50, width = 50)),
                         tags$a(href = "https://www.youtube.com/@FAMILY-project/videos", img(src = '/youtube_logo.png', height = 50, width = 50)),
                         tags$a(href = "https://www.instagram.com/familyproject_eu/", img(src = '/instagram_logo.png', height = 40, width = 40)),
                         )
                     )))),  
    tabItem(tabName = "Overview_families",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Overview of the familial groups in FAMILY</h2>"),
            fluidPage(
              fluidRow(
                column(width = 12, align = "center",
                       shinydashboardPlus::box(width = 12, align = "center",
                                               titlePanel(""),
                                               fluidRow(
                                                 column(width = 6,
                                                        div(style = "text-align: center;font-size: 18px;",
                                                            selectInput("category", "Select a category:",
                                                                        choices = c('Bipolar disorder', 'Schizophrenia', 'Major depressive disorder', 'Control'),
                                                                        selected = 'Bipolar disorder')
                                                        )
                                                 ),
                                                 column(width = 6,
                                                        align = "center",
                                                        style = "height:130px;vertical-align: middle;",
                                                        valueBoxOutput(width = '75%', "FamilyCountsBox")
                                                 )
                                               ),
                                               fluidRow(
                                                 align = "center",  # Align content of the fluidRow to center
                                                 column(width = 6, offset = 3,
                                                        align = "center",
                                                        style = "text-align: center; font-size: 16px; align = center;",
                                                        h3(style = "font-size: 18px; text-align: center; font-weight: bold;",
                                                           "Total count of familial groups"),
                                                        div(style = "margin: 0 auto;",
                                                            formattableOutput( "table_count")
                                                        )
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12, 
                                                        align = 'center',  
                                                        plotlyOutput("sectorPlot")
                                                 ),
                                                 align = "center"  
                                               )
                       )
                )
              )
            )
    ),
    tabItem(tabName = "Overview_offspring",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Familial High-Risk offspring in FAMILY</h2>"),
            fluidPage(fluidRow(column(width = 12, align = "center",
                                      shinydashboardPlus::box(
                                        width = 12, height = 5,
                                        tabsetPanel(
                                          tabPanel("1 timepoint", plotOutput("plot_n_1", height = 600)), 
                                          tabPanel("2 timepoints", plotOutput("plot_n_2", height = 600)), 
                                          tabPanel("3 timepoints", plotOutput("plot_n_3", height = 600)) 
                                        )
                                      )
            )      
            
            )
            )
    ),
    tabItem(tabName = "Overview_parents",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Parents in FAMILY</h2>"),
            fluidPage(fluidRow(column(width = 12, align = "center",
                                      column(width = 12, align = "center", 
                                             shinydashboardPlus::box(
                                               width = 12,
                                               plotOutput("plot_n_parents", height = 900)
                                             )
                                      )      
            )
            )
            )
    ),
    tabItem(tabName = "Neuroimaging",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Neuroimaging data in FAMILY</h2>"),
            fluidPage(width = 12, 
              fluidRow(column(width = 12, align = "center",
                              shinydashboardPlus::box(width = 12,
                                                      titlePanel(""),
                              tabsetPanel(
                                tabPanel("Offspring",
                                         sidebarLayout(
                                                        sidebarPanel(width = 3,
                                                          selectInput("time", "Select a category:",
                                                                      choices = c('At least one timepoint', '1', '2', '3', '4'),
                                                                      selected = 'At least one timepoint')
                                                        ),
                                                        mainPanel( 
                                                          plotlyOutput("sectorPlot_imaging_offspring")
                                                        )            
                                                      )
                              ),
              tabPanel("Parents",
                       plotlyOutput("sectorPlot_img_parents")
                       )
              )))),
              fluidRow(
              shinydashboardPlus::box(width = 12, title = 'T1-weighted imaging', br(), br(), DTOutput("table_imaging_t1")),
              shinydashboardPlus::box(width = 12, title = 'Diffusion-weighted imaging', br(), br(), DTOutput("table_imaging_dwi")),
              shinydashboardPlus::box(width = 12, title = 'Resting-state fMRI', br(), br(), DTOutput("table_imaging_rs"))
              )
            )
    ),
    tabItem(tabName = "Genetics",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Genetic data in FAMILY</h2>"),
            fluidPage(width = 12, 
                      fluidRow(column(width = 12, align = "center",
                                      shinydashboardPlus::box(width = 12,
                                                              titlePanel(""),
                                                              tabsetPanel(
                                                                tabPanel("Offspring",
                                                                         plotlyOutput("sectorPlot_gen_offspring")),
                                                                tabPanel("Parents",
                                                                         plotlyOutput("sectorPlot_gen_parents")
                                                                )
                                                              )))),
                      fluidRow(
                        shinydashboardPlus::box(width = 12, DTOutput("table_gen"))
                      )
            )
    ),
    tabItem(tabName = "Clinical_information",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Clinical data in FAMILY</h2>"),
            fluidRow(
              shinydashboardPlus::box(width=12, 
                                     "Here you can select a timepoint number and search for a keyword",
                                      br(),
                                      br(),
                                      sliderInput("slider", "Timepoint:", min=1, max=5, value=1, step=1, animate=TRUE),
                                      textInput("text", "Keyword search"),
                                      tableOutput("values")))
    ),
    tabItem(tabName = "sample_size",
            HTML("<h2 style='background-color: #422C7E; padding: 10px; color: white; font-weight: bold; text-align:center;'>Calculate the estimated sample size of your study</h2>"),
            fluidRow(
              shinydashboardPlus::box(width=12, title = 'Interested in submitting a project initiation form? Calculate the estimated sample size for your study design',
                                      checkboxGroupInput(
                                        "input_1",
                                        "Choose the population you want to study:",
                                        choices = list("Schizophrenia", "Bipolar disorder", "Major depressive disorder", "Control"),
                                        selected = "Schizophrenia" 
                                      ),
                                      checkboxGroupInput(
                                        "input_2",
                                        "Choose design you are interested in:",
                                        choices = list("Triad", "Dyad", "Parents", "Offspring"),
                                        selected = "Triad"  
                                      ),
                                      conditionalPanel(
                                        condition = "input.input_2.includes('Triad') | input.input_2.includes('Dyad')",
                                        radioButtons(
                                          "input_3",
                                          "Do you need both parents to have the same diagnosis?",
                                          choices = list("Yes", "No"),
                                          selected = "Yes" 
                                        )
                                      ),
                                      checkboxGroupInput(
                                        "input_4",
                                        "Choose the cohorts you are interested in:",
                                        choices = list("BASYS-Bcn", "BASYS-FIBHGM", "BRIDGE","KBO", "Lausanne-Geneva", "VIA"),
                                        selected = "BASYS-Bcn"  
                                      ),
                                      plotOutput("sample_size")))
    )
    )
)



ui <- shinydashboardPlus::dashboardPage(skin = "purple", header, sidebar, body)