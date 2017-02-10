##==============================================================================
## LOAD PACKAGES
##==============================================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("tidyverse", "stringr", "RODBC", "DT", "xts", "shinydashboard", "shiny", "dygraphs")

##==============================================================================
## CONNECT TO IPEADATA DATABASE AND RUN SOME QUERIES
##==============================================================================
# ipeadata <- odbcConnect("ipeadata", uid="", pwd="")

##==============================================================================
## USER-INTERFACE
##==============================================================================
# dbHeader <- dashboardHeader(title = "Dashboard",
#                             tags$li(a(href = 'http://www.ipeadata.gov.br/',
#                                       div(img(src = 'logo.png', style="margin:0;padding-left:3px;border:0;")),
#                                     class = "dropdown")))
                                      
dashboardPage(
      skin = "blue",
      dashboardHeader(title = "ipeadata Dashboard",
                      dropdownMenu(type = "messages",
                                   messageItem(
                                         from = "Erivelton Guedes",
                                         message = "A parte de séries foi corrigida."
                                   ),
                                   messageItem(
                                         from = "Support",
                                         message = "Novas funcionalidades em desenv.",
                                         icon = icon("life-ring"),
                                         time = "10/02/2017"
                                   )
                      ),
                      dropdownMenu(type = "notifications",
                                   notificationItem(
                                         text = "9 séries atualizadas",
                                         icon("check-square-o")
                                   )),
                      dropdownMenu(type = "tasks", badgeStatus = "success",
                                   taskItem(value = 90, color = "green",
                                            "Atualização de séries"
                                   ),
                                   taskItem(value = 55, color = "yellow",
                                            "Correção de valores suspeitos"
                                   )
                      )),
      
      # dbHeader,
#       dashboardHeader(
#             title = "ipeadata Dashboard"
      
      dashboardSidebar(
            width = 350,
            sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Inputs", tabName = "inputs", icon = icon("bar-chart-o"),
                           selectInput("subject",
                                       "Tema",
                                       "",
                                       # c("", levels(db_metadata$CATNOME)),
                                       selected = "",
                                       multiple = FALSE),
                           selectInput("src",
                                       "Fonte",
                                       "",
                                       # c("", levels(db_metadata$FNTNOME_P)),
                                       selected = "",
                                       multiple = FALSE),
                           selectInput("period",
                                       "Periodicidade",
                                       "",
                                       # c("", levels(db_metadata$PERNOME_P)),
                                       selected = "",
                                       multiple = FALSE),
                           selectInput("idserie",
                                       "ID da série",
                                       "",
                                       # c("", levels(db_metadata$SERCODIGOTROLL)),
                                       selected = "",
                                       multiple = FALSE)
                           # uiOutput("secondSelection")
                           # uiOutput("newinputs")
                  ),
                  conditionalPanel(
                        condition = "input.subject == 'Regional' | input.subject == 'Social'",
                        # selectInput("geolevel", "Escolha o nível geográfico", 
                        #             choices = c("Brasil", "Regiões", "Estados", "Municípios")),
                        selectInput("geolevel", 
                                    "Escolha o nível geográfico", 
                                    "",
                                    selected = "",
                                    multiple = F),
                                    # choices = c("Brasil", "Regiões", "Estados", "Municípios")),
                        selectInput("geoscope", "Escolha a abragência", 
                                    choices = c("Brasil", "Região Centro-Oeste", "Estados")),
                        selectInput("time_start", "Início", 
                                    choices = c(as.character(c(1970:2016)))),
                        selectInput("time_end", "Fim", 
                                    choices = c(as.character(c(1970:2016))))
                  ),
                  menuItem("Series atrasadas", tabName = "atrasadas", icon = icon("database"))
            )
      ),
      dashboardBody(
            tabItems(
                  tabItem(tabName = "dashboard",
                          h2("Estatísticas relevantes"),
                          fluidRow(
                                valueBoxOutput("db_nrows"),
                                valueBox("Nenhum", "Valores suspeitos", color = "red", icon = icon("exclamation")),
                                valueBoxOutput("db_missing")
                          ),
                          h2("Visualização dos dados"),
                          fluidRow(
                                box(title = textOutput("plot_title"), "", width = 12,
                                    solidHeader = T, status = "primary",
                                    helpText("Clique e arraste o cursor no gráfico para aproximar e selecionar períodos de tempo. O duplo clique no gráfico retorna para a visualização inicial."),
                                    dygraphOutput("db_plot", height = "400px")),
                                box(title = "Mapa - Selecione uma base de dados", "", width = 12,
                                    solidHeader = T, status = "primary"),
                                box(title = textOutput("table_title"), "", width = 12, 
                                    solidHeader = T, status = "primary",
                                    DT::dataTableOutput("tb1", height = "300px")                
                                )
                          )
                          
                  ),
                  tabItem(tabName = "atrasadas",
                          h2("Series atrasadas"),
                          fluidRow(
                                box(width = 12, solidHeader = T, status = "primary",
                                    title = paste("Lista de series atrasadas"),
                                    DT::dataTableOutput("tb2", height = "300px")
                                )
                          )
                  )
            )
            )
      )