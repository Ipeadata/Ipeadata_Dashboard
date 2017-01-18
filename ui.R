##==============================================================================
## CARREGA PACOTES
##==============================================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("tidyverse", "stringr", "RODBC", "DT", "xts", "shinydashboard", "shiny", "dygraphs")

##==============================================================================
## ACESSA O BANCO DO IPEADATA
##==============================================================================
ipeadata <- odbcConnect("ipeadata", uid="", pwd="")

# get_dbnames <- function(){
#     sqlQuery(ipeadata, "select distinct SERCODIGO from ipea.vw_Valor")
# }

db_name <- sqlQuery(ipeadata, "select distinct SERCODIGO from ipea.vw_Valor")
db_subject <- sqlQuery(ipeadata, "select CATNOME from CATALOGO")
db_source <- sqlQuery(ipeadata, "select FNTNOME_P from FONTES")
db_period <- sqlQuery(ipeadata, "select PERNOME_P from PERIODICIDADES")

##==============================================================================
## USER-INTERFACE DO APLICATIVO
##==============================================================================
dashboardPage(
    dashboardHeader(
        title = "Validação de dados"
        ),
    dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Inputs", tabName = "inputs",
                 icon = icon("bar-chart-o"),
                 selectInput("subject",
                             "Tema",
                             c("", as.character(db_subject$CATNOME)),
                             selected = "",
                             multiple = FALSE),
                 selectInput("source",
                             "Fonte",
                             c("", as.character(db_source$FNTNOME_P)),
                             selected = "",
                             multiple = FALSE),
                 selectInput("period",
                             "Periodicidade",
                             c("", as.character(db_period$PERNOME_P)),
                             selected = "",
                             multiple = FALSE),
                 selectInput("idserie",
                             "ID da série",
                             c("", as.character(db_name$SERCODIGO)),
                             selected = "",
                             multiple = FALSE)
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