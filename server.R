##==============================================================================
## INSTALL AND LOAD PACKAGES
##==============================================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("tidyverse", "stringr", "RODBC", "DT", "xts", "shinydashboard", "shiny", "dygraphs")

##==============================================================================
## SERVER
##==============================================================================

shinyServer(function(input, output, session) {
    
    ipeadata <- odbcConnect("ipeadata", uid="", pwd="")
    
    get_dbnames <- function(){
        sqlQuery(ipeadata, "select distinct SERCODIGO from ipea.vw_Valor")
    }
        
    statement <- reactive({
        if(!is.null(input$idserie))
            {
            sprintf("SELECT ipea.vw_Valor.SERCODIGO,
                    CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, ipea.vw_Valor.VALVALOR 
                    FROM ipea.vw_Valor 
                    WHERE (((ipea.vw_Valor.SERCODIGO)='%s'))",
                    input$idserie)
        }
        })    
    
    #  and ipea.vw_Valor.VALVALOR IS NOT NULL
    
    output$plot_title <- renderText({ 
          
          db_title <- sqlQuery(channel = ipeadata, sprintf("SELECT SERNOME_P
                            FROM SERIES WHERE SERCODIGOTROLL='%s';", input$idserie))
          
          ifelse(input$idserie=="", 
                 "Selecione uma base de dados",
                 as.character(db_title$SERNOME_P)
                 )
          })
    
    output$table_title <- renderText({ 
          
          db_title <- sqlQuery(channel = ipeadata, sprintf("SELECT SERNOME_P
                            FROM SERIES WHERE SERCODIGOTROLL='%s';", input$idserie))
          
          ifelse(input$idserie=="", 
                 "Selecione uma base de dados",
                 as.character(db_title$SERNOME_P)
          )
    })
    
    output$db_plot <- renderDygraph({
        
        validate(
            need(input$idserie, "Por favor, selecione uma base de dados."))
        
        db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
        db <- db %>%
            setNames(tolower(names(.))) %>%
            mutate(valdata = as.Date(valdata, origin = "1900-01-01"))
               
        xts(db$valvalor, order.by = as.Date(db$valdata, format = "%Y-%m-%d")) %>%
            setNames(., c("valvalor")) %>%
            dygraph(.) %>% 
#             dySeries(c("li", "valvalor", "ls"), label = "Valor", drawPoints = T) %>%
            dyRangeSelector() %>% 
            dySeries("valvalor", label = "Valor", drawPoints = TRUE) %>% 
            dyOptions(connectSeparatedPoints = FALSE)
    })
    
    output$tb1 <- DT::renderDataTable({
            db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
            db <- db %>%
                mutate(VALDATA = as.Date(VALDATA, origin = "1900-01-01"))
    })

    output$tb2 <- DT::renderDataTable({
    db2 <- sqlQuery(channel = ipeadata,
                    query = "SELECT ser.SERID, ser.SERCODIGOTROLL, f.FNTNOME_P as Fonte,

   CASE
    WHEN ser.PERID< 0 then 2*(cast(ser.SERMAXDATA-getdate() AS integer))
    WHEN ser.PERID= 1 then cast(ser.SERMAXDATA-getdate() AS integer) + ser.SERPRAZOATUALIZACAO +30
    WHEN ser.PERID> 1 then cast(ser.SERMAXDATA-getdate() AS integer) + ser.SERPRAZOATUALIZACAO
   END As Atraso ,

   CASE
    WHEN ser.PERID< 0 then cast(ser.SERMAXDATA-getdate() AS integer)
    WHEN ser.PERID= 1 then cast(ser.SERMAXDATA-getdate() AS integer)
    WHEN ser.PERID> 1 then cast(ser.SERMAXDATA-getdate() AS integer)
   END As AtrasoPuro,

   ser.SERMAXDATA,

   CASE
        when ser.SERTIPO = 'N' then 'Macroeconômico'
        when ser.SERTIPO = 'R' and ser.CATID = 1 then 'Regional'
        when ser.SERTIPO = 'R' and ser.CATID = 2 then 'Social'
        Else 'ERRO!'
    END As SERBASE
    FROM ipeadata.dbo.SERIES ser LEFT OUTER JOIN ipeadata.dbo.FONTES f ON ser.FNTID = f.FNTID 
    WHERE ser.SERTIPO = 'N' AND ser.SERNUMERICA = 1 and ser.SERSTATUS='A' and ser.SERMAXDATA < getdate() and ser.SERMAXDATA>0
    order by Atraso;")
    })

    output$db_nrows <- renderValueBox({
    
        db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
        db <- db %>%
            setNames(tolower(names(.))) %>%
            mutate(valdata = as.Date(valdata, origin = "1900-01-01"))
        
    valueBox(
        ifelse(input$idserie=="", "Nenhuma", nrow(db)),
        "Observacoes",
        icon = icon("list"))
})
    
    output$db_missing <- renderValueBox({
        
        monnb <- function(d) { 
            lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
            lt$year*12 + lt$mon} 
        
        mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
        
        db <- sqlQuery(channel = ipeadata, sprintf("SELECT ipea.vw_Valor.SERCODIGO, 
               CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, 
               ipea.vw_Valor.VALVALOR 
               FROM ipea.vw_Valor 
               WHERE SERCODIGO='%s';",
                                                   input$idserie))
        
        db_per <- sqlQuery(channel = ipeadata, sprintf("SELECT P.PERID, PERNOME_P, SERCODIGOTROLL
                            FROM PERIODICIDADES P 
                            INNER JOIN SERIES S 
                            ON P.PERID = S.PERID 
                            WHERE S.SERCODIGOTROLL='%s';", 
                                                       input$idserie))
        
        db <- db %>%
            tbl_df %>% 
            left_join(., db_per, by = c("SERCODIGO" = "SERCODIGOTROLL")) %>% 
            mutate(VALDATA = as.Date(VALDATA, origin = "1900-01-01"),
                   VALDATA = str_sub(strptime(VALDATA, format = "%Y-%m-%d"), 1, 10),
                   VALMISSING = ifelse(PERID==-15, 
                                       round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "weeks")), 0)/2-length(which(!is.na(db$VALVALOR)))+1,
                                       ifelse(
                                           PERID==-1, 
                                           round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days")), 0)-length(which(!is.na(db$VALVALOR)))+1,
                                       ifelse(
                                           PERID==12,
                                           round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/365, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                           ifelse(
                                               PERID==1,
                                               as.integer((difftime(VALDATA[nrow(db)], VALDATA[1], units = "days")/365)*12)-length(which(!is.na(db$VALVALOR)))+1,
                                               ifelse(
                                                   PERID==3,
                                                   round((as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/365)*4, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                                   ifelse(
                                                       PERID==6,
                                                       round((as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/365)*2, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                                       ifelse(
                                                           PERID==48,
                                                           round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/1460, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                                           ifelse(
                                                               PERID==60,
                                                               round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/1825, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                                               ifelse(
                                                                   PERID==120,
                                                                   round(as.integer(difftime(VALDATA[nrow(db)], VALDATA[1], units = "days"))/3650, 0)-length(which(!is.na(db$VALVALOR)))+1,
                                                                   NA))))))))))
                                       # VALMISSING = ifelse(PERID==1, mondf(VALDATA[1], VALDATA[nrow(db)])-length(which(!is.na(db$VALVALOR)))+1, NA))
        
        valueBox(
            ifelse(input$idserie=="", "Nenhum", db$VALMISSING[1]),
            "Valores ausentes", icon = icon("question"), color = "orange")

        
        })
    })