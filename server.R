##==============================================================================
## INSTALL AND LOAD PACKAGES
##==============================================================================
if (!require("install.load")) install.packages("install.load")
install.load::install_load("tidyverse", "stringr", "RODBC", "DT", "xts", "shinydashboard", "shiny", "dygraphs", "reshape2", "leaflet")

ipeadata <- odbcConnect("ipeadata", uid="", pwd="")
db_metadata <- sqlQuery(ipeadata, "select distinct SERCODIGOTROLL, CATID, PERID, FNTID from dbo.SERIES")
db_subject <- sqlQuery(ipeadata, "select CATID, CATNOME from CATALOGO")
db_source <- sqlQuery(ipeadata, "select FNTID, FNTNOME_P from FONTES")
db_period <- sqlQuery(ipeadata, "select PERID, PERNOME_P from PERIODICIDADES")
db_tert <- sqlQuery(ipeadata, "SELECT TERCODIBGE, TERNOME, TNIVID FROM dbo.TERRITORIO")
db_nivel <- sqlQuery(ipeadata, "select TNIVID, TNIVNOME from TIPONIVEL")
db_tert <- db_tert %>% 
    tbl_df %>%
    left_join(., db_nivel) %>% 
    # mutate(ID_GEOLEVEL = as.factor(ifelse(TNIVID==0, "Brasil",
    #                             ifelse(TNIVID==1, "Regioes",
    #                                    ifelse(TNIVID==2, "Estados",
    #                                           ifelse(TNIVID==5, "Municipios", 
    #                                                  NA)))))) %>% 
    select(-TNIVID)

db_metadata <- db_metadata %>% 
      # tbl_df %>% 
      left_join(., db_subject) %>% 
      left_join(., db_source) %>% 
      left_join(., db_period) 
    # mutate(CATNOME = iconv(CATNOME, to='ASCII//TRANSLIT'))

##==============================================================================
## SERVER
##==============================================================================

shinyServer(function(input, output, session) {
    
    ipeadata <- odbcConnect("ipeadata", uid="", pwd="")
    
    # get_dbnames <- function(){
    #     sqlQuery(ipeadata, "select distinct SERCODIGO from ipea.vw_Valor")
    # }
        
    statement <- reactive({
        if(!is.null(input$idserie))
            {
            db_ter <- sqlQuery(ipeadata, "SELECT TERCODIBGE, TERNOME, TNIVID FROM dbo.TERRITORIO")
            db_nivel <- sqlQuery(ipeadata, "select TNIVID, TNIVNOME from TIPONIVEL")
            db_ter <- db_ter %>% 
                filter(!(TERCODIBGE==0 & !TERNOME=="Brasil"), !duplicated(TERCODIBGE)) %>% 
                mutate(TERCODIBGE = as.character(TERCODIBGE)) %>% 
                left_join(., db_nivel) %>% 
                # mutate(ID_GEOLEVEL = as.factor(ifelse(TNIVID==0, "Brasil",
                #                                       ifelse(TNIVID==1, "Regioes",
                #                                              ifelse(TNIVID==2, "Estados",
                #                                                     ifelse(TNIVID==5, "Municipios", 
                #                                                            NA)))))) %>% 
                data.frame
            
            db_serie <- sqlQuery(ipeadata, sprintf("SELECT ipea.vw_Valor.SERCODIGO, ipea.vw_Valor.TERCODIGO,
                    CAST (ipea.vw_Valor.VALDATA as NUMERIC) as VALDATA, ipea.vw_Valor.VALVALOR 
                    FROM ipea.vw_Valor                    
                    WHERE (((ipea.vw_Valor.SERCODIGO)='%s'))",
                                 input$idserie)) 
            
            db_serie <- db_serie %>% 
                tbl_df %>% 
                # data.frame %>% 
                mutate(TERCODIGO = as.character(TERCODIGO)) %>%
                left_join(., db_ter, by = c("TERCODIGO" = "TERCODIBGE"))
            
            return(db_serie)
                
            
            # sprintf("SELECT val.SERCODIGO, val.TERCODIGO, ter.TERCODIBGE, ter.TERNOME,
            #         CAST (val.VALDATA as NUMERIC) as VALDATA, val.VALVALOR 
            #         FROM ipea.vw_Valor val                    
            #         LEFT JOIN dbo.TERRITORIO ter 
            #         ON val.TERCODIGO = ter.TERCODIBGE
            #         WHERE (((val.SERCODIGO)='%s'))",
            #         input$idserie)
        }
        })    
    
    #  and ipea.vw_Valor.VALVALOR IS NOT NULL
    
#     db_metadata <- function(){
#           db_db_metadata <- sqlQuery(ipeadata, "select distinct SERCODIGOTROLL, CATID, PERID, FNTID from dbo.SERIES")
#           db_subject <- sqlQuery(ipeadata, "select CATID, CATNOME from CATALOGO")
#           db_source <- sqlQuery(ipeadata, "select FNTID, FNTNOME_P from FONTES")
#           db_period <- sqlQuery(ipeadata, "select PERID, PERNOME_P from PERIODICIDADES")
#           db_db_metadata <- db_db_metadata %>% 
#                 tbl_df %>% 
#                 left_join(., db_subject) %>% 
#                 left_join(., db_source) %>% 
#                 left_join(., db_period)
#           }
    
    observe({
        # if(input$idserie==""){
        updateSelectInput(session, "subject", label = "Tema", 
                          choices = c(levels(db_metadata$CATNOME)),
                          selected = db_metadata$CATNOME[1])
            # }
        })
    
    # observe({
    #     # if(input$idserie==""){
    #       updateSelectInput(session, "src", label = "Fonte",
    #                         choices = c("Selecione uma fonte" = "", levels(db_metadata$FNTNOME_P)),
    #                         selected = "")
    #     # }
    #     })
    
    observe({
        # if(input$idserie==""){
          db_metadata1 <- droplevels(subset(db_metadata, grepl(input$subject, db_metadata$CATNOME)))
          updateSelectInput(session, "src", label = "Fonte", 
                            choices = c("Selecione uma fonte" = "", levels(db_metadata1$FNTNOME_P)),
                            selected = "")
        # }
          
          # if(!input$idserie==""){
        #     db_metadata1 <- droplevels(subset(db_metadata, grepl(input$subject, db_metadata$CATNOME)))
        #     updateSelectInput(session, "src", label = "Fonte", 
        #                       choices = levels(db_metadata1$FNTNOME_P),
        #                       selected = levels(db_metadata$FNTNOME_P[db_metadata$SERCODIGOTROLL==input$idserie]))
        # }
        
        })
    
    output$serie_source <- renderText({
        
        if(!input$idserie==""){
        paste("Fonte:", db_metadata$FNTNOME_P[db_metadata$SERCODIGOTROLL==input$idserie])
        }
        })
    
    output$serie_periodicidade <- renderText({
        
        if(!input$idserie==""){
        paste("Fonte:", db_metadata$FNTNOME_P[db_metadata$SERCODIGOTROLL==input$idserie])
        }
        })
    
    
    # observe({
    #     # if(input$idserie==""){
    #       updateSelectInput(session, "period", label = "Periodicidade", 
    #                         choices = c("Selecione uma periodicidade" = "", levels(db_metadata$PERNOME_P)),
    #                         selected = "")
    #     # }
    #         })
    
    observe({
        # if(input$idserie==""){
          db_metadata1 <- droplevels(subset(db_metadata, grepl(input$subject, db_metadata$CATNOME) & grepl(input$src, db_metadata$FNTNOME_P)))
          updateSelectInput(session, "period", label = "Periodicidade", 
                            choices = c("Selecione uma periodicidade" = "", levels(db_metadata1$PERNOME_P)),
                            selected = "")
        # }
          })
    
    # observe({
    #       updateSelectInput(session, "idserie", label = "ID da serie", 
    #                         choices = c("Selecione uma série" = "", levels(db_metadata$SERCODIGOTROLL)),
    #                         selected = "")
    # })
    
    observe({
        # if(input$idserie==""){
          db_metadata1 <- droplevels(subset(db_metadata, grepl(input$subject, db_metadata$CATNOME) & grepl(input$src, db_metadata$FNTNOME_P) & grepl(input$period, db_metadata$PERNOME_P)))
          updateSelectInput(session, "idserie", label = "ID da serie", 
                            choices = c("Selecione uma série histórica" = "", levels(db_metadata1$SERCODIGOTROLL)),
                            selected = "")
          # }
        })
    
    
    observe({
        updateSelectInput(session, "geolevel", label = "Escolha o nivel geográfico", 
                          choices = c("Selecione um nível geofráfico" = "", levels(factor(statement()$TNIVNOME))),
                          selected = "")})
    
    observe({
        db_metadata1 <- droplevels(subset(statement(), grepl(input$geolevel, statement()$TNIVNOME)))
        updateSelectInput(session, "geoscope", label = "Escolha a abrangência", 
                          choices = c("Selecione uma abrangência" = "", levels(factor(db_metadata1$TERNOME))),
                          selected = "")})
    
    # output$src <- renderUI({
    #     data_src <- NULL
    #     if(!is.null(input$idserie)){
    #         data_src <- db_metadata$FNTNOME_P[db_metadata$SERCODIGOTROLL==input$idserie]
    #         return(data_src)}
    #     selectInput('src', 'Fonte', levels(factor(db_metadata$FNTNOME_P)), selected = data_src, width = "100%")
    # })
    
    # observe({
    #     updateSelectInput(session, "src", label = "Fonte", 
    #                       choices = c("", levels(db_metadata$FNTNOME_P)),
    #                       selected = db_metadata$FNTNOME_P[db_metadata$SERCODIGOTROLL==input$idserie])})
    # 
    # observe({
    #     updateSelectInput(session, "period", label = "Periodicidade", 
    #                       choices = c("", levels(db_metadata$PERNOME_P)),
    #                       selected = db_metadata$PERNOME_P[db_metadata$SERCODIGOTROLL==input$idserie])})
    # 
    # observe({
    #     updateSelectInput(session, "subject", label = "Tema", 
    #                       choices = c("", levels(db_metadata$CATNOME)),
    #                       selected = db_metadata$CATNOME[db_metadata$SERCODIGOTROLL==input$idserie])})
    

output$plot_title <- renderText({ 
          
          db_title <- sqlQuery(channel = ipeadata, sprintf("SELECT SERNOME_P
                            FROM SERIES WHERE SERCODIGOTROLL='%s';", input$idserie))
          
          ifelse(input$idserie=="", 
                 "Selecione uma série histórica",
                 as.character(db_title$SERNOME_P)
                 )
          })
    
    output$table_title <- renderText({ 
          
          db_title <- sqlQuery(channel = ipeadata, sprintf("SELECT SERNOME_P
                            FROM SERIES WHERE SERCODIGOTROLL='%s';", input$idserie))
          
          ifelse(input$idserie=="", 
                 "Selecione uma série histórica",
                 as.character(db_title$SERNOME_P)
          )
    })
    
    output$db_plot <- renderDygraph({
        
        if(input$subject=="Macroeconômico"){
        
              validate(
                    need(input$idserie, "Por favor, selecione uma série histórica."))
              
              # db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
        db <- statement()
        db <- db %>% 
            select(-TERNOME, -TERCODIGO, -TNIVNOME, -TNIVID, -SERCODIGO) %>% 
            setNames(tolower(names(.))) %>% 
            mutate(valdata = as.Date(valdata, origin = "1900-01-01"))
        
        return(xts(db$valvalor, order.by = as.Date(db$valdata, format = "%Y-%m-%d")) %>%
            setNames(., c("valvalor")) %>%
            dygraph(.) %>% 
            # dySeries(c("li", "valvalor", "ls"), label = "Valor", drawPoints = T) %>%
            dyRangeSelector() %>% 
            dySeries("valvalor", label = "Valor", drawPoints = TRUE) %>% 
            dyOptions(connectSeparatedPoints = FALSE))
        }

        if(input$subject=="Regional" | input$subject=="Social"){

            validate(
                need(input$geolevel, "Por favor, selecione o nível geográfico e a abragência."))

            validate(
                need(input$geoscope, "Por favor, selecione o nível geográfico e a abragência."))
            
            db <- statement()
            db <- db %>%
                tbl_df %>%
                filter(TNIVNOME==input$geolevel, TERNOME %in% input$geoscope) %>%
                select(VALDATA, VALVALOR, TERNOME) %>%
                mutate(VALDATA = as.Date(VALDATA, origin = "1900-01-01")) %>%
                dcast(., VALDATA ~ TERNOME, value.var = "VALVALOR")

            return(xts(db[-1], order.by = as.Date(db$VALDATA, format = "%Y-%m-%d")) %>%
                       dygraph %>%
                       dyLegend(show = "follow") %>%
                       dyRangeSelector() %>%
                       dySeries(drawPoints = TRUE) %>%
                       dyOptions(connectSeparatedPoints = FALSE))
            }
               
#         xts(db$valvalor, order.by = as.Date(db$valdata, format = "%Y-%m-%d")) %>%
#             setNames(., c("valvalor")) %>%
#             dygraph(.) %>% 
# #             dySeries(c("li", "valvalor", "ls"), label = "Valor", drawPoints = T) %>%
#             dyRangeSelector() %>% 
#             dySeries("valvalor", label = "Valor", drawPoints = TRUE) %>% 
#             dyOptions(connectSeparatedPoints = FALSE)
    })
    
    output$tb1 <- DT::renderDataTable({
          
          
          if(input$subject=="Macroeconômico"){
                
                # db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
        db <- statement()
        db <- db %>%
            select(-TERCODIGO, -TNIVID, -TNIVNOME, -TERNOME) %>% 
            mutate(VALDATA = as.Date(VALDATA, origin = "1900-01-01"))
        
        return(datatable(db, options = list(width = 12, pageLength = 12), 
                         rownames= FALSE, filter = "bottom"))
        
          }
        
        if((input$subject=="Regional" | input$subject=="Social") & input$geolevel==""){
              
              return()
        }
        
        if(!input$geolevel==""){
              
              # db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
            db_geo <- statement()
            db_geo <- db_geo %>%
                filter(TNIVNOME==input$geolevel, TERNOME %in% input$geoscope) %>%
                select(-TERCODIGO, -TNIVID, -TNIVNOME, -TERNOME) %>% 
                mutate(VALDATA = as.Date(VALDATA, origin = "1900-01-01"))
            return(datatable(db_geo, options = list(width = 12, pageLength = 12), rownames= FALSE, filter = 'top'))
            }
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
        when ser.SERTIPO = 'N' then 'Macroeconomico'
        when ser.SERTIPO = 'R' and ser.CATID = 1 then 'Regional'
        when ser.SERTIPO = 'R' and ser.CATID = 2 then 'Social'
        Else 'ERRO!'
    END As SERBASE
    FROM ipeadata.dbo.SERIES ser LEFT OUTER JOIN ipeadata.dbo.FONTES f ON ser.FNTID = f.FNTID 
    WHERE ser.SERTIPO = 'N' AND ser.SERNUMERICA = 1 and ser.SERSTATUS='A' and ser.SERMAXDATA < getdate() and ser.SERMAXDATA>0
    order by Atraso;")
    })

    output$db_nrows <- renderValueBox({
    
        # db <- sqlQuery(channel = ipeadata, query = statement(), rows_at_time = 10)
        db <- statement()
        db <- db %>%
            setNames(tolower(names(.))) %>%
            mutate(valdata = as.Date(valdata, origin = "1900-01-01"))
        
    valueBox(
        ifelse(input$idserie=="", "Nenhuma", nrow(db)),
        tags$b("Observacoes"),
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
            tags$b("Valores ausentes"), icon = icon("question"), color = "orange")

        })
    
    output$map <- renderLeaflet({
                leaflet() %>% 
            setView(lng = -56.4568744, lat = -12.5713749, zoom = 4) %>% 
            addProviderTiles("Esri.WorldStreetMap", group = "ESRI")
            # addProviderTiles("Imagery", group = "Mapbox")
        
    })
    
    
    })
    