shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=280*1024^2) 
  
  modal <- modalDialog(
    title = "Update 16-Oct",
    easyClose = T,
    h4("Bienvenido!"),
    h6("Ahora podes subir tu propia capa espacial en formato CSV o SHP y volcar tu informacion en la malla hexagonal. Las layers nuevas que creas estarán disponibles para ser utilizadas por todas las herramientas durante la sesión."),
    h6("- Nuevo feature: Regresión espacialmente ponderada (GWR)."),
    h6("18-Oct: Bugfix en upload de capa de puntos!"),
    h6("21-Oct: Matriz de correlaciones"),
    
    footer = modalButton("Entendido"))
    

  # Show the model on start up ...
  showModal(modal)
  

  pTyper <- reactive({input$pType})
  lista1r <- reactive({input$lista1})
  lista2r <- reactive({input$lista2})
  lista3r <- reactive({input$lista3})
  lista4r <- reactive({input$lista4})
  listaAr <- reactive({input$listaA})
  listaBr <- reactive({input$listaB})

  vardepr <- reactive({input$vardep})
  selectizer <- reactive({input$selectize})
  dimensionesr <- reactive({input$dimensiones})
  
  vardeprGWR <- reactive({input$vardepGWR})
  selectizerGWR <- reactive({input$selectizeGWR})

  selectizeCorrplot <- reactive({input$selectizeCorrplot})
  
  
  
  
  datosCSVreactive <- reactiveVal(datosCSV)
  datosCSVboxcoxreactive <- reactiveVal(datosCSVboxcox)
  listavarlab <- reactiveVal(as.vector(lista))


  observeEvent(input$clusterizar, {
    datosCSV <-  datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()

    k<-input$nclusters
    paleta <- palette(magma(k))
  

    if (input$clusterboxcox == TRUE) {
    varcluster <- c(sapply(input$dimensiones, DataType, simplify = TRUE, USE.NAMES = FALSE))
    dat<- datosCSVboxcox[,c("hexacities",varcluster)]
    dat[is.na(dat)] <- 0
    


        dat <- as.data.frame(apply(dat, 2, Normalizar))
        
       
     } else {
       
       varcluster <- c(sapply(input$dimensiones, DataType, simplify = TRUE, USE.NAMES = FALSE))
       dat<- datosCSV[,c("hexacities",varcluster)]
       dat[is.na(dat)] <- 0
       
       dat <- as.data.frame(apply(dat, 2, Normalizar))
       
     }
  
    
    dat$hexacities <- datosCSV$hexacities
    
    
    D0 <- dist(dat[ ,!(colnames(dat) == "hexacities")]) # the socio-demographic distances
    D1 <- as.dist(D.geo)
    

    tree <- hclustgeo(D0,D1,alpha=input$alpha)
    dat$P5bis <- cutree(tree,k)
    
    datmap <- dat[,c("hexacities","P5bis")] 
    

    map@data <- merge(x = map@data, y=datmap, by="hexacities", all.x = TRUE)
    map <-subset(map, !is.na(map@data[,2]))
    map <- map[!is.na(map[,2])]
    k<-input$nclusters
    
    paleta <- palette(magma(k))
    
    
    leafletProxy("map4", data = map)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      clearShapes() %>%
      addPolygons(data = map, layerId = ~map$hexacities, fillColor = paleta[as.numeric(map@data$P5bis)],fillOpacity = 0.6,
                  color = "white", weight = 0,popup=paste0("Cluster ", map@data[,2]))%>%
      clearControls()%>%
    addLegend("topleft",
              colors =as.vector(paleta[unique(as.numeric(map@data$P5bis))]),
              labels= as.vector(paste0("Cluster ", unique(map@data$P5bis))),
              title= ("Clusters"),
              opacity = 0.5)
    
    
  })
    
  
  
    observeEvent(input$boton2, {
    
      datosCSV <- datosCSVreactive()
      datosCSVboxcox <- datosCSVboxcoxreactive()
      
    vd <- DataType(input$vardep)
    vi <- c(sapply(input$selectize, DataType, simplify = TRUE, USE.NAMES = FALSE))
    
    datosCSVin <- subset(datosCSV, BARRIO %in% input$barrioin)
   
    datosCSVout <-  subset(datosCSV, BARRIO %in% input$barrioout)
    # idsOut <- unique(datosCSVout$hexacities)
    
    fmla <- paste(vd, paste(vi, collapse = " + "), sep = "~")
    lmodel <- lm(fmla, data = datosCSVin)
   
    
    cooksd <- cooks.distance(lmodel)
    datosCSVin$cooksd <- cooks.distance(lmodel)
    
    adentro <- nrow(datosCSVin) - input$cookometro
    datosCSVin <- datosCSVin[order(cooksd),]  
    atroden <- datosCSVin[1:adentro,c("hexacities")]
    # datosCSVin2 <-subset(datosCSVin, !(hexacities %in% influential$hexacities))
    # datosCSVin2 <- datosCSVin[!(datosCSVin$hexacities %in% as.vector(influential)),]
    datosCSVin2 <-subset(datosCSVin, hexacities %in% atroden)
    # influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))
    # datosCSVin <- datosCSVin[datosCSVin$hexacities - filterout$hexacities, ]
    lmodel2 <- lm(fmla, data = datosCSVin2)
    cooksd2 <- cooks.distance(lmodel2)
    
    
    # summary(lmodel2)
    
    if (input$resorfit == "Residual") {
      # datosCSVout$picked0 <- predict(lmodel2, datosCSVout)
      datosCSVout$picked <- as.vector(datosCSVout[,paste0(DataType(input$vardep))]) - as.vector(predict(lmodel2, datosCSVout))
      fitres <- datosCSVout[,c("hexacities","picked"),drop=F]
      # fitres <-subset(fitres0, hexacities %in% atroden)
      
      # as.data.frame(fitres)
    } 
    
    else if(input$resorfit == "Cook's distance") {
      datosCSVin2$picked <- datosCSVin2$cooksd
      # datosCSVout0 <- as.data.frame(merge(datosCSVin[, c("hexacities","picked")], datosCSVout, by="hexacities"))
      fitres0 <- datosCSVin2[,c("hexacities","picked"),drop=F]
      fitres <-subset(fitres0, hexacities %in% atroden)
    } 
      else {
        datosCSVout$picked <- predict(lmodel2, datosCSVout)
        fitres <- datosCSVout[,c("hexacities","picked"),drop=F]
        
        
      }     

         shapeFile@data <- merge(x = shapeFile@data, y=fitres, by="hexacities", all.x = TRUE)
         shapeFile <-subset(shapeFile, !is.na(shapeFile@data[,2]))
         shapeFile <- shapeFile[!is.na(shapeFile[,2])]
         
         # shapeFile <- shapeFile[!is.na(shapeFile@data[,2]),]
         
         rc1 <- colorRampPalette(colors = c("blue", "white"), space = "Lab")((sqrt(min(fitres$picked)^2))/max(sqrt(fitres$picked^2))*10)
         
         rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab")((max(fitres$picked))/max(sqrt(fitres$picked^2))*10)
         
         rampcols <- c(rc1, rc2)
         
         mypal <- colorNumeric(palette = rampcols, domain = fitres$picked)
         
         
         # fitres$picked

         # fillColor = if(input$resorfit == "Residual"){~mypal(fitres$picked)}else{"Red"}, fillOpacity = if(input$resorfit == "Residual"){sqrt(sqrt((fitres$picked)^2)/max(sqrt((fitres$picked)^2)))}else{(shapeFile@data[,2]+0.00001)/max(shapeFile@data[,2])},
         

         leafletProxy("map2", data = shapeFile)%>%
         addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
         clearShapes() %>%
         addPolygons(data = shapeFile, layerId = ~shapeFile$hexacities, fillColor = if(input$resorfit == "Residual"){~mypal(fitres$picked)}else{"Red"}, fillOpacity = if(input$resorfit == "Residual"){sqrt(sqrt((fitres$picked)^2)/max(sqrt((fitres$picked)^2)))}else{(shapeFile@data[,2]+0.00001)/max(shapeFile@data[,2])},
                       color = "white", weight = 0,popup=paste(shapeFile@data[,2])) %>%
         clearControls() %>%
                     addLegend("topleft",
                     colors =if(input$resorfit == "Residual"){c("#ff0000", "#ff8686", "#fff", "#7585ff", "#001eff")}else{c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9")},
                     labels= c("+", "","","", "-"),
                     title= (input$resorfit),
                     opacity = 1)

         
    output$prediction <- renderPrint({
        return(summary(lmodel2))
    
  })
    
   
    
    output$residvsfit <- renderPlotly({
    Fitted.Values <- predict(lmodel2, datosCSVout)
    Residuals <- as.vector(datosCSVout[,paste0(DataType(input$vardep))]) - predict(lmodel2, datosCSVout)
      

    regMat <- data.frame(Fitted.Values, 
                         Residuals)
    
    
    LOESS1 <- loess.smooth(Fitted.Values, Residuals)
    
    plt1 <- regMat %>% 
      plot_ly(x = Fitted.Values, y = Residuals, 
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
      
      add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
                line = list(width = 2)) %>% 
      
      layout(title = "Residuales vs Fitted", plot_bgcolor = "#e6e6e6", width = 750, height = 300)
    
    })
    
    output$cooksdistance <- renderPlot({
    

      sample_size <- nrow(datosCSVin2)
      plot(sample(datosCSVin2$cooksd, sample_size,replace=FALSE), pch="*", cex=2, main="Observaciones influyentes por distancia de Cook")  # plot cook's distance
      abline(h = 4/sample_size, col="red")  
       # text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red")  # labels
    
    
    })
  })
  
    
    
    
    output$Corrplot<- renderPlot({
      
      datosCSV <- datosCSVreactive()
      
      vi <- c(sapply(input$selectizeCorrplot, DataType, simplify = TRUE, USE.NAMES = FALSE))
      
      datosCSV <-datosCSV[,vi]
      
      colnames(datosCSV)<- c(lapply(input$selectizeCorrplot, substr,start=1, stop=25 ))
      
      corrplot.mixed(cor(datosCSV), tl.col="black", tl.pos = "lt", tl.cex=0.8, tl.srt=70,lower = "ellipse", upper="number" )
      

       })

    
    
    gwrmodel <- eventReactive(input$boton2GWR,{

       datosCSV <- datosCSVreactive()
       datosCSVboxcox <- datosCSVboxcoxreactive()

       vd <- DataType(input$vardepGWR)
       vi <- c(sapply(input$selectizeGWR, DataType, simplify = TRUE, USE.NAMES = FALSE))


       fmla <- paste(vd, paste(vi, collapse = " + "), sep = "~")

       DM <- gw.dist(dp.locat=coordinates(shapeFile))


      shapeFile@data <- merge(x = shapeFile@data, y=datosCSV, by.x="hexacities", by.y="hexacitiesid", all.x = TRUE)



       bandwi <-bw.gwr(fmla,data=shapeFile, approach="CV",kernel="gaussian",
                       adaptive=FALSE,dMat=DM, p=2, theta=0, longlat=F)

       gwrmodel <- gwr.basic(fmla, data=shapeFile,bw=bandwi, kernel='gaussian',dMat=DM)

      
      
    })
    
    
    
    observe({
      
      if(!is.null(input$resorfitGWR)){
        
        gwrmodel<-gwrmodel()
        
        gwrshapeFile<-gwrmodel$SDF
        
        
         sel <- paste0(input$resorfitGWR)
        
        gwrshapeFile <- gwrshapeFile[,sel]
        
        
        rc1 <- colorRampPalette(colors = c("blue", "white"), space = "Lab")((sqrt(min(gwrshapeFile@data[,1])^2))/max(sqrt(gwrshapeFile@data[,1]^2))*10)
        
        rc2 <- colorRampPalette(colors = c("white", "red"), space = "Lab")((max(gwrshapeFile@data[,1]))/max(sqrt(gwrshapeFile@data[,1]^2))*10)
        
        rampcols <- c(rc1, rc2)
        
        mypal <- colorNumeric(palette = rampcols, domain = gwrshapeFile@data[,1])
        
        
        
          leafletProxy("map2", data = gwrshapeFile)%>%
            addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
            clearShapes() %>%
            addPolygons(data = gwrshapeFile, fillColor = if(min(gwrshapeFile@data[,1]) < 0){~mypal(gwrshapeFile@data[,1])}else{"Red"}, fillOpacity = if(min(gwrshapeFile@data[,1]) < 0){sqrt(sqrt((gwrshapeFile@data[,1])^2)/max(sqrt((gwrshapeFile@data[,1])^2)))}else{(gwrshapeFile@data[,1]+0.00001)/max(gwrshapeFile@data[,1])},

                        color = "white", weight = 0,popup=paste(gwrshapeFile@data[,1])) %>%
            clearControls() %>%
            addLegend("topleft",
                      colors =if(min(gwrshapeFile@data[,1]) < 0){c("#ff0000", "#ff8686", "#fff", "#7585ff", "#001eff")}else{c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9")},
                      labels= c("+", "","","", "-"),
                      title= (input$resorfitGWR),
                      opacity = 1)
        
      }})
    
    

    
    output$resorfitGWR <- renderUI({
      
      gwrmodel<-gwrmodel()
      
      gwrshapeFile<-gwrmodel$SDF
      
      selectInput("resorfitGWR", "Resultado a mapear", colnames(gwrshapeFile@data), selected = "yhat",width = 200)
      
      
       })
    
    
    
    
    output$predictionGWR <- renderPrint({
      gwrmodel<-gwrmodel()
      

      return(gwrmodel)

      
        })
    
    
     
         # observeEvent(input$boton2GWR, {
    #   
     #  datosCSV <- datosCSVreactive()
     #  datosCSVboxcox <- datosCSVboxcoxreactive()
     # 
     #  vd <- DataType(input$vardepGWR)
     #  vi <- c(sapply(input$selectizeGWR, DataType, simplify = TRUE, USE.NAMES = FALSE))
     # 
     #  # datosCSVin <- subset(datosCSV, BARRIO %in% input$barrioin)
     #  # datosCSVout <-  subset(datosCSV, BARRIO %in% input$barrioout)
     #  # idsOut <- unique(datosCSVout$hexacities)
     # 
     # 
     #  fmla <- paste(vd, paste(vi, collapse = " + "), sep = "~")
     # 
     #  DM <- gw.dist(dp.locat=coordinates(shapeFile))
     # 
     # 
     # shapeFile@data <- merge(x = shapeFile@data, y=datosCSV, by.x="hexacities", by.y="hexacitiesid", all.x = TRUE)
     # 
     # 
     # 
     #  bandwi <-bw.gwr(fmla,data=shapeFile, approach="CV",kernel="gaussian",
     #                  adaptive=FALSE,dMat=DM, p=2, theta=0, longlat=F)
     # 
     #  gwrmodel <- gwr.basic(fmla, data=shapeFile,bw=bandwi, kernel='gaussian',dMat=DM)
     # 
     #  gwrshapeFile<-gwrmodel$SDF
    #   
    #   
    #   
    #   
    #   
    #   leafletProxy("map2", data = gwrshapeFile)%>%
    #     addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
    #     clearShapes() %>%
    #     addPolygons(data = gwrshapeFile, fillColor = "Red", fillOpacity = gwrshapeFile@data$yhat / max(gwrshapeFile@data$yhat),
    #                 color = "white", weight = 0,popup=paste(gwrshapeFile@data$yhat)) %>%
    #     clearControls() %>%
    #     addLegend("topleft",
    #               colors =if(input$resorfit == "Residual"){c("#ff0000", "#ff8686", "#fff", "#7585ff", "#001eff")}else{c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9")},
    #               labels= c("+", "","","", "-"),
    #               title= (input$resorfit),
    #               opacity = 1)
    #   
    #   
    #   updateSelectInput(session,"resorfitGWR", choices = colnames(gwrshapeFile@data), selected = "yhat")
    #   
    #   
    #   
    #   
    #   
    #   output$predictionGWR <- renderPrint({
    #     return(gwrmodel)
    #     
    #   })
    #   
    #   
    # 
    #  })
    
    
    
    
    
  
  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 12)) %>%
      setView(lng = -58.3503847, lat =-34.6198848, zoom = 12)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
    addLegend("topleft",
              colors =c("#ff0000", "#ff8686", "#fff", "#7585ff", "#001eff"),
              labels= c("+", "","","", "-"),
              title= ("Residual"),
              opacity = 1)
    
    
  })
  
  
  

  
  

  
  
  
  ############################################################  
  
  indiceA <- reactive({
    datosCSV <-  datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    
    
    req(input$transA)
    
    if (input$transA == "log10") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data[,2] <- log10(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$transA == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$transA == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$transA == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$transA == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$transA == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaA))),drop=F]
      
      indice1data2 <- indice1data
    }
    
    
  })
  
  
  indiceAb <-  reactive({
    
    
    if (input$direccionA == TRUE) {
      df1   <- Invertir(indiceA(),2)}
    else {
      df1 <- indiceA()
    }
  })
  
  
  indiceB <- reactive({
    datosCSV <-  datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    req(input$transB)
    
    if (input$transB == "log10") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data[,2] <- log10(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$transB == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$transB == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$transB == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$transB == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$transB == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$listaB))),drop=F]
      
      indice1data2 <- indice1data
    }
    
    
  })
  
  indiceBb <-  reactive({
    
    
    if (input$direccionB == TRUE) {
      df1   <- Invertir(indiceB(),2)}
    else {
      df1 <- indiceB()
    }
  })
  
  ############################################################  
    indice1 <- reactive({
 
      datosCSV<-datosCSVreactive()
      datosCSVboxcox <- datosCSVboxcoxreactive()
      
    req(input$trans1)

    if (input$trans1 == "log10") {
       indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
       indice1data[,2] <- log10(indice1data[,2])
       indice1data2 <- indice1data
    }
    
    else if (input$trans1 == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }

    else if (input$trans1 == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$trans1 == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$trans1 == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$trans1 == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista1))),drop=F]
      
      indice1data2 <- indice1data
    }
      

  })
  
  
  indice1b <-  reactive({
  

    if (input$direccion1 == TRUE) {
      df1   <- Invertir(indice1(),2)}
  else {
    df1 <- indice1()
  }
    })
  

  indice2 <- reactive({
    datosCSV<-datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    req(input$trans2)
    
    if (input$trans2 == "log10") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data[,2] <- log10(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans2 == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans2 == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$trans2 == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$trans2 == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$trans2 == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista2))),drop=F]
      
      indice1data2 <- indice1data
    }
    
    
  })
  
  indice2b <-  reactive({
    
    
    if (input$direccion2 == TRUE) {
      df1   <- Invertir(indice2(),2)}
    else {
      df1 <- indice2()
    }
  })
  
  indice3 <- reactive({
    datosCSV<-datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    req(input$trans3)
    
    if (input$trans3 == "log10") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data[,2] <- log10(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans3 == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans3 == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$trans3 == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$trans3 == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$trans3 == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista3))),drop=F]
      
      indice1data2 <- indice1data
    }
    
    
  })
  
  indice3b <-  reactive({


    if (input$direccion3 == TRUE) {
      df1   <- Invertir(indice3(),2)}
    else {
      df1 <- indice3()
    }
  })
  
  
  
  
  indice4 <- reactive({
    datosCSV<-datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    req(input$trans4)
    
    if (input$trans4 == "log10") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data[,2] <- log10(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans4 == "Raiz cuadrada") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data[,2] <- sqrt(indice1data[,2])
      indice1data2 <- indice1data
    }
    
    else if (input$trans4 == "^2") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^2
      indice1data2 <- indice1data
    }
    
    else if (input$trans4 == "^1/3") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data[,2] <- (indice1data[,2])^(1/3)
      indice1data2 <- indice1data
    }
    
    else if (input$trans4 == "1/x") {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data[,2] <- 1/(indice1data[,2])
      indice1data2 <- indice1data
    }
    else if (input$trans4 == "boxcox") {
      indice1data <- datosCSVboxcox[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      indice1data2 <- indice1data
    }
    
    else
    {
      indice1data <- datosCSV[,c("hexacities", paste0(DataType(input$lista4))),drop=F]
      
      indice1data2 <- indice1data
    }
    
    
  })
  indice4b <-  reactive({
    
    
    if (input$direccion4 == TRUE) {
      df1   <- Invertir(indice4(),2)}
    else {
      df1 <- indice4()
    }
  })
  
  
  ############################################################
  output$histogramaA <- renderPlotly({
    histogramaAdata  <- indiceAb()
    
    
    
    histogramaAdata2 <- histogramaAdata[,2,drop=F]
    as.data.frame(names(histogramaAdata2)[1]<-"columna")
    histogramaAfit <-density(histogramaAdata2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histogramaAfit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histogramaAfit$x, y = histogramaAfit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histogramaAfit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  
  
  output$histogramaB <- renderPlotly({
    histogramaAdata  <- indiceBb()
    
    histogramaAdata2 <- histogramaAdata[,2,drop=F]
    as.data.frame(names(histogramaAdata2)[1]<-"columna")
    histogramaAfit <-density(histogramaAdata2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histogramaAfit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histogramaAfit$x, y = histogramaAfit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histogramaAfit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  ############################################################
  
  
  output$histograma1 <- renderPlotly({
    histograma1data  <- indice1b()
    


    histograma1data2 <- histograma1data[,2,drop=F]
    as.data.frame(names(histograma1data2)[1]<-"columna")
    histograma1fit <-density(histograma1data2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histograma1fit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histograma1fit$x, y = histograma1fit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histograma1fit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  
  
  output$histograma2 <- renderPlotly({
    histograma1data  <- indice2b()

    histograma1data2 <- histograma1data[,2,drop=F]
    as.data.frame(names(histograma1data2)[1]<-"columna")
    histograma1fit <-density(histograma1data2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histograma1fit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histograma1fit$x, y = histograma1fit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histograma1fit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  
  
  
  output$histograma3 <- renderPlotly({
    histograma1data  <- indice3b()
    

    histograma1data2 <- histograma1data[,2,drop=F]
    as.data.frame(names(histograma1data2)[1]<-"columna")
    histograma1fit <-density(histograma1data2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histograma1fit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histograma1fit$x, y = histograma1fit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histograma1fit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                             titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  
  
  
  output$histograma4 <- renderPlotly({
    histograma1data  <- indice4b()
    
    
    histograma1data2 <- histograma1data[,2,drop=F]
    as.data.frame(names(histograma1data2)[1]<-"columna")
    histograma1fit <-density(histograma1data2$columna)
    
    
    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )
    
    plot_ly(x = histograma1fit, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = histograma1fit$x, y = histograma1fit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(histograma1fit$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                              showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 150, height = 75, margin = m)
  })
  
  ################################# mapa #################################
  
  output$slider2 <- renderUI({
    
    datosCSV <- datosCSVreactive()
    
    sliderInput("slider2", "Filtrá Min-Max", min=min(datosCSV[,paste0(DataType(input$pType))]), max=max(datosCSV[,paste0(DataType(input$pType))]), value=c(0,max(datosCSV[,paste0(DataType(input$pType))])))
  })
  
  
selected <- eventReactive(input$slider2,{
  datosCSV <- datosCSVreactive()
  
    testdata <- datosCSV[,c("hexacities", paste0(DataType(input$pType))),drop=F]
    testdata2 <- testdata      
    testdata2[testdata2[,2] >= input$slider2[1] & testdata2[,2] <= input$slider2[2],]
    
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 12)) %>% 
      setView(lng = -58.418018, lat =-34.6159966, zoom = 12)
      # addProviderTiles(if(isTRUE(input$switch)){providers$Esri.WorldImagery}else{providers$CartoDB.Positron}, options = providerTileOptions(noWrap = TRUE))%>%
      
  
  })
  
  
  observe({
    if(!is.null(input$pType)){
      leafletProxy("map")%>%
        clearControls%>%
  addLegend("topleft",
            colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
            labels= c("Más", "","","", "Menos"),
            title= (input$pType),
            opacity = 1)
  }})
  
  
  observe({
    
    if(!is.null(input$switch)){
      leafletProxy("map")%>%
      
  addProviderTiles(if(isTRUE(input$switch)){providers$Esri.WorldImagery}else{providers$CartoDB.Positron}, options = providerTileOptions(noWrap = TRUE))
    
    }})
  
  
  observe({

    if(!is.null(input$slider2)){

      
      
      shapeFile@data <- merge(x = shapeFile@data, y=selected(), by="hexacities", all.x = TRUE)
      shapeFile@data[is.na(shapeFile@data)] <- 0

      leafletProxy("map", data = shapeFile)%>%
        clearShapes() %>%
        addPolygons(data = shapeFile, layerId = ~shapeFile$hexacities, fillColor = "Red", fillOpacity = (shapeFile@data[,2]-min(shapeFile@data[,2])) /max(shapeFile@data[,2]),
                    color = "white", weight = 0,popup=paste(shapeFile@data[,2]))

    }})



  output$map3 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 12)) %>%
      setView(lng = -58.3503847, lat =-34.6198848, zoom = 12)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      addLegend("topleft",
                colors =c("#00d844", "#52dd7e", "#82d89d", "#a2d6b2", "#c5dbcc"),
                labels= c("Mejor", "","","", "Peor"),
                title= ("Indice customizado"),
                opacity = 1)

  })

  
  output$map4 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 12)) %>%
      setView(lng = -58.3503847, lat =-34.6198848, zoom = 12)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))
      # addLegend("topleft",
      #           colors =c("#00d844", "#52dd7e", "#82d89d", "#a2d6b2", "#c5dbcc"),
      #           labels= c("Mejor", "","","", "Peor"),
      #           title= ("Clusters"),
      #           opacity = 0.5)
    
  })
  
  

  observeEvent(input$boton, {

    lista <- listavarlab()
    datosCSV <-  datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    
    i1  <- indice1b()
    i2  <- indice2b()
    i3  <- indice3b()
    i4  <- indice4b()


    indice1y2<- merge(x = i1, y=i2, by ="hexacities", all.x = TRUE)
    indice1y2y3 <- merge(x = indice1y2, y=i3, by = "hexacities", all.x = TRUE)
    indice1y2y3y4 <- merge(x = indice1y2y3, y=i4, by = "hexacities", all.x = TRUE)

    
    indice1y2y3y4$onenorm <- (indice1y2y3y4[,2] -min(indice1y2y3y4[,2]))/(max(indice1y2y3y4[,2])-min(indice1y2y3y4[,2]))
    indice1y2y3y4$twonorm <- (indice1y2y3y4[,3] -min(indice1y2y3y4[,3]))/(max(indice1y2y3y4[,3])-min(indice1y2y3y4[,3]))
    indice1y2y3y4$threenorm <- (indice1y2y3y4[,4] -min(indice1y2y3y4[,4]))/(max(indice1y2y3y4[,4])-min(indice1y2y3y4[,4]))
    indice1y2y3y4$fournorm <- (indice1y2y3y4[,5] -min(indice1y2y3y4[,5]))/(max(indice1y2y3y4[,5])-min(indice1y2y3y4[,5]))
    
    
    indice1y2y3y4$indiceponderado <- (( indice1y2y3y4$onenorm * input$pondera1) + (indice1y2y3y4$twonorm * input$pondera2) + (indice1y2y3y4$threenorm * input$pondera3) + (indice1y2y3y4$fournorm * input$pondera4))/(input$pondera1+input$pondera2+input$pondera3+input$pondera4)
    indice1y2y3y4$IndiceCustomizado <- (indice1y2y3y4$indiceponderado -min(indice1y2y3y4$indiceponderado))/(max(indice1y2y3y4$indiceponderado)-min(indice1y2y3y4$indiceponderado))

    tablaparamerge <-  indice1y2y3y4[,c("hexacities", "IndiceCustomizado"),drop=F]

    
    newdatosCSVreactive <- cbind(datosCSV, tablaparamerge)
    newdatosCSVboxcoxreactive <- cbind(datosCSVboxcox, tablaparamerge)
    
    datosCSVreactive(newdatosCSVreactive)
    datosCSVboxcoxreactive(newdatosCSVboxcoxreactive)
    
    listavarlab(as.vector(c(lista, "IndiceCustomizado")))

    
    updateSelectInput(session,"pType", choices = listavarlab(), selected = pTyper())
    updateSelectInput(session,"lista1", choices = listavarlab(), selected= lista1r() )
    updateSelectInput(session,"lista2", choices = listavarlab(), selected=lista2r() )
    updateSelectInput(session,"lista3", choices = listavarlab(), selected=lista3r() )
    updateSelectInput(session,"lista4", choices = listavarlab(), selected=lista4r() )
    
    updateSelectInput(session, "listaA", choices = listavarlab(), selected=listaAr() )
    updateSelectInput(session,"listaB", choices = listavarlab(), selected=listaBr() )
    
    updateSelectInput(session,"vardep", "Variable a explicar", choices = listavarlab(), selected = vardepr())
    updateSelectInput(session,"selectize", "Variables independientes", choices = listavarlab(),  selected = selectizer())
    updateSelectInput(session, "dimensiones", "Seleccione las dimensiones de entrada", choices = listavarlab(), selected= dimensionesr())
    
    updateSelectInput(session,"vardepGWR", "Variable a explicar", choices = listavarlab(), selected = vardeprGWR())
    updateSelectInput(session,"selectizeGWR", "Variables independientes", choices = listavarlab(),  selected = selectizerGWR())
    
    updateSelectInput(session,"selectizeCorrplot", "Variables:", choices = listavarlab(),  selected = selectizeCorrplot())
    

    
    
    shapeFile@data <- merge(x = shapeFile@data, y=tablaparamerge, by="hexacities", all.x = TRUE)
    shapeFile@data[is.na(shapeFile@data)] <- 0

    leafletProxy("map3", data = shapeFile)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      clearShapes() %>%
      addPolygons(data = shapeFile, layerId = ~shapeFile$hexacities, fillColor = "#00d844", fillOpacity = (shapeFile@data[,2]+0.001)/max(shapeFile@data[,2]),
                  color = "white", weight = 0,popup=paste(shapeFile@data[,2]))


  })


  
  output$map5 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 12)) %>%
      setView(lng = -58.3503847, lat =-34.6198848, zoom = 12)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      addLegend("topleft",
                colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
                labels= c("Más", "","","", "Menos"),
                title= ("Variable resultante"),
                opacity = 1)
    
  })
  
  

  observeEvent(input$varlab, {
    
    lista <- listavarlab()
    datosCSV <-  datosCSVreactive()
    datosCSVboxcox <- datosCSVboxcoxreactive()
    
    
   
   
    iA  <- indiceAb()
    iB  <- indiceBb()
    indiceAyB<- merge(x = iA, y=iB, by ="hexacities", all.x = TRUE)
    
    if (input$combType == "Multiplicacion") {
      
      indiceAyB$resultante <-  indiceAyB[,2] * indiceAyB[,3]
 
    } 
    
    else if(input$combType == "Division") {
      indiceAyB$resultante <-  indiceAyB[,2] / indiceAyB[,3]
      
      
    } 
    
    if (input$combType == "Suma") {
      indiceAyB$resultante <-  indiceAyB[,2] + indiceAyB[,3]
      
    } 
    
    else if(input$combType == "Resta") {
      indiceAyB$resultante <-  indiceAyB[,2] - indiceAyB[,3]
      
    } 
    
    label1 <- paste(DataType(input$listaA), input$combType, DataType(input$listaB), sep="_" )
    colnames(indiceAyB)[4] <- toString(label1)
    
    indiceAyB <-  indiceAyB[,c("hexacities", label1), drop = FALSE]
    
    is.na(indiceAyB)<-sapply(indiceAyB, is.infinite)
    indiceAyB[is.na(indiceAyB)]<-0
    
    
    


    
    
    lambda <- getLambda(indiceAyB, lambda = seq(-10, 10, 1/100), parallel = TRUE)
    
    indiceAyBboxcox <-  normaliseData(indiceAyB, lambda)
     
    newdatosCSVboxcoxreactive <- cbind(datosCSVboxcox, indiceAyBboxcox)
    datosCSVboxcoxreactive(newdatosCSVboxcoxreactive)
    
    
    
    newdatosCSVreactive <- cbind(datosCSV, indiceAyB)
    
    datosCSVreactive(newdatosCSVreactive)
    

    
    # lista1r<-lista1r()
    
    
    listavarlab(as.vector(c(lista, label1)))
    

    updateSelectInput(session,"pType", choices = listavarlab(), selected = pTyper())
    updateSelectInput(session,"lista1", choices = listavarlab(), selected= lista1r() )
    updateSelectInput(session,"lista2", choices = listavarlab(), selected=lista2r() )
    updateSelectInput(session,"lista3", choices = listavarlab(), selected=lista3r() )
    updateSelectInput(session,"lista4", choices = listavarlab(), selected=lista4r() )
    
    updateSelectInput(session, "listaA", choices = listavarlab(), selected=listaAr() )
    updateSelectInput(session,"listaB", choices = listavarlab(), selected=listaBr() )
    
    updateSelectInput(session,"vardep", "Variable a explicar", choices = listavarlab(), selected = vardepr())
    updateSelectInput(session,"selectize", "Variables independientes", choices = listavarlab(),  selected = selectizer())
    updateSelectInput(session, "dimensiones", "Seleccione las dimensiones de entrada", choices = listavarlab(), selected= dimensionesr())
    
    updateSelectInput(session,"vardepGWR", "Variable a explicar", choices = listavarlab(), selected = vardeprGWR())
    updateSelectInput(session,"selectizeGWR", "Variables independientes", choices = listavarlab(),  selected = selectizerGWR())
    
    updateSelectInput(session,"selectizeCorrplot", "Variables:", choices = listavarlab(),  selected = selectizeCorrplot())
    
    
    

    shapeFile@data <- merge(x = shapeFile@data, y=indiceAyB, by="hexacities", all.x = TRUE)
    shapeFile@data[is.na(shapeFile@data)] <- 0
    
    leafletProxy("map5", data = shapeFile)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      clearShapes() %>%
      addPolygons(data = shapeFile, layerId = ~shapeFile$hexacities, fillColor = "Red", fillOpacity = (shapeFile@data[,2]+0.001)/max(shapeFile@data[,2]),
                  color = "white", weight = 0,popup=paste(shapeFile@data[,2])) %>%
      clearControls()%>%
      addLegend("topleft",
                colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
                labels= c("Más", "","","", "Menos"),
              title= paste(label1),
              opacity = 1)
    
  
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################# mapa fin #################################
  
  
  output$areaPlot <- renderPlotly({
    l  <- selected()
    
    x <- l[,2,drop=F]
    as.data.frame(names(x)[1]<-"columna")
    fit <-density(x$columna)
    
    m <- list(
      l = 0,
      r = 5,
      b = 0,
      t = 20
    )
    
    plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
      config(displayModeBar = F) %>% 
      add_trace(cliponaxis = TRUE, x = fit$x, y = fit$y, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', fillcolor = '#0782e0',fill = "tozeroy") %>% 
      layout( title = "Histograma (densidad)",font=list(family = "Arial", color="black", size=9), xaxis = list(        showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = FALSE,
                                   zeroline = TRUE,
                                   zerolinecolor = 'rgb(200, 200, 200)',
                                   range = c(0, max(x$columna)),
                                   layer = 'below traces'),
              yaxis = list(showgrid = FALSE,
                           showline = FALSE,
                           showticklabels = FALSE,
                           zeroline = TRUE,
                           zerolinecolor = 'rgb(200, 200, 200)',
                           tickfont = list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           showtitle = FALSE,
                           titlefont =list(
                             family = "Arial",
                             size = 10,
                             color = "black"),
                           layer = 'below traces',
                           exponentformat = "e"),
              paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = F, width = 270, height = 100, margin = m)
  })
  
  
  
  output$radarPlot3 <- renderPlotly({
    
    radardf0 <- datosCSV[datosCSV$hexacities == 1322, c("Transporte","SupEdifd","Crimen","NivelSocioecon", "Vegetacion"), drop =F]
    radardf<- as.data.frame(t(radardf0))
    avector <- as.vector(radardf[,1])
    
    plot_ly(type = 'scatterpolar',
            radardf, 
            r = avector,
            theta = c("Transporte","Sup. Edif.","Crimen","Nivel Socioecon.", "Vegetacion"),mode = "lines",line = list(color = 'rgb(206, 23, 23)', width = 2),  fill = 'toself',fillcolor = 'red', opacity = 0.7 ,hoverinfo='skip') %>%
      config(displayModeBar = F) %>% 
      layout( title = 'Perfil urbano por deciles (click en el mapa)',font=list(family = "Arial", color="black", size=9), polar = list(bgcolor='white',  
                                                                               
                                                                               radialaxis = list(
                                                                                 side = 'counterclockwise',
                                                                                 showline = T,
                                                                                 linewidth = 1,
                                                                                 zeroline = F,
                                                                                 tickwidth = 0,
                                                                                 tickcolor = "red",
                                                                                 linecolor = "black",
                                                                                 gridcolor = "black",
                                                                                 gridwidth = 1,
                                                                                 layer = 'below traces',
                                                                                 range = c(0, 10)),
                                                                               
                                                                               angularaxis = list(
                                                                                 labels = FALSE,
                                                                                 tickwidth = 0,
                                                                                 linewidth = 1,
                                                                                 linecolor = "black",
                                                                                 gridcolor = "black",
                                                                                 gridwidth = 1),
                                                                               layer = 'below traces'
                                                                               
                                                                               
      ),paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = F, width = 270, height = 200, margin = list(
        l = 35,
        r = 35,
        b = 35,
        t = 30
      ))
    
  })
  
  
  observe({
    click = input$map_shape_click
    
    radardf0 <- datosCSV[datosCSV$hexacities == click$id, c("Transporte","SupEdifd","Crimen","NivelSocioecon", "Vegetacion"), drop =F]
    radardf<- as.data.frame(t(radardf0))
    avector <- as.vector(radardf[,1])
    
    if(is.null(click))
      return()
    else
      output$radarPlot3 <- renderPlotly({
        plot_ly(type = 'scatterpolar',
                radardf, 
                r = avector,
                theta = c("Transporte","Sup. Edif.","Crimen","Nivel Socioecon.", "Vegetacion"),
                mode = "lines",line = list(color = 'rgb(206, 23, 23)', width = 2), fill = 'toself',fillcolor = 'red', opacity = 0.7,hoverinfo='skip') %>%
           config(displayModeBar = F) %>% 
          
          layout( title = 'Perfil urbano por deciles (click en el mapa)',font=list(family = "Arial", color="black", size=9), polar = list(bgcolor='white',  
                                                                                   
                                                                                   radialaxis = list(
                                                                                     side = 'counterclockwise',
                                                                                     showline = T,
                                                                                     linewidth = 1,
                                                                                     zeroline = F,
                                                                                     tickwidth = 0,
                                                                                     tickcolor = "red",
                                                                                     linecolor = "black",
                                                                                     gridcolor = "black",
                                                                                     gridwidth = 1,
                                                                                     layer = 'below traces',
                                                                                     range = c(0, 10)),
                                                                                   
                                                                                   angularaxis = list(
                                                                                     labels = FALSE,
                                                                                     tickwidth = 0,
                                                                                     linewidth = 1,
                                                                                     linecolor = "black",
                                                                                     gridcolor = "black",
                                                                                     gridwidth = 1),
                                                                                   layer = 'below traces'
                                                                                   
                                                                                   
          ),paper_bgcolor='transparent',plot_bgcolor='rgba(0,0,0,0)', showlegend = F, autosize = T, width = 270, height = 200, margin = list(
            l = 35,
            r = 35,
            b = 35,
            t = 30
          ))
        
      })
    
  })
  
  
  
  
  
  
  filedataCA <- reactive({
    infile <- input$subircsvCA
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  columnasCA <-reactive({
    
    numericas <-  filedataCA()
    numericas <- numericas[sapply(numericas,is.numeric)]
    
    
    colnames(numericas)
  
    
    })
  
  columnasCAstreet <-reactive({
    
    colnames(filedataCA())
    
    
  })
  
  

  filedata <- reactive({
    infile <- input$subircsv
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  columnas <-reactive({
    numericas <-  filedata()
    numericas <- numericas[sapply(numericas,is.numeric)]
    
    
    colnames(numericas)
  })
  
  

  
  # output$tabla <- renderDataTable(filedata(), options = list(lengthMenu = c(5, 20, 40), pageLength = 5))
  
  observeEvent(input$subircsv, {
    
   
    updateSelectInput(session,"csvLat", choices = columnas() )
    updateSelectInput(session,"csvLong", choices = columnas() )
    updateSelectInput(session,"csvData", choices = c("Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono",columnas()))
    updateSelectInput(session,"csvAgr", choices = c("sum", "mean") )
    
    
    
 })

  
  observeEvent(input$subircsvCA, {
    
    updateSelectInput(session,"csvCACalle", choices = columnasCAstreet() )
    updateSelectInput(session,"csvCAAltura", choices = columnasCAstreet() )
    updateSelectInput(session,"csvCAData", choices = c("Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono",columnasCA()))
    updateSelectInput(session,"csvCAAgr", choices = c("sum", "mean") )
    
    
  })
  
  
  
  
  observeEvent(input$addCSV, {
    
    
    if (input$csvName=="") {
      modal <- modalDialog(
        title = "Advertencia",
        easyClose = T,
        h5("Nombre de la variable"),
        h6("Por favor ingrese un nombre para la variable"),
        footer = modalButton("Entendido"))
      
      
      showModal(modal)
      
    } else { 
    
    
    
    
    
    filedata <- filedata()
    label1 <- paste(input$csvName)
    
    
    if (input$csvData == "Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono" ) {
      
      filedata2 <- filedata[,c(paste(input$csvLat),paste(input$csvLong))]
      filedata2$uno <- 1
      as.data.frame(names(filedata2)[3]<-label1)
      filedata2[is.na(filedata2)] <- 0
      filedata2[filedata2==""] <- 0
      
      shapefilePuntos <- SpatialPointsDataFrame(filedata2[,1:2],filedata2, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84") )
      shapefilePuntos <- spTransform(shapefilePuntos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
      
      res <- over(shapeFile, shapefilePuntos, fn = sum)
      res[is.na(res)] <- 0
      rownames(res) <- as.integer(rownames(res))+1
      
    } else {
      
      filedata2 <- filedata[,c(paste(input$csvLat),paste(input$csvLong), paste(input$csvData))]
      as.data.frame(names(filedata2)[3]<-label1)
      filedata2[is.na(filedata2)] <- 0

      shapefilePuntos <- SpatialPointsDataFrame(filedata2[,1:2],filedata2, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84") )
      shapefilePuntos <- spTransform(shapefilePuntos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
      
      res <- over(shapeFile,shapefilePuntos, fn = input$csvAgr)
      res[is.na(res)] <- 0
      rownames(res) <- as.integer(rownames(res))+1
      

    }
    

    shapeFile2@data <- merge(x = shapeFile2@data, y=res, by.y = 0, by.x="hexacities", all.x = TRUE)
    shapeFile2@data[is.na(shapeFile2@data)] <- 0
    
    
    leafletProxy("map5", data = shapeFile2)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      clearShapes() %>%
      addPolygons(data = shapeFile2, layerId = ~shapeFile2$hexacities, fillColor = "Red", fillOpacity = (shapeFile2@data[,4]+0.001)/max(shapeFile2@data[,4]),
                  color = "white", weight = 0,popup=paste(shapeFile2@data[,4])) %>%
      clearControls()%>%
      addLegend("topleft",
                colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
                labels= c("Más", "","","", "Menos"),
                title= paste(input$csvName),
                opacity = 1)
    
    }

    datosCSV <- datosCSVreactive()
    newdatosCSVreactive <- cbind(datosCSV, res)
    datosCSVreactive(newdatosCSVreactive)


    lista <- listavarlab()

    listavarlab(as.vector(c(lista, label1)))


    updateSelectInput(session,"pType", choices = listavarlab(), selected = pTyper())
    updateSelectInput(session,"lista1", choices = listavarlab(), selected= lista1r())
    updateSelectInput(session,"lista2", choices = listavarlab(), selected=lista2r())
    updateSelectInput(session,"lista3", choices = listavarlab(), selected=lista3r())
    updateSelectInput(session,"lista4", choices = listavarlab(), selected=lista4r())

    updateSelectInput(session, "listaA", choices = listavarlab(), selected=listaAr())
    updateSelectInput(session,"listaB", choices = listavarlab(), selected=listaBr())

    updateSelectInput(session,"vardep", "Variable a explicar", choices = listavarlab(), selected = vardepr())
    updateSelectInput(session,"selectize", "Variables independientes", choices = listavarlab(),  selected = selectizer())
    updateSelectInput(session, "dimensiones", "Seleccione las dimensiones de entrada", choices = listavarlab(), selected= dimensionesr())

    updateSelectInput(session,"vardepGWR", "Variable a explicar", choices = listavarlab(), selected = vardeprGWR())
    updateSelectInput(session,"selectizeGWR", "Variables independientes", choices = listavarlab(),  selected = selectizerGWR())
    
    updateSelectInput(session,"selectizeCorrplot", "Variables:", choices = listavarlab(),  selected = selectizeCorrplot())
    

     })
  
  
  
  
  observeEvent(input$addCSVCA, {
    
    
    if (input$csvCAName=="") {
      modal <- modalDialog(
        title = "Advertencia",
        easyClose = T,
        h5("Nombre de la variable"),
        h6("Por favor ingrese un nombre para la variable"),
        footer = modalButton("Entendido"))
      
      
      showModal(modal)
      
    } else { 
      
      
      
      
      
      filedata <- filedataCA()
      label1 <- paste(input$csvCAName)
      
      
      if (input$csvCAData == "Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono" ) {
        
        filedata <- filedata[,c(paste(input$csvCACalle),paste(input$csvCAAltura))]
        
        
        filedata$hccallealtura <- paste(filedata[,1]," ",filedata[,2]) 
        
        geocoded <- lapply(filedata$hccallealtura, USIG_geocode)
        
        filedata<-do.call(rbind, geocoded)
        
        # filedata<-cbind(filedata, geocoded)
        
        filedata <- filedata[,c("lng", "lat")]
        filedata[is.na(filedata)] <- 0
        
        filedata$lat <- as.numeric(filedata$lat)
        filedata$lng <- as.numeric(filedata$lng)
        
        filedata$uno <- 1
        as.data.frame(names(filedata)[3]<-label1)
        filedata[is.na(filedata)] <- 0
        filedata[filedata==""] <- 0
        
        
        shapefilePuntos <- SpatialPointsDataFrame(filedata[,1:2],filedata, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84") )
        shapefilePuntos <- spTransform(shapefilePuntos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"))
        
        res <- over(shapeFile, shapefilePuntos, fn = sum)
        
        res[is.na(res)] <- 0
        
        rownames(res) <- as.integer(rownames(res))+1
        
        
        
      } else {
        
        filedata <- filedata[,c(paste(input$csvCACalle),paste(input$csvCAAltura), paste(input$csvCAData))]
        
        filedata$hccallealtura <- paste(filedata[,1]," ",filedata[,2]) 
        
        geocoded <- lapply(filedata$hccallealtura, USIG_geocode)
        
        geocoded<-do.call(rbind, geocoded)

        filedata  <- cbind(filedata, geocoded)
        
        filedata <- filedata[,c("lng","lat", paste(input$csvCAData))]
       
        as.data.frame(names(filedata)[3]<-label1)
        filedata[is.na(filedata)] <- 0
        
        # filedata[,3] <- as.numeric(as.character(filedata[,3]))
        filedata$lat <- as.numeric(filedata$lat)
        filedata$lng <- as.numeric(filedata$lng)
        
        
        shapefilePuntos <- SpatialPointsDataFrame(filedata[,1:2],filedata, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") )
        shapefilePuntos <- spTransform(shapefilePuntos, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        res <- over(shapeFile,shapefilePuntos, fn = input$csvCAAgr)
        
        res[is.na(res)] <- 0
        rownames(res) <- as.integer(rownames(res))+1
        
        
      }
      
      
      shapeFile2@data <- merge(x = shapeFile2@data, y=res, by.y = 0, by.x="hexacities", all.x = TRUE)
      shapeFile2@data[is.na(shapeFile2@data)] <- 0
      
      
      leafletProxy("map5", data = shapeFile2)%>%
        addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
        clearShapes() %>%
        addPolygons(data = shapeFile2, layerId = ~shapeFile2$hexacities, fillColor = "Red", fillOpacity = (shapeFile2@data[,4]+0.001)/max(shapeFile2@data[,4]),
                    color = "white", weight = 0,popup=paste(shapeFile2@data[,4])) %>%
        clearControls()%>%
        addLegend("topleft",
                  colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
                  labels= c("Más", "","","", "Menos"),
                  title= paste(input$csvCAName),
                  opacity = 1)
      
    }

    datosCSV <- datosCSVreactive()
    newdatosCSVreactive <- cbind(datosCSV, res)
    datosCSVreactive(newdatosCSVreactive)


    lista <- listavarlab()

    listavarlab(as.vector(c(lista, label1)))


    updateSelectInput(session,"pType", choices = listavarlab(), selected = pTyper())
    updateSelectInput(session,"lista1", choices = listavarlab(), selected= lista1r())
    updateSelectInput(session,"lista2", choices = listavarlab(), selected=lista2r())
    updateSelectInput(session,"lista3", choices = listavarlab(), selected=lista3r())
    updateSelectInput(session,"lista4", choices = listavarlab(), selected=lista4r())

    updateSelectInput(session, "listaA", choices = listavarlab(), selected=listaAr())
    updateSelectInput(session,"listaB", choices = listavarlab(), selected=listaBr())

    updateSelectInput(session,"vardep", "Variable a explicar", choices = listavarlab(), selected = vardepr())
    updateSelectInput(session,"selectize", "Variables independientes", choices = listavarlab(),  selected = selectizer())
    updateSelectInput(session, "dimensiones", "Seleccione las dimensiones de entrada", choices = listavarlab(), selected= dimensionesr())

    updateSelectInput(session,"vardepGWR", "Variable a explicar", choices = listavarlab(), selected = vardeprGWR())
    updateSelectInput(session,"selectizeGWR", "Variables independientes", choices = listavarlab(),  selected = selectizerGWR())
    
    updateSelectInput(session,"selectizeCorrplot", "Variables:", choices = listavarlab(),  selected = selectizeCorrplot())
    

  })
  
  
  
  
  
  
  
  
  
  
  
  filedatashp <- reactive({

    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    shpdf <- input$subirshp
    
    # The files are uploaded with names 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names: fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    
    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.
    
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp, not only that contain .shp)
    map <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    map
    
    
  })
  
  
  
  columnasshp <-reactive({
    filedatashp <- filedatashp()
    
    numericas <- filedatashp@data[sapply(filedatashp@data,is.numeric)]
    
    
    colnames(numericas)

  
    
    
  })
  
  
  
  # output$tabla <- renderDataTable(filedata(), options = list(lengthMenu = c(5, 20, 40), pageLength = 5))
  
  observeEvent(input$subirshp, {
    
    filedatashp <- filedatashp()
    
    if (class(filedatashp) ==  "SpatialPointsDataFrame") {

      updateSelectInput(session,"shpData", choices = c("Sin variable de agregación, sólo quiero contar cuantos puntos caen en cada hexágono", columnasshp() ))
      updateSelectInput(session,"shpAgr", choices = c("Suma de los valores de los puntos", "Promedio de los valores de los puntos") )
    
    } else if (class(filedatashp) ==  "SpatialPolygonsDataFrame") {

      updateSelectInput(session,"shpData", choices = c("Sin variable de agregación, sólo quiero calcular las superficies de los polígonos y sumarlas en cada hexágono",columnasshp() ))
      updateSelectInput(session,"shpAgr", choices = c("Suma proporcional al area cortada por los hexágonos", "Promedio proporcional al area cortada por los hexágonos" ))
    }
    
   else if (class(filedatashp) ==  "SpatialLinesDataFrame") {
    
     updateSelectInput(session,"shpData", choices = c("Sin variable de agregación, sólo quiero calcular la longitud de las líneas y sumarlas en cada hexágono",columnasshp() ))
     updateSelectInput(session,"shpAgr", choices = c("Suma proporcional a los metros", "Promedio ponderado por metros" ))
  
     }
  })
  

  
  
  observeEvent(input$addSHP, {



    if (input$shpName=="") {
      modal <- modalDialog(
        title = "Advertencia",
        easyClose = T,
        h5("Nombre de la variable"),
        h6("Por favor ingrese un nombre para la variable"),
        footer = modalButton("Entendido"))


      # Show the model on start up ...
      showModal(modal)

    } else {

      filedatashp <- filedatashp()
      label1 <- paste(input$shpName)


    if (class(filedatashp) ==  "SpatialPointsDataFrame") {

      if (input$shpData == "Sin variable de agregación, sólo quiero contar cuantos puntos caen en cada hexágono"){

        shite <- as.vector(colnames(filedatashp@data))
        filedatashp$uno <- 1
        filedatashp <- filedatashp[,!(names(filedatashp) %in% shite)]
        names(filedatashp@data)[names(filedatashp@data)=="uno"] <- label1
        filedatashp@data[is.na(filedatashp@data)] <- 0
        filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

        res <- over(shapeFile,filedatashp, fn = sum)
        res[is.na(res)] <- 0
        res$hexacities <- as.integer(rownames(res))+1

        rownames(res) <- as.integer(rownames(res))+1
        
        res<-res[,order(ncol(res):1)]
        
        
      }

      else {

        filedatashp <- filedatashp[,(names(filedatashp) %in% paste(input$shpData))]
        # filedatashp@data[,1] <- as.numeric(filedatashp@data[,1])
        names(filedatashp@data)[names(filedatashp@data)==input$shpData] <- label1
        filedatashp@data[is.na(filedatashp@data)] <- 0
        filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        res <- over(shapeFile,filedatashp, fn = switchfuncion(input$shpAgr))
        res[is.na(res)] <- 0
    
        res$hexacities <- as.integer(rownames(res))+1
        
        rownames(res) <- as.integer(rownames(res))+1
        res<-res[,order(ncol(res):1)]
        
       }

    }
     else  if (class(filedatashp) ==  "SpatialPolygonsDataFrame") {


       if (input$shpData == "Sin variable de agregación, sólo quiero calcular las superficies de los polígonos y sumarlas en cada hexágono"){

         filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

         # intersect <- raster::intersect(shapeFile,filedatashp)
         shapeFilesf <- sf::st_as_sf(shapeFile)
         filedatashp <- sf::st_as_sf(filedatashp)

         intersect <- sf::st_intersection(filedatashp, shapeFilesf)
         intersect <-sf:::as_Spatial(intersect)

         intersect$area_sqkm2 <- area(intersect) / 1000000

         # centroids <- SpatialPointsDataFrame(gCentroid(intersect, byid=TRUE),
         #                                     intersect@data, match.hexacities=FALSE)


         res <-
           intersect@data %>%
           group_by(hexacities) %>%
           summarise_at(vars(area_sqkm2), funs( area_sqkm2= sum(area_sqkm2)))

         res[is.na(res)] <- 0

       }
       else {

         if (input$shpAgr == "Suma proporcional al area cortada por los hexágonos") {

           filedatashp <- filedatashp[,(names(filedatashp) %in% paste(input$shpData))]
           # filedatashp@data[,1] <- as.numeric(as.character(filedatashp@data[,1]))
           filedatashp@data[is.na(filedatashp@data)] <- 0
           names(filedatashp@data)[names(filedatashp@data)== paste(input$shpData)] <- "varobj"

           filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

           shapeFilesf <- sf::st_as_sf(shapeFile)
           filedatashp <- sf::st_as_sf(filedatashp)
           filedatashp$area_sqkm <- sf::st_area(filedatashp)

           intersect <- sf::st_intersection(shapeFilesf,filedatashp)
           # intersect <-sf:::as_Spatial(intersect)
           # intersect <- raster::intersect(shapeFile,filedatashp)

           # intersect[is.na(intersect)] <-0
           intersect$area_sqkm2 <- sf::st_area(intersect)

           intersect$area_sqkm <- as.numeric(as.character(sub("\\s+\\D+$", "", intersect$area_sqkm)))
           intersect$area_sqkm2 <- as.numeric(as.character(sub("\\s+\\D+$", "", intersect$area_sqkm2)))

           intersect$varnueva <- intersect$varobj * intersect$area_sqkm2 / intersect$area_sqkm

           res <-
             intersect %>%
             group_by(hexacities) %>%
             summarise_at(vars(varnueva), funs( varnueva= sum(varnueva)))

           names(res)[names(res)== "varnueva"] <- label1
           res$geometry <- NULL

           res[is.na(res)] <- 0


         } else if (input$shpAgr == "Promedio proporcional al area cortada por los hexágonos"){


           filedatashp <- filedatashp[,(names(filedatashp) %in% paste(input$shpData))]
           # filedatashp@data[,1] <- as.numeric(as.character(filedatashp@data[,1]))
           filedatashp@data[is.na(filedatashp@data)] <- 0
           names(filedatashp@data)[names(filedatashp@data)== paste(input$shpData)] <- "varobj"

           filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

           shapeFilesf <- sf::st_as_sf(shapeFile)
           filedatashp <- sf::st_as_sf(filedatashp)

           intersect <- sf::st_intersection(shapeFilesf,filedatashp)
           # intersect <-sf:::as_Spatial(intersect)
           # intersect <- raster::intersect(shapeFile,filedatashp)
           # intersect[is.na(intersect)] <-0
           intersect$area_sqkm2 <- sf::st_area(intersect)

           intersect$area_sqkm2 <- as.numeric(as.character(sub("\\s+\\D+$", "", intersect$area_sqkm2)))


           res <-
             intersect %>%
             group_by(hexacities) %>%
             summarise(weighted_value = weighted.mean(varobj , area_sqkm2))

           names(res)[names(res)== "weighted_value"] <- label1
           res$geometry <- NULL

           res[is.na(res)] <- 0

         }

          }

       }

    else  if (class(filedatashp) ==  "SpatialLinesDataFrame") {

      if (input$shpData == "Sin variable de agregación, sólo quiero calcular la longitud de las líneas y sumarlas en cada hexágono"){

        filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

        intersect <- raster::intersect(filedatashp, shapeFile)
        intersect$largooo <- SpatialLinesLengths(intersect, TRUE)*1000
         intersect <- intersect[,(names(intersect) %in% c("hexacities", "largooo"))]
         
        res <-
          intersect@data %>%
          group_by(hexacities) %>%
          summarise_at(vars(largooo), funs( varnueva= sum(largooo)))

        names(res)[names(res)== "largooo"] <- label1
        res[is.na(res)] <- 0

      } else {

        if (input$shpAgr == "Suma proporcional a los metros"){


          filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

          filedatashp <- filedatashp[,(names(filedatashp) %in% paste(input$shpData))]
          filedatashp@data[,1] <- as.numeric(filedatashp@data[,1])
          filedatashp@data[is.na(filedatashp@data)] <- 0
          filedatashp$largooo <- SpatialLinesLengths(filedatashp, TRUE)

          intersect <- raster::intersect(filedatashp,shapeFile)

          intersect@data[is.na(intersect@data)] <-0
          intersect$largooo2 <- SpatialLinesLengths(intersect, TRUE)
          intersect$varnueva <- intersect@data[,2] * intersect$largooo2 / intersect$largooo


          res <-
            intersect@data %>%
            group_by(hexacities) %>%
            summarise_at(vars(varnueva), funs( varnueva= sum(varnueva)))

          names(res)[names(res)== "varnueva"] <- label1


          res[is.na(res)] <- 0



        } else if(input$shpAgr == "Promedio ponderado por metros"){


          filedatashp <- spTransform(filedatashp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 ++proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

          filedatashp <- filedatashp[,(names(filedatashp) %in% paste(input$shpData))]
          filedatashp@data[,1] <- as.numeric(as.character(filedatashp@data[,1]))

          intersect <- raster::intersect(filedatashp, shapeFile)

          intersect$largooo2 <- SpatialLinesLengths(intersect, TRUE)

          res <-
            intersect@data %>%
            group_by(hexacities) %>%
            summarise(weighted_value = weighted.mean(!!rlang::sym(input$shpData) , largooo2))

          names(res)[names(res)== "weighted_value"] <- label1





        }

      }




    }



    shapeFile2@data <- merge(x = shapeFile2@data, y=res, by.y="hexacities", by.x="hexacities", all.x = TRUE)
    shapeFile2@data[is.na(shapeFile2@data)] <- 0

    leafletProxy("map5", data = shapeFile2)%>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))%>%
      clearShapes() %>%
      addPolygons(data = shapeFile2, layerId = ~shapeFile2$hexacities, fillColor = "Red", fillOpacity = (shapeFile2@data[,2]+0.001)/max(shapeFile2@data[,2]),
                  color = "white", weight = 0,popup=paste(shapeFile2@data[,2])) %>%
      clearControls()%>%
      addLegend("topleft",
                colors =c("#ff0000", "#ff5200", "#ffaa90", "#ffc8b7", "#ffe2d9"),
                labels = c("Más", "","","", "Menos"),
                title = paste(input$csvName),
                opacity = 1)
    }



    newdatosCSVreactive <- datosCSVreactive()

     # newdatosCSVreactive <- cbind(datosCSV, res)

    colnames(res)[2] <- toString(label1)
    
    
    newdatosCSVreactive <- left_join(newdatosCSVreactive, res, by = 'hexacities')
      # newdatosCSVreactive <- merge(x = newdatosCSVreactive, y=res, by= "hexacities", all.x = TRUE)

    newdatosCSVreactive[is.na(newdatosCSVreactive)] <- 0

datosCSVreactive(newdatosCSVreactive)


    lista <- listavarlab()

    listavarlab(as.vector(c(lista, label1)))


    updateSelectInput(session,"pType", choices = listavarlab(), selected = pTyper())
    updateSelectInput(session,"lista1", choices = listavarlab(), selected= lista1r())
    updateSelectInput(session,"lista2", choices = listavarlab(), selected=lista2r())
    updateSelectInput(session,"lista3", choices = listavarlab(), selected=lista3r())
    updateSelectInput(session,"lista4", choices = listavarlab(), selected=lista4r())

    updateSelectInput(session, "listaA", choices = listavarlab(), selected=listaAr())
    updateSelectInput(session,"listaB", choices = listavarlab(), selected=listaBr())

    updateSelectInput(session,"vardep", "Variable a explicar", choices = listavarlab(), selected = vardepr())
    updateSelectInput(session,"selectize", "Variables independientes", choices = listavarlab(),  selected = selectizer())
    updateSelectInput(session, "dimensiones", "Seleccione las dimensiones de entrada", choices = listavarlab(), selected= dimensionesr())


    updateSelectInput(session,"vardepGWR", "Variable a explicar", choices = listavarlab(), selected = vardeprGWR())
    updateSelectInput(session,"selectizeGWR", "Variables independientes", choices = listavarlab(),  selected = selectizerGWR())

    updateSelectInput(session,"selectizeCorrplot", "Variables:", choices = listavarlab(),  selected = selectizeCorrplot())
    
  })
  
  
})

