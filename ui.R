shinyUI(
  fluidPage(
    
  tags$head(HTML("<title>hexaBA</title> <link rel='icon' type='image/gif/png' href='icono.png'>")), #WIth company logo
 
  navbarPage(theme = shinytheme("spacelab"), div(img(src="hexaBA.png",height="52", style="padding: 12px 12px; margin-top:-1em;margin-bottom:0em;vertical-align:top;horizontal-align:left; z-index:1000000;")), windowTitle = "hexaBA", collapsible = TRUE, id="nav", position ="fixed-top",
      
  tabPanel("Visor de capas", icon=icon("layer-group"),
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      leafletOutput("map", width="100%", height="100%"),
     
      jqui_draggable(
       # bootstrapPage(
        absolutePanel(
       id = "controls", 
       class = "modal-body", 
        fixed = FALSE, draggable = TRUE,
        top = 30, left = "auto", right = 50, bottom = "auto",
        width = 300, height = "auto",
        
       HTML('<button data-toggle="collapse" data-target="#demo">Colapsar</button>'),
       tags$div(id = 'demo',  class="collapse-in",
       
        div(style = "display: inline-block;height:18px;vertical-align:top;horizontal-align:middle;margin-top:-1em;margin-bottom:1em;",h3("Explorador de capas")),
        
        div(style = "display: inline-block;height:10px;vertical-align:middle;;margin-bottom:1em", checkboxInput("switch", "Vista satelital", value = TRUE)),
       selectInput("pType", "",
                  choices = lista
                        
                   ),
       
       uiOutput("slider2" ),
       hr(),
       plotlyOutput("areaPlot",  width = 270, height = 100),
       hr(),
       plotlyOutput("radarPlot3",  width = 250, height = 200)
       
       
       
      ))
      , options = list(cancel = ".shiny-input-container"))
    )
    ,
    
    tags$div(id="cite",
             HTML('Contacto: Nahuel Patiño / nahuel89p@gmail.com / Twitter:@nahuelpat89 / <br> BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z / LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT'
             ))
  ),

  
                            
tabPanel("Scoring",icon=icon("scale", lib="glyphicon"),

         div(class="outer",
             tags$style(type='text/css', ".selectize-input { padding: 1px; min-height: 0;} .selectize-dropdown { line-height: 22px; }"),
             # tags$div(id = 'control',  class="collapse"),
            
             tags$head(tags$style(
               HTML('
                    #controls {opacity : 0.65;}
                    #controls:hover{opacity: 1;}
                    #controls {border: 1px solid;}
                    #controls {border-radius: 15px;}
                    #controls {background-color: rgba(255,255,255,1);}
                    #controls {padding: 0 20px 20px 20px;}
                    #controls {z-index: 100000;}
                    ')
               )),
             
             
              
             tags$head(
               # Include our custom CSS
                includeCSS("styles.css"),
               includeScript("gomap.js")
             ),
             
             leafletOutput("map3", width="100%", height="100%"),
   
               
             jqui_draggable(
               # bootstrapPage(
             absolutePanel(
                      
                 id = "controls", 
                 class = "modal-body", 
                 fixed = FALSE, draggable = TRUE,
                 top = 25, left = "auto", right = 10, bottom = "auto",
                 width = 1050, height = "auto",
                 
                 HTML('<button data-toggle="collapse" data-target="#demo3">Colapsar</button>'),
                 tags$div(id = 'demo3',  class="collapse-in",
                 
                 h3("Compositor de índice"),
                 h6("Permite combinar en un índice hasta 4 variables a libre elección, con posibilidad de determinar algunos aspectos de cada variable, como su ponderación, transformación y sentido. Para desactivar una variable, deslizar la ponderación a 0. "),
                 h6("El indice generado se agregará a la lista de variables y podrá ser utilizado en todas las otras secciones. "),
                 
                 hr(),
                 fluidRow(
                    column(2,div(style = "display: inline-block;height:30px;vertical-align:middle;padding: 0px 0px; margin-top:-4em;margin-bottom:-3em;horizontal-align:middle",
                        actionButton("boton", "Generar indice")),
                        
                        div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>"))),
              
                   column(10, div(style = "display: inline-block;height:30px;vertical-align:middle;padding: 0px 0px; margin-top:-4em;margin-bottom:-2em;horizontal-align:middle",
                   h6("El ejemplo a continuación, combina 'volumen edificado', 'precio del metro cuadrado' (a modo de mas barato mejor), % de actividad comercial (sobreponderado), e influencia del subte. Puede interpretarse como útil para alguien que quiere abrir un local en una zona comercial, transitada y barata."))

                    )
                 
                 ),
                
                 hr(),
                 
                 fluidRow("V.1", 
                          div(style="display: inline-block;vertical-align:middle; height: 75px; width: 250px;", selectInput("lista1", "", choices = lista, selected='Volumen edificado')),
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("trans1", "",   choices = transforma )),    
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:top; height: 75px;width: 150px;",  sliderInput("pondera1","Ponderación", min=0, max=2, value=1, step=0.01)),    
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 150px;",  checkboxInput("direccion1","Menos es mejor", FALSE ) ),    
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 5px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 130px;",  plotlyOutput("histograma1",width = 150, height = 75))   
                          
                 ),
                 
                 hr(),
                 
                 
                 fluidRow("V.2",
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 250px;", selectInput("lista2", "", choices = lista, selected="Precio M2 (USD)")),
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("trans2", "",   choices = transforma, selected = "boxcox")),    
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 150px;",  sliderInput("pondera2","", min=0, max=2, value=1, step=0.01)),    
                          div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  checkboxInput("direccion2","Menos es mejor", TRUE)),    
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 5px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 130px;",  plotlyOutput("histograma2",width = 150, height = 75))
                          
                 ) ,
                 
                 hr(),
                 
                 
                 fluidRow("V.3",
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 250px;", selectInput("lista3", "", choices = lista, selected="% Volumen Edificado Destinado a Actividades Comerciales")),
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("trans3", "",   choices = transforma, selected = "boxcox")),    
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 150px;",  sliderInput("pondera3","", min=0, max=2, value=1.8, step=0.01)),    
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  checkboxInput("direccion3","Menos es mejor")),    
                          div(style="display: inline-block;vertical-align:middle; height: 75px;width: 5px;",HTML("<br>")),
                          
                          div(style="display: inline-block;vertical-align:middle;height: 75px; width: 130px;",  plotlyOutput("histograma3",width = 150, height = 75))
                 ) ,
                          hr(),
                          
                          fluidRow("V.4",
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 250px;", selectInput("lista4", "", choices = lista, selected="Area de Influencia del Subte (Ajustado por Usuarios por Estacion)")),
                                   div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("trans4", "",   choices = transforma )),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  sliderInput("pondera4","", min=0, max=2, value=1, step=0.01)),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle; height: 75px;width: 150px;",  checkboxInput("direccion4","Menos es mejor")),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 5px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px;width: 130px;",  plotlyOutput("histograma4",width = 150, height = 75))
                                   
                                   
                          
                     
                             
                 )
           
                 
                 
               ), options = list(cancel = ".shiny-input-container"))),
             
             
             
             tags$div(id="cite",
                      HTML('Contacto: Nahuel Patiño / nahuel89p@gmail.com / Twitter:@nahuelpat89 / <br> BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z / LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT'
                      ))
         )
),
  

 # HTML("<i class=fa fa-crosshairs></i>")


tabPanel("Análisis de regresión (new!)",icon=icon("chart-line"),

         div(class="outer",
             tags$style(type='text/css', ".selectize-input {font-size: 12px; padding: 1px; min-height: 0;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
             # tags$div(id = 'control',  class="collapse"),

             tags$head(tags$style(
               HTML('
                    #controls {opacity : 0.65;}
                    #controls:hover{opacity: 1;}
                    #controls {border: 1px solid;}
                    #controls {border-radius: 15px;}
                    #controls {background-color: rgba(255,255,255,1);}
                    #controls {padding: 0 20px 20px 20px;}
                    ')
               )),

             # tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }")
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css"),
               includeScript("gomap.js")
             ),

             leafletOutput("map2", width="100%", height="100%"),
             

             jqui_draggable(
               # bootstrapPage(
               absolutePanel(
                 id = "controls",
                 class = "modal-body",
                 fixed = FALSE, draggable = TRUE,
                 top = 25, left = "auto", right = 50, bottom = "auto",
                 width = 800, height = "auto",
                 
                 HTML('<button data-toggle="collapse" data-target="#demo2">Colapsar</button>'),
                 tags$div(id = 'demo2',  class="collapse-in",
                 
                 tabsetPanel(
                   tabPanel("Regresión lineal", icon=icon("chart-line"),
                             
                          
                 tabsetPanel(
                 tabPanel("Variables", icon=icon("sliders-h"),

                 h3("Regresión lineal (OLS)"),
                  h6("Seleccionar una variable a explicar, y múltiples variables explicativas. Click en 'Modelar', con libre elección entre resultados fitted, residuales y distancia de Cook. 'Fitted' describe los valores esperados (predecidos) para cada hexágono 'Residual' describe la diferencia, negativa (azul) o positiva (rojo), entre los valores esperados y observados para cada hexágono 'Cook's distance' revela qué tan influyente y atípico es cada hexágono, sugiriendo una examinación mas pormenorizada para esos casos."),
                 
                 hr(),
                 fluidRow(column(2,div(style="display: inline-block;height:25px;vertical-align:middle;padding: 0px 0px; margin-top:-4em;margin-bottom:-3em;horizontal-align:middle",  actionButton("boton2", "Modelar")),
                          div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>"))),
                          
                          column(10, div(style="display: inline-block;height:25px;padding: 0px 0px; margin-top:-4em;margin-bottom:-2em;vertical-align:middle;horizontal-align:middle",  h6("En el ejemplo por default, se modelizará el voto al partido más votado en las elecciones de 2017, en función de un conjunto de variables sociodemográficas y urbanas.")))),
                 hr(),
                 div(style="display: inline-block;vertical-align:middle", selectInput("vardep", "Variable a explicar", lista,width = 400, selected = "% Elecciones 2017: Vamos Juntos")),
                 div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>")),
                 div(style="display: inline-block;vertical-align:middle", selectInput("resorfit", "Resultado a mapear", c("Residual", "Fitted", "Cook's distance"),width = 200)),
                 
                 
                 selectInput("selectize", "Variables independientes", lista, multiple=TRUE, selectize=TRUE,width = 750, selected = default),
                 div(style="display: inline-block;vertical-align:middle; width: 600px;", sliderInput("cookometro","Omitir para el modelado los 'x' casos mas atípicos (influyentes según Cook's Distance):", min = 0, max = 500, value = 0, width = 800))
                
                 
                 ),
               
                 
                 tabPanel("Subset input/output",icon=icon("map-marked-alt"),
                          h3("Subset por barrios"),
                          h5("Se recomienda alterar los barrios de entrada/salida para tratar de predecir, en otras partes de la ciudad, la ocurrencia de fenómenos hasta ahora acotados a pocos barrios."),
                          hr(),
                          selectInput("barrioin", "Barrios de entrada: utilizados para calcular la función de regresión. Por defecto, todos. Click y tecla <supr> para quitar un barrio.", barrios, multiple=TRUE, selectize=TRUE,selected= barrios,width = 950),
                          selectInput("barrioout", "Barrios de salida: donde se va a aplicar la función de regresión. Por defecto, todos. Click y tecla <supr> para quitar un barrio.", barrios, multiple=TRUE, selectize=TRUE, selected = barrios,width = 950)),
                          
                 tabPanel("Verbatim output", icon=icon("terminal"),
                          h3("Log de consola"),
                          h5("Salida de consola R"),
                          verbatimTextOutput("prediction")),
                 
                 tabPanel("Plots",icon=icon("chart-bar"),
                          h3("Plots"),
                          plotlyOutput("residvsfit", height = 300,width = 750),
                          plotOutput("cooksdistance", height = 300, width = 750)
                          
                          
                          )
                 
                 
                 
                 )),
                 
                 
                 
                 tabPanel("Regresión espacialmente ponderada (GWR) (new!)", icon=icon("weight-hanging"),
                          

                 
                          tabsetPanel(
                            
                          
                          tabPanel("Variables", icon=icon("sliders-h"),
                                   
                                   h3("Regresión Espacialmente Ponderada (GWR)"),
                                   h6("Seleccionar una variable a explicar, y múltiples variables explicativas. Click en 'Modelar' y esperar dos minutos. Al finalizar, se hará visible la lista desplegable de dimensiones y análisis disponibles para ser mapeados. "),
                                   h6("LEYENDA: y = var objetivo | yhat = predicción del modelo | CV_Score = Cross Validation Score | Local_R2 = R2 local | x_SE = Error estándar del estimador del coeficiente de la variable x | x_TV = t valor del estimador del coef. de la var. x | "),
                                   
                                   hr(),
                                   fluidRow(column(2,div(style="display: inline-block;height:25px;vertical-align:middle;padding: 0px 0px; margin-top:-4em;margin-bottom:-3em;horizontal-align:middle",  actionButton("boton2GWR", "Modelar")),
                                                   div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>"))),
                                            
                                            column(10, div(style="display: inline-block;height:25px;padding: 0px 0px; margin-top:-4em;margin-bottom:-2em;vertical-align:middle;horizontal-align:middle",  h6("En el ejemplo por default, se modelizará el voto al partido más votado en las elecciones de 2017, en función de un conjunto de variables sociodemográficas y urbanas.")))),
                                   hr(),
                                   div(style="display: inline-block;vertical-align:middle", selectInput("vardepGWR", "Variable a explicar", lista,width = 400, selected = "% Elecciones 2017: Vamos Juntos")),
                                   div(style="display: inline-block;vertical-align:middle; width: 15px;",HTML("<br>")),
                                   div(style="display: inline-block;vertical-align:middle",  uiOutput("resorfitGWR")),
                                   
                                   
                                   selectInput("selectizeGWR", "Variables independientes", lista, multiple=TRUE, selectize=TRUE,width = 750, selected = default)
                                   
                                   
                          ),
                          
                          
            
                          tabPanel("Verbatim output", icon=icon("terminal"),
                                   h3("Log de consola"),
                                   h5("Salida de consola R"),
                                   verbatimTextOutput("predictionGWR"))
                          
                          # tabPanel("Plots",icon=icon("chart-bar"),
                          #          h3("Plots")
                          #          
                          #          
                          # )
                          
                          )
                          
                 
                 
                 
                 
                 
                 
                 
                 ##########
                 
                 ),
                 
                 
                 tabPanel("Correlaciones (new!)", icon=icon("th"),
                          
                          h3("Matriz de correlaciones"),
                          h6("Selecciona el conjunto de variables a ser incluidas en el análisis de correlación"),
                          
                          selectInput("selectizeCorrplot", "Variables:", lista, multiple=TRUE, selectize=TRUE,width = 750, selected = defaultCorr),
                          
                          plotOutput("Corrplot", width = "100%", height = "600px")
                          
                          
                          
                 )
                 
                 )
                          
                 , options = list(cancel = ".shiny-input-container")))),

                   
             
             tags$div(id="cite",
                      HTML('Contacto: Nahuel Patiño / nahuel89p@gmail.com / Twitter:@nahuelpat89 / <br> BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z / LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT'
                      ))
               )
             






),


tabPanel("Clusterización",icon=icon("globe"),
         
         
         div(class="outer",
             tags$style(type='text/css', ".selectize-input {font-size: 12px; padding: 1px; min-height: 0;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
             # tags$div(id = 'control',  class="collapse"),
             
             tags$head(tags$style(
               HTML('
                    #controls {opacity : 0.65;}
                    #controls:hover{opacity: 1;}
                    #controls {border: 1px solid;}
                    #controls {border-radius: 15px;}
                    #controls {background-color: rgba(255,255,255,1);}
                    #controls {padding: 0 20px 20px 20px;}
                    ')
               )),
             
             # tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }")
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css"),
               includeScript("gomap.js")
             ),
             
             leafletOutput("map4", width="100%", height="100%"),
             
             
             jqui_draggable(
               # bootstrapPage(
               absolutePanel(
                 id = "controls",
                 class = "modal-body",
                 fixed = FALSE, draggable = TRUE,
                 top = 50, left = "auto", right = 50, bottom = "auto",
                 width = 500, height = "auto",
           

                 HTML('<button  data-toggle="collapse" data-target="#demo4">Colapsar</button>'),
                 
                 tags$div(id = 'demo4',  class="collapse-in",
                          h3("Clusterización multidimensional"),
                          selectInput("dimensiones", "Seleccione las dimensiones de entrada", lista, multiple=TRUE, selectize=TRUE,selected= default2,width = 950),
                          sliderInput("nclusters","n de Clusters", min = 2, max =30, step = 1, value =6),
                          sliderInput("alpha", "Alpha (privilegia más, o menos, la continuidad espacial) ", min = 0, max =1, value =0.28),
                          checkboxInput("clusterboxcox","Pre-normalización boxcox de las variables", FALSE),
                          actionButton("clusterizar", "Clusterizar")
         
                 )), options = list(cancel = ".shiny-input-container")))
         ,
         
         
         
         tags$div(id="cite",
                  HTML('Contacto: Nahuel Patiño / nahuel89p@gmail.com / Twitter:@nahuelpat89 / <br> BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z / LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT'
                  ))
         
)
,



tabPanel("Laboratorio de capas (new!)",icon=icon("flask"),
         
         
         
         
         div(class="outer",
             tags$style(type='text/css', ".selectize-input {font-size: 12px; padding: 1px; min-height: 0;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
             # tags$div(id = 'control',  class="collapse"),
             
             tags$head(tags$style(
               HTML('
                    #controls {opacity : 0.65;}
                    #controls:hover{opacity: 1;}
                    #controls {border: 1px solid;}
                    #controls {border-radius: 15px;}
                    #controls {background-color: rgba(255,255,255,1);}
                    #controls {padding: 0 20px 20px 20px;}
                    ')
               )),
             
             # tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }")
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css"),
               includeScript("gomap.js")
             ),
             
             leafletOutput("map5", width="100%", height="100%"),
             
             
             jqui_draggable(
               # bootstrapPage(
               absolutePanel(
                 id = "controls",
                 class = "modal-body",
                 fixed = FALSE, draggable = TRUE,
                 top = 50, left = "auto", right = 50, bottom = "auto",
                 width = 950, height = "auto",
                 
                 
                 HTML('<button  data-toggle="collapse" data-target="#demo5">Colapsar</button>'),
                 
                 tags$div(id = 'demo5',  class="collapse-in",
                          
                          tabsetPanel(
                            tabPanel("Cruzar variables", icon=icon("vials"),
                          
                          
                          h3("Cruzar capas"),
                          h6("Combina dos capas y las almacena en el entorno para que puedan ser utilizadas en cualquier otra sección de la app. Al hacer click en 'Añadir variable', cada variable nueva se añadirá al final de todas las listas de selección. Las variables creadas se perderán una vez finalizada la sesión."),
                          h6("De momento, todas los valores nulos e infinitos resultantes de una división serán transformados a ceros." ),
                          h6("Desambiguación:  División = A / B; Resta = A - B" ),
                          hr(),
                          
                          fluidRow("",
                                   div(style="display: inline-block;vertical-align:middle; height: 40px;width: 10px;",HTML("<br>")),
                                   div(style="display: inline-block;vertical-align:middle;height: 40px; width: 130px;",  actionButton("varlab", "Añadir variable")),
                                   div(style="display: inline-block;vertical-align:middle; height: 40px;width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 40px; width: 150px;",   selectInput("combType","Operación",choices = combinacion, selected = "Multiplicacion")),
                                   div(style="display: inline-block;vertical-align:middle;height: 40px; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 40px; width: 450px;",   h6("El ejemplo a continuación combina 'Precio del M2' con 'Superficie residencial' para valuar económicamente el real estate en cada hexágono."))
                              ),
                 
                          hr(),
                          
                          fluidRow("Var. A",
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 250px;", selectInput("listaA", "", choices = lista, selected="Precio M2 (USD)")),
                                   div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("transA", "",   choices = transforma)),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  checkboxInput("direccionA","Invertir serie", FALSE)),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 5px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 130px;",  plotlyOutput("histogramaA",width = 150, height = 75))
                                   
                          ) ,
                          
                          hr(),
                          
                          
                          fluidRow("Var. B",
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 250px;", selectInput("listaB", "", choices = lista, selected="Volumen Edificado Residencial")),
                                   div(style="display: inline-block;vertical-align:middle; height: 75px;width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  selectInput("transB", "",   choices = transforma)),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 15px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 150px;",  checkboxInput("direccionB","Invertir serie", FALSE)),    
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 5px;",HTML("<br>")),
                                   
                                   div(style="display: inline-block;vertical-align:middle;height: 75px; width: 130px;",  plotlyOutput("histogramaB",width = 150, height = 75))
                                   
                          ) ),
                          
                          tabPanel("Subir archivo CSV (new!)", icon=icon("file", lib="glyphicon"),
                                   h3("Subir archivo CSV"),
                                   
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                            
                                            div(style="display: inline-block;vertical-align:middle; height: 100px;width: 300px;",
                                   
                                   fileInput("subircsv", "Cargá archivo CSV con puntos basados en pares latitud/longitud", multiple = FALSE, accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"), width = NULL)),
                                   div(style="display: inline-block;vertical-align:middle; height: 85px;width: 20px;",HTML("<br>")),
                                   div(style="display: inline-block;vertical-align:middle; height: 85px;width: 460px;",h6("Subir archivo CSV separado por comas con columnas de coordenadas para latitud y longitud. La información puede añadirse a la malla hexagonal contando los puntos que caen en cada hexágono, o sumando o promediando una variable numérica a elección."))
                                   
                                   ),
                                   
                                   hr(),
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 175px;",  selectInput("csvLat", "Longitud:", choices = c("Subí un archivo primero"))),
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 15px;",HTML("<br>")),
                                            
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 175px;",  selectInput("csvLong", "Latitud:",choices = c("Subí un archivo primero") )),
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 15px;",HTML("<br>")),
                                            
                                            
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 450px;",  selectInput("csvData", "Variable de agregación (sólo numéricas):",choices = c("Subí un archivo primero") )))
                           
                                                          ,
                                   
                                          fluidRow("",
                                                   div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                                   div(style="display: inline-block;vertical-align:middle;height: 85px; width: 240px;",textInput("csvName", "Nombre que tendrá la variable:")),
                                                   div(style="display: inline-block;vertical-align:middle;height: 85px; width: 15px;",HTML("<br>")),
                                                   div(style="display: inline-block;vertical-align:middle;height: 85px; width: 170px;", conditionalPanel(
                                                     condition = "input.csvData != 'Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono'",   selectInput("csvAgr", "Tipo de agregación:", choices = c("Subí un archivo primero")))
                                          )),
                                   hr(),
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 10px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 140px;",actionButton("addCSV", "Añadir variable")),
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 340px;",h6("(Feature en proceso de testing. Reportar bugs al autor de la webapp.)"))
                 
            
             
                                   )
                                   
                                   

                                   
                                   
                                   ),
                          
                          tabPanel("Subir archivo SHP (new!)", icon=icon("shapes"),
                                   
                                   h3("Subir archivo SHP"),
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle; height: 100px;width: 300px;", 
                                   fileInput("subirshp", "Cargá capa vectorial en formato SHP", multiple = TRUE, accept = c(".shp", ".shx", ".dbf",".prj", ".qpj"))),
                          div(style="display: inline-block;vertical-align:middle; height: 85px;width: 20px;",HTML("<br>")),
                          div(style="display: inline-block;vertical-align:middle; height: 85px;width: 460px;",h6("Subir archivo SHP seleccionando todos sus componentes (*.prj, *.dbf, *.shx, *.qpj). Las formas posibles de agregar datos a la malla hexagonal dependerá de si la capa subida es de puntos, líneas o polígonos, lo que se detectará automáticamente.")
                          
                          )),
                                   hr(),
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                             
                                            
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 650px;",   selectInput("shpData", "Variable de agregación (sólo numéricas)",choices = c("Subí un archivo primero") ))
                                            
                                   ),
                                   
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 240px;",textInput("shpName", "Nombre que tendrá la variable:")),
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 15px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle;height: 85px; width: 250px;",
                                                 conditionalPanel(
                                                  condition = "input.shpData != ('Sin variable de agregación, sólo quiero contar cuantos puntos caen en cada hexágono' ||   'Sin variable de agregación, sólo quiero calcular las superficies de los polígonos y sumarlas en cada hexágono' ||  'Sin variable de agregación, sólo quiero calcular la longitud de las líneas y sumarlas en cada hexágono') ", 
                                                selectInput("shpAgr", "Tipo de agregación:", choices = c("Subí un archivo primero"))))
                                         
                                            ),
                                   hr(),
                                   fluidRow("",
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 10px;",HTML("<br>")),
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 140px;",actionButton("addSHP", "Añadir variable")),
                                            div(style="display: inline-block;vertical-align:middle; height: 55px;width: 340px;",h6("(Feature en proceso de testing. Reportar bugs al autor de la webapp.)"))
                                            
                                            
                                   
                                   
                          )),
                          
                     
                            tabPanel("Subir lista de domicilios (New!)", icon=icon("city"),
                                     
                                     
                                     
                                     h3("Subir lista de domicilios"),
                                     
                                     fluidRow("",
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                              
                                              div(style="display: inline-block;vertical-align:middle; height: 100px;width: 300px;",
                                                  
                                                  fileInput("subircsvCA", "Cargá archivo CSV con puntos basados en pares compuestos por calle y altura", multiple = FALSE, accept = c(
                                                    "text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv"), width = NULL)),
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 20px;",HTML("<br>")),
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 460px;",h6("Subir archivo CSV separado por comas con columnas separadas para calle y altura, que serán geolocalizados con más éxito cuanto mejor escritas estén las calles y alturas. La información puede añadirse a la malla hexagonal contando los puntos que caen en cada hexágono, o sumando o promediando una variable numérica a elección."))
                                              
                                     ),
                                     
                                     hr(),
                                     fluidRow("",
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 175px;",  selectInput("csvCACalle", "Calle:", choices = c("Subí un archivo primero"))),
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 15px;",HTML("<br>")),
                                              
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 175px;",  selectInput("csvCAAltura", "Altura:",choices = c("Subí un archivo primero") )),
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 15px;",HTML("<br>")),
                                              
                                              
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 450px;",  selectInput("csvCAData", "Variable de agregación (sólo numéricas)",choices = c("Subí un archivo primero") )))
                                     
                                     ,
                                     
                                     fluidRow("",
                                              div(style="display: inline-block;vertical-align:middle; height: 85px;width: 10px;",HTML("<br>")),
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 240px;",textInput("csvCAName", "Nombre que tendrá la variable:")),
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 15px;",HTML("<br>")),
                                              div(style="display: inline-block;vertical-align:middle;height: 85px; width: 170px;", conditionalPanel(
                                                condition = "input.csvCAData != 'Sin agregación, sólo quiero contar cuantos puntos caen en cada hexágono'",   selectInput("csvCAAgr", "Tipo de agregación:", choices = c("Subí un archivo primero")))
                                              )),
                                     hr(),
                                     fluidRow("",
                                              div(style="display: inline-block;vertical-align:middle; height: 55px;width: 10px;",HTML("<br>")),
                                              div(style="display: inline-block;vertical-align:middle; height: 55px;width: 140px;",actionButton("addCSVCA", "Añadir variable")),
                                              div(style="display: inline-block;vertical-align:middle; height: 55px;width: 340px;",h6("(Demora varios minutos. Feature en proceso de testing. Reportar bugs al autor de la webapp.)"))
                                              
                                              
                                              
                                     )
                                     
                                     
                                     
                                     
                                     
                            
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                           
                            )
                 
                 

                 ))), options = list(cancel = ".shiny-input-container")))
         ,
         
         
         
         tags$div(id="cite",
                  HTML('Contacto: Nahuel Patiño / nahuel89p@gmail.com / Twitter:@nahuelpat89 / <br> BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z / LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT'
                  ))
             )
,


# 
# tabPanel("Habilitar premium (new!)",icon=icon("unlock-alt"),
#          
#          fluidPage(
#            
#            div(id = "about", class = "card", style = "margin-top:4em;",
#                (div(style = "display: inline-block;height:18px;vertical-align:top;horizontal-align:middle;margin-top:-1em;margin-bottom:-1em;",  h3("Próximamente"))),
#                includeMarkdown("./premium.md")
#            ))
#          
# )   ,





tabPanel("About",icon=icon("info"),
         fluidPage(
           
           div(id = "about", class = "card", style = "margin-top:4em;",
               (div(style = "display: inline-block;height:18px;vertical-align:top;horizontal-align:middle;margin-top:-1em;margin-bottom:-1em;",  h3("About"))),
               includeMarkdown("./about.md")
           ))

       
         
         
)



)

)
)
