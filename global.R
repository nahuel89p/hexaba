library(ClustGeo)
library(dplyr)
library(geosphere)
library(leaflet)
library(plotly)
library(rgdal)
library(rgeos)
library(shiny)
library(shinydashboard)
library(shinyjqui)
library(shinythemes)
library(tidyverse)
library(viridis)
library(normalr)
library(sp)
library(raster)
library(rlang)
library(lwgeom)
library(GWmodel)
library(corrplot)

barrios <- c(
"AGRONOMIA",
"ALMAGRO",
"BALVANERA",
"BARRACAS",
"BELGRANO",
"BOCA",
"BOEDO",
"CABALLITO",
"CHACARITA",
"COGHLAN",
"COLEGIALES",
"CONSTITUCION",
"FLORES",
"FLORESTA",
"LINIERS",
"MATADEROS",
"MONSERRAT",
"MONTE CASTRO",
"NUEVA POMPEYA",
"NUNEZ",
"PALERMO",
"PARQUE AVELLANEDA",
"PARQUE CHACABUCO",
"PARQUE CHAS",
"PARQUE PATRICIOS",
"PATERNAL",
"PUERTO MADERO",
"RECOLETA",
"RETIRO",
"SAAVEDRA",
"SAN CRISTOBAL",
"SAN NICOLAS",
"SAN TELMO",
"VELEZ SARSFIELD",
"VERSALLES",
"VILLA CRESPO",
"VILLA DEL PARQUE",
"VILLA DEVOTO",
"VILLA GRAL. MITRE",
"VILLA LUGANO",
"VILLA LURO",
"VILLA ORTUZAR",
"VILLA PUEYRREDON",
"VILLA REAL",
"VILLA RIACHUELO",
"VILLA SANTA RITA",
"VILLA SOLDATI",
"VILLA URQUIZA")





switchfuncion <- function(x) {
(switch(x,
                    "Suma de los valores de los puntos" =  "sum",
                    "Promedio de los valores de los puntos" = "mean")) }



combinacion <- c("Multiplicacion",
                "Division",
                "Suma",
                "Resta"
                )


transforma <- c("Sin transformar",
                "boxcox",
                "log10",
                "Raiz cuadrada",
                "^2",
                "^1/3")


lista <-   c("Volumen Edificado" ,
                   "Poblacion" ,
                   "% de Extranjeros" ,
                   "% de Hogares con NBIs" ,
                   "Personas por hogar",
                   "Indice de Desarrollo Socioeconomico" ,
                   "Precio M2 (USD)" ,
                   "Tiempo de transp. publico hasta el centro (Google's Distance Matrix API)",
                   "% Elecciones 2017: Vamos Juntos",
                   "% Elecciones 2017: Unidad Portena",
                   "% Elecciones 2017: Evolucion Ciudadana",
                   "% Elecciones 2017: FIT",
                   "% Elecciones 2017: Otros",
                   "Area de Influencia del Subte (Ajustado por Usuarios por Estacion)" ,
                   "Suma de Metros Recorridos por Colectivos" ,
                   "Cant. Lineas de Colectivos en Paradas" ,
                   "Cantidad de Paradas de Colectivos" ,
                   "Metros de Avenidas (Vias principales)" ,
                   "Hurto Aumotor (II semestre 2018)" ,
                   "Robo Automotor (II semestre 2018)" ,
                   "Hurto sin Violencia (II semestre 2018)" ,
                   "Robo con Violencia (II semestre 2018)" ,
                   "Cant. Arboles (Calles)" ,
                   "Suma Altura Arboles (Calles)" ,
                   "Suma Diametro Arboles (Calles)" ,
                   "Suma D*A Arboles (Calles)" ,
                   "Cant. Arboles (Parques)" ,
                   "Suma Altura Arboles (Parques)" ,
                   "Suma Diametro Arboles (Parques)" ,
                   "Suma D*A Arboles (Parques)" ,
                   "Suma D*A Arboles (Calles y Parques)",
                   "% de Hogares con Calidad Media a Baja de Construccion" ,
                   "% de Hogares con Materiales de Calidad Media a Baja" ,
                   "Cant. comercios",
                   "Cant. comercios (sin rubro indumentaria)",
                   "Total reclamos SUACI 2018",
                   "(RECLAMOS SUACI 2018) Auto Mal Estacionado" ,
                   "Superficie No Edificable" ,
                   "Volumen Edificado Destinado a Actividades Civiles" ,
                   "Volumen Edificado Destinado a Actividades Comerciales" ,
                   "Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
                   "Volumen Edificado No Clasificado" ,
                   "Volumen Edificado Residencial" ,
                   "% Superficie No Edificable" ,
                   "% Volumen Edificado Destinado a Actividades Civiles" ,
                   "% Volumen Edificado Destinado a Actividades Comerciales" ,
                   "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
                   "% Volumen Edificado No Clasificado" ,
                   "% Volumen Edificado Residencial" ,
                   "Cajeros Automaticos" ,
                   "Farmacias" ,
                   "Gimnasios" ,
                   "Starbucks" ,
                   "McDonalds",
                   "Compuesto: Crimen (deciles)",
                   "Compuesto: Vegetacion (deciles)",
                   "Compuesto: Nivel Socioeconomico (deciles)",
                   "Compuesto: Acceso a Transporte (deciles)",
                   "Superficie edificada (deciles)")        



# 
# lista <-   c("Volumen Edificado" ,
#              "Poblacion" ,
#              "Cant. De Argentinos" ,
#              "Cant. De Extranjeros" ,
#              "% de Extranjeros" ,
#              "% de Hogares con NBIs" ,
#              "Personas por hogar",
#              "Indice de Desarrollo Socioeconomico" ,
#              "Precio M2 (USD)" ,
#              "Tiempo de transp. publico hasta el centro",
#              "% Elecciones 2017: Vamos Juntos",
#              "% Elecciones 2017: Unidad Portena",
#              "% Elecciones 2017: Evolucion Ciudadana",
#              "% Elecciones 2017: FIT",
#              "% Elecciones 2017: Otros",
#              "Area de Influencia del Subte (Ajustado por Usuarios por Estacion)" ,
#              "Suma de Metros Recorridos por Colectivos" ,
#              "Cant. Lineas de Colectivos en Paradas" ,
#              "Cantidad de Paradas de Colectivos" ,
#              "Metros de Avenidas (Vias principales)" ,
#              "Metros de Vias Principales (Todas)" ,
#              "Hurto Aumotor" ,
#              "Robo Automotor" ,
#              "Hurto sin Violencia" ,
#              "Robo con Violencia" ,
#              "Cant. Arboles (Calles)" ,
#              "Suma Altura Arboles (Calles)" ,
#              "Suma Diametro Arboles (Calles)" ,
#              "Suma D*A Arboles (Calles)" ,
#              "Cant. Arboles (Parques)" ,
#              "Suma Altura Arboles (Parques)" ,
#              "Suma Diametro Arboles (Parques)" ,
#              "Suma D*A Arboles (Parques)" ,
#              "Suma D*A Arboles (Calles y Parques)",
#              "% de Hogares con Calidad Media a Baja de Construccion" ,
#              "% de Hogares con Materiales de Calidad Media a Baja" ,
#              "Cant. comercios",
#              "Cant. comercios (sin rubro indumentaria)",
#              "Total reclamos SUACI 2018",
#              "(RECLAMOS SUACI 2018) Auto Mal Estacionado" ,
#              "(RECLAMOS SUACI 2018) Campana Verde Danada" ,
#              "(RECLAMOS SUACI 2018) Casa o Terreno Tomado" ,
#              "(RECLAMOS SUACI 2018) Contenedor de Basura Danado" ,
#              "(RECLAMOS SUACI 2018) Manteros" ,
#              "(RECLAMOS SUACI 2018) Plaga en Arbolado" ,
#              "(RECLAMOS SUACI 2018) Reparar Luminaria" ,
#              "(RECLAMOS SUACI 2018) Reparar Vereda" ,
#              "Superficie No Edificable" ,
#              "Volumen Edificado Destinado a Actividades Civiles" ,
#              "Volumen Edificado Destinado a Actividades Comerciales" ,
#              "Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
#              "Volumen Edificado No Clasificado" ,
#              "Volumen Edificado Residencial" ,
#              "% Superficie No Edificable" ,
#              "% Volumen Edificado Destinado a Actividades Civiles" ,
#              "% Volumen Edificado Destinado a Actividades Comerciales" ,
#              "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
#              "% Volumen Edificado No Clasificado" ,
#              "% Volumen Edificado Residencial" ,
#              "Area de Canteros" ,
#              "Area de Espacios Verdes" ,
#              "Cajeros Automaticos" ,
#              "Carnicerias RES" ,
#              "Farmacias" ,
#              "Gimnasios" ,
#              "Starbucks" ,
#              "Martinez" ,
#              "McDonalds",
#              "Compuesto: Crimen (deciles)",
#              "Compuesto: Vegetacion (deciles)",
#              "Compuesto: Nivel Socioeconomico (deciles)",
#              "Compuesto: Acceso a Transporte (deciles)",
#              "Superficie edificada (deciles)")      



defaultCorr <-  c(
  "Volumen Edificado",
  "Poblacion" ,
  "Personas por hogar",
  "Indice de Desarrollo Socioeconomico" ,
  "Precio M2 (USD)" ,
  "% de Hogares con Materiales de Calidad Media a Baja" ,
  "% Volumen Edificado Destinado a Actividades Comerciales" ,
  "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
  "% Volumen Edificado Residencial" ,
  "Compuesto: Acceso a Transporte",
  "% Elecciones 2017: Vamos Juntos",
  "% Elecciones 2017: Unidad Portena",
  "Area de Influencia del Subte (Ajustado por Usuarios por Estacion)")     







default2 <-   c("Volumen Edificado" ,
             "% de Hogares con NBIs" ,
             "Precio M2 (USD)" ,
             "% Volumen Edificado Destinado a Actividades Comerciales")



default <-  c(
  "Volumen Edificado",
  "Poblacion" ,
  "Personas por hogar",
  "Indice de Desarrollo Socioeconomico" ,
  "Precio M2 (USD)" ,
  "% de Hogares con Calidad Media a Baja de Construccion" ,
  "% de Hogares con Materiales de Calidad Media a Baja" ,
  "% Volumen Edificado Destinado a Actividades Civiles" ,
  "% Volumen Edificado Destinado a Actividades Comerciales" ,
  "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" ,
  "% Volumen Edificado Residencial" ,
  "Compuesto: Nivel Socioeconomico",
  "Compuesto: Acceso a Transporte")     


datosCSV <- read.csv("dataframe 530.csv",fileEncoding = "UTF-8")
# 
# names(datosCSV)[names(datosCSV)=="id"] <- "hexacities"
# 
# write.csv(datosCSV, "F:\\hexaBA\\dataframe 530.csv", fileEncoding = "UTF-8")



# datosCSV$BARRIO<-chartr("Ã‘", "N", datosCSV$BARRIO)
datosCSV$BARRIO<-chartr("?", "N", datosCSV$BARRIO)
datosCSV$BARRIO<-gsub("NUÃ¯Â¿Â½EZ", "NUNEZ", datosCSV$BARRIO)
datosCSV[is.na(datosCSV)] <- 0
datosCSV$hexacities <- as.integer(datosCSV$hexacities)
datosCSV <- datosCSV[sort(datosCSV$hexacities),] 
datosCSV$INSE <- round(datosCSV$INSE,2)
datosCSV$CPTreeIx <- datosCSV$CTreeIx + datosCSV$PTreeIx

datosCSV$Transporte <- (ntile(datosCSV$Subte , 20) + ntile(datosCSV$MtsBus, 20)  + ntile(datosCSV$ParadasBus , 20) + ntile(datosCSV$LineasBus1 , 20))/8
datosCSV$SupEdifd <- ntile(datosCSV$SupEdif, 10)  
datosCSV$Crimen <- (ntile(datosCSV$RoboConVio, 20)+ ntile(datosCSV$RobosAutos, 20)+ntile(datosCSV$HurtoAut_1, 20)+ntile(datosCSV$HurtoSinVi , 20))/8
datosCSV$NivelSocioecon <- (ntile(datosCSV$INSE, 20)  + ntile(datosCSV$PrecioM2, 20)) / 4
datosCSV$Vegetacion <- (ntile(datosCSV$CPTreeIx, 10))  


datosCSVboxcox <- read.csv("datosCSVboxcox530.csv",fileEncoding = "UTF-8")

 # names(datosCSVboxcox)[names(datosCSVboxcox)=="id"] <- "hexacities"
 #   
 # write.csv(datosCSVboxcox, "F:\\hexaBA\\datosCSVboxcox530.csv", fileEncoding = "UTF-8")


selected <- datosCSV[,c("hexacities"), drop=F]
as.data.frame(selected)
selected$hexacities <- as.integer(selected$hexacities)

testdata <- datosCSV[,c("hexacities", "RoboAutom"),drop=F]
testdata <- testdata[testdata[,2] == 1,]


shapeFile <- readOGR("dock subset 410w2.shp")
shapeFile <- spTransform(shapeFile, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# shapeFile$hexacities <- as.integer(shapeFile$hexacities)
shapeFile$hexacities <- as.integer(as.character(shapeFile$hexacities))

shapeFile2 <- shapeFile

# shapeFile2 <-shapeFile
# shapeFile3 <- shapeFile
map <- shapeFile
# shapeFile4 <- shapeFile

# write.csv(centroids, "centroids.csv")
# 
# centroids <- read.csv("centroids.csv")
centroids <- gCentroid(map, byid = TRUE, id = map@data$hexacities)
D.geo <- as.data.frame(geosphere::distm(centroids))
colnames(D.geo) <- map@data$hexacities
rownames(D.geo) <- map@data$hexacities


Normalizar <- function(x){((x-min(x))/(max(x)-min(x)))*100000}


Invertir <- function (x, y){
  x[,y] <- max(x[,y])-x[,y]
   z<-x
   as.data.frame(z)
  return(z)
}



DataType <- function(x) {
 
   if(is.null(switch(x,
                     "Compuesto: Acceso a Transporte (deciles)" =  "Transporte",
                     "Personas por hogar" = "pph",
                     "Compuesto: Nivel Socioeconomico (deciles)" = "NivelSocioecon",
                     "Compuesto: Vegetacion (deciles)" = "Vegetacion",
                     "Compuesto: Crimen (deciles)" = "Crimen",
                     "Superficie edificada (deciles)" = "SupEdifd",
                     "% de Extranjeros" = "rExtranjer",
                     "% de Hogares con Calidad Media a Baja de Construccion" = "rMalCalCon",
                     "% de Hogares con Materiales de Calidad Media a Baja" = "rMalCalMat",
                     "% de Hogares con NBIs" = "NBIratio",
                     "% Superficie No Edificable" = "rNoEdif",
                     "% Volumen Edificado Destinado a Actividades Civiles" = "rActCivil",
                     "% Volumen Edificado Destinado a Actividades Comerciales" = "rActComerc",
                     "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" = "rSPML",
                     "% Volumen Edificado No Clasificado" = "rNoClasif",
                     "% Volumen Edificado Residencial" = "rResidenci",
                     "(RECLAMOS SUACI 2018) Auto Mal Estacionado" = "AutoMalEst",
                     "(RECLAMOS SUACI 2018) Cable Cortado" = "CablCortOC",
                     "(RECLAMOS SUACI 2018) Calle Inundada" = "CalleInund",
                     "(RECLAMOS SUACI 2018) Campana Verde Danada" = "CampaVerde",
                     "(RECLAMOS SUACI 2018) Casa o Terreno Tomado" = "Squatted",
                     "(RECLAMOS SUACI 2018) Contenedor de Basura Danado" = "Contenedor",
                     "(RECLAMOS SUACI 2018) Derratizar Terreno" = "DerratTerr",
                     "(RECLAMOS SUACI 2018) Derratizar Via Publica" = "DerratViaP",
                     "(RECLAMOS SUACI 2018) Manteros" = "Manteros",
                     "(RECLAMOS SUACI 2018) Plaga en Arbolado" = "PlagaArb",
                     "(RECLAMOS SUACI 2018) Reclamos a Comercios" = "ReclComerc",
                     "(RECLAMOS SUACI 2018) Reparar Luminaria" = "ReparLumin",
                     "(RECLAMOS SUACI 2018) Reparar Vereda" = "ReparVered",
                     "Area de Canteros" = "Canteros",
                     "Area de Espacios Verdes" = "Parques",
                     "Area de Influencia del Subte (Ajustado por Usuarios por Estacion)" = "Subte",
                     "Robo Automotor (II semestre 2018)" = 'RoboAutom',
                     "Metros de Avenidas (Vias Complementarias)" = "AvsCompl",
                     "Metros de Avenidas (Vias Locales)" = "AvsLocal",
                     "Metros de Avenidas (Vias principales)" = "AvsPpal",
                     "Cajeros Automaticos" = "ATM",
                     "Cant. Arboles (Calles)" = "CntCTree",
                     "Cant. Arboles (Parques)" = "CntPtree",
                     "Cant. De Argentinos" = "Argentinos",
                     "Cant. De Extranjeros" = "Extranjero",
                     "Cant. Lineas de Colectivos en Paradas (Fuente Alternativa)" = "LineasBus2",
                     "Cant. Lineas de Colectivos en Paradas" = "LineasBus1",
                     "Cantidad de Paradas de Colectivos (Fuente Alternativa)" = "ParadasB_1",
                     "Cantidad de Paradas de Colectivos" = "ParadasBus",
                     "Carnicerias RES" = "res",
                     "Farmacias" = "Farmacias",
                     "Gimnasios" = "Gyms",
                     "Hogares" = "Hogares",
                     "Hogares con NBIs" = "HogConNBIs",
                     "Hurto sin Violencia (II semestre 2018)" = "HurtoSinVi",
                     "Hurto Aumotor (II semestre 2018)" = "HurtoAut_1",
                     "Indice de Desarrollo Socioeconomico" = "INSE",
                     "Martinez" = "Martinez",
                     "McDonalds" = "McDonalds",
                     "Poblacion" = "Poblacion",
                     "Precio M2 (USD)" = "PrecioM2",
                     "Robo con Violencia (II semestre 2018)" = "RoboConVio",
                     "Starbucks" = "Starbucks",
                     "Suma Altura Arboles (Calles)" = "CTreeHeigh",
                     "Suma Altura Arboles (Parques)" = "PtreeHeigh",
                     "Suma D*A Arboles (Calles)" = "CTreeIx",
                     "Suma D*A Arboles (Parques)" = "PTreeIx",
                     "Suma de Metros Recorridos por Colectivos" = "MtsBus",
                     "Suma Diametro Arboles (Calles)" = "CTreeDiam",
                     "Suma Diametro Arboles (Parques)" = "PTreeDiam",
                     "Superficie No Edificable" = "NoEdificab",
                     "Metros de Vias Principales (Todas)" = "ViasPpales",
                     "Volumen Edificado" = "SupEdif",
                     "Volumen Edificado Destinado a Actividades Civiles" = "ActCivil",
                     "Volumen Edificado Destinado a Actividades Comerciales" = "ActComerci",
                     "Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" = "SPML",
                     "Volumen Edificado No Clasificado" = "NoClasific",
                     "Volumen Edificado Residencial" = "Residencia",
                     "% Elecciones 2017: Vamos Juntos" = "rVamosJunt",
                     "% Elecciones 2017: Unidad Portena" = "rUC",
                     "% Elecciones 2017: Evolucion Ciudadana" = "rECO",
                     "% Elecciones 2017: FIT" = "rFIT",
                     "% Elecciones 2017: Otros" = "rOTROS",
                     "Cant. comercios" = "nComercios",
                     "Cant. comercios (sin rubro indumentaria)" = "nComSPilch",
                     "Total reclamos SUACI 2018" = "SUACItotal",
                     "Suma D*A Arboles (Calles y Parques)" = "CPTreeIx",
                     "Tiempo de transp. publico hasta el centro (Google's Distance Matrix API)" = "mtrSanNic"))){
    return (x) }
    else 
      switch(x,
             "Compuesto: Acceso a Transporte (deciles)" =  "Transporte",
             "Personas por hogar" = "pph",
             "Compuesto: Nivel Socioeconomico (deciles)" = "NivelSocioecon",
             "Compuesto: Vegetacion (deciles)" = "Vegetacion",
             "Compuesto: Crimen (deciles)" = "Crimen",
             "Superficie edificada (deciles)" = "SupEdifd",
             "% de Extranjeros" = "rExtranjer",
             "% de Hogares con Calidad Media a Baja de Construccion" = "rMalCalCon",
             "% de Hogares con Materiales de Calidad Media a Baja" = "rMalCalMat",
             "% de Hogares con NBIs" = "NBIratio",
             "% Superficie No Edificable" = "rNoEdif",
             "% Volumen Edificado Destinado a Actividades Civiles" = "rActCivil",
             "% Volumen Edificado Destinado a Actividades Comerciales" = "rActComerc",
             "% Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" = "rSPML",
             "% Volumen Edificado No Clasificado" = "rNoClasif",
             "% Volumen Edificado Residencial" = "rResidenci",
             "(RECLAMOS SUACI 2018) Auto Mal Estacionado" = "AutoMalEst",
             "(RECLAMOS SUACI 2018) Cable Cortado" = "CablCortOC",
             "(RECLAMOS SUACI 2018) Calle Inundada" = "CalleInund",
             "(RECLAMOS SUACI 2018) Campana Verde Danada" = "CampaVerde",
             "(RECLAMOS SUACI 2018) Casa o Terreno Tomado" = "Squatted",
             "(RECLAMOS SUACI 2018) Contenedor de Basura Danado" = "Contenedor",
             "(RECLAMOS SUACI 2018) Derratizar Terreno" = "DerratTerr",
             "(RECLAMOS SUACI 2018) Derratizar Via Publica" = "DerratViaP",
             "(RECLAMOS SUACI 2018) Manteros" = "Manteros",
             "(RECLAMOS SUACI 2018) Plaga en Arbolado" = "PlagaArb",
             "(RECLAMOS SUACI 2018) Reclamos a Comercios" = "ReclComerc",
             "(RECLAMOS SUACI 2018) Reparar Luminaria" = "ReparLumin",
             "(RECLAMOS SUACI 2018) Reparar Vereda" = "ReparVered",
             "Area de Canteros" = "Canteros",
             "Area de Espacios Verdes" = "Parques",
             "Area de Influencia del Subte (Ajustado por Usuarios por Estacion)" = "Subte",
             "Robo Automotor (II semestre 2018)" = 'RoboAutom',
             "Metros de Avenidas (Vias Complementarias)" = "AvsCompl",
             "Metros de Avenidas (Vias Locales)" = "AvsLocal",
             "Metros de Avenidas (Vias principales)" = "AvsPpal",
             "Cajeros Automaticos" = "ATM",
             "Cant. Arboles (Calles)" = "CntCTree",
             "Cant. Arboles (Parques)" = "CntPtree",
             "Cant. De Argentinos" = "Argentinos",
             "Cant. De Extranjeros" = "Extranjero",
             "Cant. Lineas de Colectivos en Paradas (Fuente Alternativa)" = "LineasBus2",
             "Cant. Lineas de Colectivos en Paradas" = "LineasBus1",
             "Cantidad de Paradas de Colectivos (Fuente Alternativa)" = "ParadasB_1",
             "Cantidad de Paradas de Colectivos" = "ParadasBus",
             "Carnicerias RES" = "res",
             "Farmacias" = "Farmacias",
             "Gimnasios" = "Gyms",
             "Hogares" = "Hogares",
             "Hogares con NBIs" = "HogConNBIs",
             "Hurto sin Violencia (II semestre 2018)" = "HurtoSinVi",
             "Hurto Aumotor (II semestre 2018)" = "HurtoAut_1",
             "Indice de Desarrollo Socioeconomico" = "INSE",
             "Martinez" = "Martinez",
             "McDonalds" = "McDonalds",
             "Poblacion" = "Poblacion",
             "Precio M2 (USD)" = "PrecioM2",
             "Robo con Violencia (II semestre 2018)" = "RoboConVio",
             "Starbucks" = "Starbucks",
             "Suma Altura Arboles (Calles)" = "CTreeHeigh",
             "Suma Altura Arboles (Parques)" = "PtreeHeigh",
             "Suma D*A Arboles (Calles)" = "CTreeIx",
             "Suma D*A Arboles (Parques)" = "PTreeIx",
             "Suma de Metros Recorridos por Colectivos" = "MtsBus",
             "Suma Diametro Arboles (Calles)" = "CTreeDiam",
             "Suma Diametro Arboles (Parques)" = "PTreeDiam",
             "Superficie No Edificable" = "NoEdificab",
             "Metros de Vias Principales (Todas)" = "ViasPpales",
             "Volumen Edificado" = "SupEdif",
             "Volumen Edificado Destinado a Actividades Civiles" = "ActCivil",
             "Volumen Edificado Destinado a Actividades Comerciales" = "ActComerci",
             "Volumen Edificado Destinado a Servicios-Produccion-Mantenimiento-Logistica" = "SPML",
             "Volumen Edificado No Clasificado" = "NoClasific",
             "Volumen Edificado Residencial" = "Residencia",
             "% Elecciones 2017: Vamos Juntos" = "rVamosJunt",
             "% Elecciones 2017: Unidad Portena" = "rUC",
             "% Elecciones 2017: Evolucion Ciudadana" = "rECO",
             "% Elecciones 2017: FIT" = "rFIT",
             "% Elecciones 2017: Otros" = "rOTROS",
             "Cant. comercios" = "nComercios",
             "Cant. comercios (sin rubro indumentaria)" = "nComSPilch",
             "Total reclamos SUACI 2018" = "SUACItotal",
             "Suma D*A Arboles (Calles y Parques)" = "CPTreeIx",
             "Tiempo de transp. publico hasta el centro (Google's Distance Matrix API)" = "mtrSanNic")}



USIG_geocode <- function(address) {
  
  base_url <- "http://servicios.usig.buenosaires.gob.ar/normalizar/"
  
  make_address_query <- function(address) {
    query <- paste0(base_url,
                    "?direccion=",
                    address,
                    "&geocodificar=TRUE")
  }
  
  query <- URLencode(make_address_query(address))
  
  # flog.info(paste("Trying", query))
  
  results <- tryCatch(httr::GET(query),
                      error = function(error_message) {return(NULL)})
  
  
  
  if (!httr:::is.response(results)) {
    data.frame(address = address, address_normalised = NA,
               lat = NA, lng = NA, stringsAsFactors = FALSE)
  } else {
    results <- httr::content(results)
    if (length(results$direccionesNormalizadas)) {
      x <- results$direccionesNormalizadas[[1]]$coordenadas$x
      y <- results$direccionesNormalizadas[[1]]$coordenadas$y
      address_normalised <- results$direccionesNormalizadas[[1]]$direccion
    } else {
      x <- NA
      y <- NA
      address_normalised <- NA
    }
    
    data.frame(address = address,
               address_normalised = ifelse(is.null(address_normalised), NA, address_normalised),
               lat = ifelse(is.null(y), NA, y),
               lng = ifelse(is.null(x), NA, x),
               stringsAsFactors = FALSE)
  }
  
}



