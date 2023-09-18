<hr>

#### Descripción
Módulo experimental de data science y urbanismo escrito en R. La mayoría de las bases de datos fueron obtenidas del [portal de datos abiertos de la Ciudad de Buenos Aires](https://data.buenosaires.gob.ar/)  y en algunos casos presentan algun tipo de procesamiento por el autor de esta webapp.

#### Breves notas metodológicas


 -  Cada hexágono mide 250 metros de lado a lado, componiendo una grilla de 3439 hexagonos.


  
 -  El método de agregacion de valores en hexágonos es directo (predicado geométrico de inclusión) en el caso de las unidades geoespaciales basadas en puntos, y por imputación proporcional por area en el caso de las unidades geoespaciales basadas en polígonos (como radios censales y secciones electorales) tras practicar la operacion 'intersect' (predicado geométrico de intersección) en el software QGIS.
  
 -  Para el caso de usos del suelo se combinaron las bases de [parcelas](https://data.buenosaires.gob.ar/dataset/parcelas) y [usos del suelo](https://data.buenosaires.gob.ar/dataset/relevamiento-usos-suelo) a los fines de imputarle de modo estimativo un valor de superficie (metros cuadrados) a cada subestructura edilicia (por ejemplo, muy a menudo en un mismo edificio coexisten departamentos residenciales con locales comerciales en planta baja).
  
 -  La librería "GWmodel" es utilizada para la regresión espacialmente ponderada (GWR) y la librería "Clustgeo" es utilizada para clusterizar.
  
 
  
   	

 
#### Autor:


Nahuel Patiño, lic. en Ciencia Política por la Universidad Católica de Córdoba, trabaja en el Ministerio de Ambiente y Espacio Público de la ciudad de Buenos Aires, y actualmente cursa el primer año de la maestría de Data Mining en la Universidad Austral.  
  [Twitter](https://twitter.com/nahuelpat89) -  [LinkedIn](https://www.linkedin.com/in/nahuelp89/) - [Flickr](https://www.flickr.com/photos/28404816@N08/)

BTC tip jar: 12NQRGkkUbmejwspCcUtvYoMWYq3tBMG6Z
 
LTC: MV7hNjWjjshUCF3P2BEUkPDG5vc4vaEYdT