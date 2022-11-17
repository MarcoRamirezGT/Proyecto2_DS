# install.packages(c("tidyverse", "udpipe", "tm", "tidytext")) # las hemos instalado en capítulos anteriores
# install.packages(c("caTools", "caret", "randomForest", "rpart", "rpart.plot", "ROSE")) # las usaremos por primera vez

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)         #For Naive Bayes
library(caret)         #For the Confusion Matrix
library(tidyverse) # para manipular tablas


library(gmodels) #provides CrossTable() function for comparison

db<-read.csv('train.csv')
db<-db[,c(1,3,5)]
colnames(db)<-c('id','oracion','clase_oracion')
# db[nrow(db)+1,]<-c(test_txt,'Ineffective')

# db<-head(db,200)

# db$Tag<-factor(db$Tag)
glimpse(db) # exploramos la estructura de los datos
table(db$clase_oracion) # vemos la distribución de las clasificaciones
#install.packages("udpipe") # instalamos la libreria
library(udpipe) # la cargamos

modelo_sp <- udpipe::udpipe_download_model('english') # descarga el modelo y guarda la referencia  
modelo_sp$file_model # refrencia al modelo descargado
modelo_sp <- udpipe_load_model(file = modelo_sp$file_model) # cargamos el modelo en memoria

oraciones_anotadas <- udpipe_annotate( 
  object = modelo_sp, # el modelo de idioma
  x = db$oracion, # el texto a anotar, 
  doc_id = db$id, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 100
) %>% as.data.frame(.) # convertimos el resultado en data frame

oraciones_anotadas2 <- oraciones_anotadas %>% 
  filter(upos %in% c("ADJ","NOUN","VERB") ) %>% # filtramos por tipo de palabra
  filter(lemma != "bigdata") %>% # sacamos la expresion bigdata que estará en todas las oraciones
  select(doc_id, lemma) # nos quedamos sólo con los lemmas

glimpse(oraciones_anotadas2)

library(tidytext) # para manejar texto
library(tm) # para vectorizar
or_dtm <- oraciones_anotadas2 %>%
  count(doc_id, lemma) %>%
  tidytext::cast_dtm(document = doc_id, term = lemma, value = n)


or_dtm

or_dtm <- tm::removeSparseTerms(or_dtm, sparse = .98)
or_dtm

set.seed(100) # seteamos una semilla, para poder reproducir los resultados

# transformamos nuestra matriz de la manera requerida por caTools
or_dtm2 = as.data.frame(as.matrix(or_dtm))
colnames(or_dtm2) = make.names(colnames(or_dtm2))
db<-head(db,nrow(or_dtm2))

or_dtm2$clase_oracion  = as.factor(db$clase_oracion) # reincluimos la clase como factor

library(caTools) # cargamos la librería para hacer el split
division = sample.split(or_dtm2$clase_oracion, SplitRatio = 0.7) # divide 70/30
or_train = subset(or_dtm2, division==TRUE) # subconjunto de entrenamiento
or_test = subset(or_dtm2, division==FALSE) # subconjunto de testeo


library(randomForest) # cargamos la librería
or_rf = randomForest(clase_oracion ~ ., data=or_train) # generar modelo

or_rf_predict = predict(object=or_rf, newdata=or_test) # predecimos con tabla test, para evaluar contra real


View(or_rf_predict)

confusionMatrix(or_test$clase_oracion,or_rf_predict)

test_txt<-("though some say that life on Mars does exist, I think that there is no life on Mars.")

### Prediccion de texto
# 
#
# id<-c('1')
# clase<-c('Adequate')
# db_test<-data.frame(id,test_txt,clase)
# 
# colnames(db_test)<-c('id','oracion','clase_oracion')
# 
# 
# oraciones_anotadas_Test <- udpipe_annotate( 
#   object = modelo_sp, # el modelo de idioma
#   x = db_test$oracion, # el texto a anotar, 
#   doc_id = db_test$id, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
#   trace = 100
# ) %>% as.data.frame(.) # convertimos el resultado en data frame
# 
# oraciones_anotadas2_test <- oraciones_anotadas_Test %>% 
#   filter(upos %in% c("ADJ","NOUN","VERB") ) %>% # filtramos por tipo de palabra
#   filter(lemma != "bigdata") %>% # sacamos la expresion bigdata que estará en todas las oraciones
#   select(doc_id, lemma) # nos quedamos sólo con los lemmas
# 
# glimpse(oraciones_anotadas2_test)
# 
# or_dtm <- oraciones_anotadas2_test %>%
#   count(doc_id, lemma) %>%
#   tidytext::cast_dtm(document = doc_id, term = lemma, value = n)
# 
# 
# or_dtm
# 
# or_dtm_test <- tm::removeSparseTerms(or_dtm, sparse = .98)
# or_dtm_test
# 
# set.seed(100) # seteamos una semilla, para poder reproducir los resultados
# 
# # transformamos nuestra matriz de la manera requerida por caTools
# or_dtm2_test = as.data.frame(as.matrix(or_dtm_test))
# colnames(or_dtm2_test) = make.names(colnames(or_dtm2_test))
# db_test<-head(db_test,nrow(or_dtm2_test))
# 
# or_dtm2_test$clase_oracion  = as.factor(db_test$clase_oracion) # reincluimos la clase como factor
# 
# 
# # division = sample.split(or_dtm2$clase_oracion, SplitRatio = 0.7) # divide 70/30
# # or_train = subset(or_dtm2, division==TRUE) # subconjunto de entrenamiento
# or_test_test = subset(or_dtm2_test, division==FALSE) # subconjunto de testeo
# 
# 
# predict(object=or_rf, newdata=test_txt) # predecimos con tabla test, para evaluar contra real
