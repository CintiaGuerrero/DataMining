
library(tidyverse)
df_seeds <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', col.names =c('area','perimetro','compacto','longitud','anchura','coeficient.asimetria','longitud.ranura','tipo'))

#P1) --> El dataframe tiene 210 filas i 8 columnas

dim(df_seeds)


#P2)

df_seeds$tipo <- factor(df_seeds$tipo, labels = c("Kama", "Rosa", "Canadian"))


#P3) --> Kama = 14,33 ; Rosa = 18,33 ; Canadian = 11,87

media <- aggregate(area~tipo, df_seeds, mean, na.rm=TRUE)


#P4) --> grafico de cajas. La linea representa la mediana

ggplot(df_seeds, aes(x=tipo, y=area)) + geom_boxplot()


#P5) 

dispersion <- ggplot (df_seeds, aes(compacto, area)) + geom_point()
dispersion + geom_point(aes(colour = factor(tipo)))


#P6) --> Crea una nueva variable booleana con la función mutate, que indica si la semilla es tipo Kama (TRUE) o no (FALSE)

df_seeds |> mutate(is_kama = tipo=='Kama') -> df_seeds


#P7) --> Porque separandolos se puede evaluar el rendimiento del modelo

set.seed(123) # Este set.seed hace que a todos nos generen los mismos número aleatorios
idx <- sample(1:nrow(df_seeds), 0.7*nrow(df_seeds))
df_seeds_train <- df_seeds[idx,]
df_seeds_test <- df_seeds[-idx,]


#P8) --> Algoritmo de regresión logística


#P9) --> 

model <- glm(df_seeds_train, formula=is_kama~area+perimetro+compacto+longitud+coeficient.asimetria+longitud.ranura, family='binomial')

#P10) --> 

predictions <- predict(model, df_seeds_test, type='response')
predictions_binary <- ifelse(predictions >= 0.5, 1, 0)
confusion_matrix <- table(predictions_binary, df_seeds_test$is_kama)
precision <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[2,1])
recall <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])

print(paste("Precisión: ", precision))
print(paste("Exhaustividad: ", recall))
  

#P11) --> Agrupa los datos de las columnas area, perimetro, etc. en 3 grupos (clusters) con K-means en la variable c1
#Después ejectuta una tabla de contingencia que compara los 3 clusters con las distintas categorias de la variable tipo
#Permite ver la distribución de las semillas segun el cluster y el tipo de semilla.

set.seed(123)
cl<-df_seeds |> select(area,perimetro,compacto,longitud,anchura,coeficient.asimetria,longitud.ranura) |> kmeans(3)
table(real=df_seeds$tipo,cluster=cl$cluster)

