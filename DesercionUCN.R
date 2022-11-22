d1=student_antofa
d2=student_coq
print(nrow(d2)) # 649 estudiantes

### Unir archivos csv a un nuevo df:
d3=merge(d1,d2,by=c("carrera","sex","edad","direccion","tamfam","Pstatus","Medu","Pedu","Mtrabajo","Ptrabajo","razon","apoyopsi","internet"))
print(nrow(d3)) # 382 students

### Dar forma al conjunto de datos:
# Crear una nueva columna binaria llamada "retencion" (1= retenidos; 0= no retenidos):
d2$retencion <- set.seed(649) # crear la nueva columna y establecer la semilla para asegurar que obtendremos la misma distribución al obtener los números binarios aleatorios (1 y 0)
d2$retencion <- sample(1:0, 649, replace=T, prob=c(retencion = 0.85, 0.15)) # estableciendo las cuentas de 1 y 0 que queremos obtener
d2 <- data.frame(d2)

# Contar elementos binarios (doble verificación):
table(d2$retencion) # se debería obtener 0= 111, and 1= 538
View(d2)

### Crear códigos ficticios (binarios) para las variables existentes:
install.packages("fastDummies")
library('fastDummies')

dataDM <- dummy_cols(d2, select_columns = c('carrera', 'sex', 'direccion',
                                            'tamfam','Mtrabajo', 'Pstatus', 'internet', 'AORA',
                                            'sopfam', 'ayudantia', 'actividades', 'hijos',
                                            'AORA', 'sopfam','ayudantia', 'actividades', 'hijos'),
                     remove_first_dummy = TRUE)
head(dataDM)

str(dataDM)

# Crear un nuevo marco de datos solamente con todas las variables numéricas y ficticias que vamos a estudiar:
dataA <- dataDM[,c("retencion", "carrera_INGECO", "sex_M", "edad", "direccion_U", "tamfam_Me3", "Mtrabajo_salud", "Mtrabajo_otro", "Mtrabajo_servicio", "Mtrabajo_profesor", "Pstatus_S", "Medu", "Pedu", "internet_si", "AORA_si", "sopfam_si", "ayudantia_si", "actividades_si", "hijos_si", "tiempoviaje", "tiempoestudio", "reprobacion", "relfam", "trabajo", "adaptado", "entresem", "findesem", "salud", "ausencias", "P1", "P3", "P5")]

str(dataA)

# * Probablemente sería más eficiente eliminar las columnas que no queríamos en el df *head(dataA)

# Verificar nombres de las columnas:
ls(dataA) # o 'names(data)'
# Verificar dimensiones (número de rollos y columnas):
dim(dataA) # o 'nrow(data)' y 'ncol(data)'
# Revisar clase:
class(dataA)


### Comprobar valores faltantes:
sum(is.na(dataA)) # Tenemos cero datos faltantes.
any(is.na(dataA)) # Devuelve FALSO porque no tenemos datos faltantes 
# Trazar la matriz de valores perdidos:
install.packages("Amelia") # es posible que se le plantee una pregunta
library(Amelia)

# Ejecutar todo esto para trazar el mapa:
missmap(dataA, col = c("indianred", "lightgreen"),
        main = "Missing Values vs Observed")

# Exportar los datos fusionados/binarios a un archivo csv:
write.csv(dataA,'Binary_DF.csv')



######################################
### FASE 2 - ANALISIS EXPLORATORIO ###
######################################

# gráfico de barras agrupadas
install.packages("tidyverse")
library(ggplot2)

#Gráfico de barras de retención por tipoentrada:
RT <-ggplot(dataDM, aes(x = retencion)) + geom_bar(position = "dodge")
RT

RG1 <-ggplot(dataDM, aes(x = retencion, fill = tipoentrada)) + geom_bar(position = "dodge")
RG1


#Gráfico de dispersión con línea de ajuste lineal
ggplot(dataDM,
       aes(x = P1, 
           y = P3)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")


#library(reshape2)
#sp <- ggplot(dataDM, aes(x=P5, y=edad, colour = factor(sex))) + geom_point(shape=7)
#sp + facet_grid(sex ~ direccion)

install.packages("GGally")
library(GGally)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("viridis")
library(viridis)




##################################################################
### FASE 3 - REDUCCIÓN DE DIMENSIONALIDAD / Evaluación inicial ###
##################################################################

### Detección de valores atípicos:
boxplot(dataA, col="steelblue")#, boxwex = TRUE)

boxplot(dataA$ausencias, col="steelblue")

# Trazar un gráfico bivariante para las variables P1 y P32:
install.packages("HSAUR2")
library("HSAUR2")
install.packages("tools")
library("tools")
install.packages("MVA")
library("MVA")


# extraer las variables "P1" y "P3" del dataframe 
mydata <- dataA[, c("P1", "P3")] 

#Correr estas dos líneas juntas:
bvbox(mydata, xlab="P1", ylab="P3", type="n")
text(mydata$P1, mydata$P3, cex = 0.6, col="steelblue")

### Histograma con boxplot para la variable "p5":
mydata2 <-dataA[,c("P1")]
# Diseño para dividir la pantalla
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Dibujar el boxplot y el histograma 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(mydata2 , horizontal=TRUE , ylim=c(1,7), xaxt="n" , 
        col="steelblue", frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(mydata2 , breaks=40 , col="steelblue", border=T , 
     main="" , xlim=c(1,7))


### Asimetría y curtosis: para la variable P1:
install.packages("moments")
library(moments)
skewness(dataA)
kurtosis(dataA)

### Dendograma/Matriz de correlación:
install.packages("gplots")
library("gplots")

# Crear matriz
dataM <- data.matrix(dataA)
# Matriz de correlación:
data_cor <- cor(dataM)
# Matriz de correlación de dendogramas:
heatmap.2(data_cor, scale="none", col=redblue(100),
          trace="none", density.info="none")


# Matriz de correlacion:
install.packages("corrplot")
library(corrplot)
correlations <- cor(data_cor)
corrplot(correlations, tl.col = "gray", method="circle", order = "hclust") # añadir = 3


### PCA:
data_PCA <- princomp(dataA, cor=T)
summary(data_PCA, loading = T)
score <- data_PCA$scores
head(score)
# biplot opcional:
# biplot(data_PCA, col=c("black", "red"), cex = 0.6) 

# Plots PCA:
install.packages("devtools")
library(usethis)
library(devtools)

install_github("vqv/ggbiplot")
force = TRUE
##install_github("ggbiplot")

install.packages("ggplot2")
install.packages("plyr")
install.packages("scales")
install.packages("grid")
library(ggbiplot)

ggbiplot(data_PCA, pc.biplot = TRUE, alpha = 0, varname.adjust = 2, varname.size = 3.5)


# Trazar el gráfico de barras para visualizar el porcentaje acumulativo de PCA:
install.packages("factoextra") #no funcionará en MAC
library(factoextra)

# Diagrama de flujo de las dimensiones:
fviz_screeplot(data_PCA, main=" ",ncp=50) # añadir etiquetas: addlabels = TRUE

# Biplot - Dos componentes principales:
fviz_pca_biplot(data_PCA,col.var="contrib", invisible = "ind", habillage ="none", geom = "text", labelsize=4) + theme_minimal()

# Contribuciones de las variables a PC1
fviz_contrib(data_PCA, choice = "var", axes = 1, top = 10)

# Contribuciones de las variables a PC2
fviz_contrib(data_PCA, choice = "var", axes = 2, top = 10)

### Librería VIF, si es necesario:
install.packages("car")
install.packages("carData")
library("carData")
library("car")


### Construir un modelo Logit para probar la multicolinealidad:
# Realizar una regresión Logit para tratar la colinealidad:
logitRet <- glm(retencion ~ carrera_INGECO + sex_M + edad + direccion_U + tamfam_Me3 + Pstatus_S +
                  Mtrabajo_salud + Mtrabajo_otro + Mtrabajo_servicio + Mtrabajo_profesor +
                  Medu + Pedu + internet_si + AORA_si + sopfam_si + ayudantia_si +
                  actividades_si + hijos_si + tiempoviaje + tiempoestudio + reprobacion + relfam +
                  trabajo + adaptado + entresem + findesem + salud + ausencias + P1 + P3 + P5,
                data=dataA, family=binomial(link="logit"))
summary(logitRet)


### Realizar el análisis VIF, para excluir las variables con alta colinealidad:
vif(logitRet)

# Gráfico de barras para VIF:
# crear un vector de valores VIF
vif_values <- vif(logitRet)
#crear un gráfico de barras horizontales para mostrar cada valor de VIF
barplot(vif_values, main = "VIF Values - Collinearity", horiz = FALSE,
        col = "steelblue", cex.names = 0.8)
#añadir una línea vertical en el 5
abline(h = 5, lwd = 2, lty = 2, col='red')

# Crear un nuevo df sin colinealidad (excluyendo manualmente las variables P3, findesem y entresem):
dataC1 <- dataA[,c("retencion", "carrera_INGECO", "sex_M","edad", "direccion_U","tamfam_Me3", "Pstatus_S",
                   "Mtrabajo_salud", "Mtrabajo_otro", "Mtrabajo_servicio", "Mtrabajo_profesor",
                   "Medu", "internet_si", "AORA_si", "sopfam_si", "ayudantia_si", 
                   "actividades_si", "hijos_si", "tiempoviaje", "tiempoestudio", "reprobacion", "relfam",
                   "trabajo", "adaptado", "entresem", "salud", "ausencias", "P1", "Pedu", "findesem")]


# Ejecutar un nuevo modelo Logit sin colinealidad:
logitRet_C1 <- glm(retencion ~ carrera_INGECO + sex_M + edad + direccion_U + tamfam_Me3 + Pstatus_S +
                     Mtrabajo_salud + Mtrabajo_otro + Mtrabajo_servicio + Mtrabajo_profesor +
                     Medu + internet_si + AORA_si + sopfam_si + ayudantia_si +
                     actividades_si + hijos_si + tiempoviaje + tiempoestudio + reprobacion + relfam +
                     trabajo + adaptado + entresem + salud + ausencias + P1 + Pedu + findesem,
                   data=dataC1, family=binomial(link="logit"))
summary(logitRet_C1)


# Obtener la nueva salida de VIF:
vif(logitRet_C1)

# crear un vector de valores VIF:
vif_values <- vif(logitRet_C1)
#crear un gráfico de barras horizontales para mostrar cada valor de VIF:
barplot(vif_values, main = "VIF Values - No Collinearity", horiz = FALSE,
        col = "steelblue", cex.names = 0.8)
#añadir una línea vertical en el 5
abline(h = 2.5, lwd = 2, lty = 2, col='red')

# Ejecutar un nuevo modelo Logit sólo con las variables estadísticamente significativas:
dataC <- dataC1[,c("retencion", "entresem", "sopfam_si", "Medu", "Pedu", "AORA_si")]

logitRet_C <- glm(retencion ~ entresem + sopfam_si + Medu + Pedu + AORA_si,
                  data=dataC, family=binomial(link="logit"))
summary(logitRet_C)

# Ejecutar un nuevo modelo Logit sólo con las variables estadísticamente significativas a "0,05":
dataC <- dataC1[,c("retencion", "entresem", "sopfam_si", "Medu")]

logitRet_C <- glm(retencion ~ entresem + sopfam_si + Medu,
                  data=dataC, family=binomial(link="logit"))
summary(logitRet_C)

# Obtener una nueva salida VIF:
vif(logitRet_C)

# Crear valores de vector VIF
vif_values <- vif(logitRet_C)
#crear un gráfico de barras horizontales para mostrar cada valor de VIF
barplot(vif_values, main = "VIF Values - No Collinearity", horiz = FALSE,
        col = "steelblue", cex.names = 0.8)
#añadir una línea vertical en el 5
abline(h = 0.99, lwd = 2, lty = 2, col='red')

# Crear nueva matriz de correlación:
dataM2 <- data.matrix(dataC)
# Matriz de correalación
data_cor2 <- cor(dataM2)
correlations2 <- cor(data_cor2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

install.packages("corrplot")
library("corrplot")
corrplot(correlations2, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .6,
         addCoef.col = "black", # Añadir coeficiente de correalción
         tl.col = "gray", tl.srt = 90, # Color y rotación de la etiqueta de texto
         # Combinar con la importancia
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # ocultar el coeficiente de correlación en la diagonal principal
         diag = FALSE)


#Matriz "kdepairs": 
install.packages("ResourceSelection")
library(ResourceSelection) 
cor_dataA <- cor(dataA)
kdepairs(data_cor2)



#######################################
### FASE 4 - APRENDIZAJE AUTOMÁTICO ###
#######################################

ls(dataC) # revisar columnas
dim(dataC) # reducción en número de variables (de 28 a 24)
table(dataC$retencion)

head(dataC)


# Hacer que todas las variables sean un factor:
dataC[sapply(dataC, is.numeric)] <- lapply(dataC[sapply(dataC, is.numeric)], as.factor)

str(dataC)
table(dataC$retencion)

### Tratamiento de la clasificación desequilibrada en el regresor (variable de reteción):
### Tres opciones/técnicas disponibles: SMOTE, splitBalanced y ROSE.

install.packages("caret")
library("caret")
## Crear un modelo de referencia (desequilibrado) para compararlo con un conjunto de modelos equilibrados:
library(caret)
set.seed(649)
splitIndex <- createDataPartition(dataC$retencion, p = .85,
                                  list = FALSE,
                                  times = 1)
trainSplit <- dataC[splitIndex,]
nrow(trainSplit) # 553 estudiantes
table(trainSplit$retencion) # 1= 458, 0=95

# Revisar proporciones:
prop.table(table(trainSplit$retencion))

testSplit <- dataC[-splitIndex,] 
nrow(testSplit) # 96 estudiantes

# Librería para equilibrar el clasificador:
install.packages("ROSE")
library(ROSE)

# Comprobar el rendimiento del modelo de desequilibrio:
library(rpart)

treeimb <- rpart(retencion ~ ., data = trainSplit)
pred.treeimb <- predict(treeimb, newdata = testSplit)
head(pred.treeimb)

accuracy.meas(testSplit$retencion, pred.treeimb[,2])

roc.curve(testSplit$retencion, pred.treeimb[,2], plotit = FALSE)

### Equilibrar los datos:
# Opción de remuestreo 1 (over):
data.bal.ov <- ovun.sample(retencion ~ ., data = trainSplit, method = "over",
                           p=0.5, seed = 2)$data
table(data.bal.ov$retencion)

# Opción de remuestreo 2 (under):
data.bal.un <- ovun.sample(retencion ~ ., data = trainSplit, method = "under",
                           p = 0.5, seed = 1)$data
table(data.bal.un$retencion)


# Opción de remuestreo 3 (both):
data.bal.ou <- ovun.sample(retencion ~ ., data = trainSplit, method = "both",
                           N = 553, p = 0.5, seed = 2)$data
table(data.bal.ou$retencion)

# Opción de remuestreo 4 (ROSE):
data.rose <- ROSE(retencion ~ ., data = trainSplit, seed = 1)$data

table(data.rose$retencion)


# Entrenamiento de los clasificadores y ejecución del conjunto de pruebas mediante árboles de clasificación:
library(rpart)
tree.ov <- rpart(retencion ~ ., data = data.bal.ov)
tree.un <- rpart(retencion ~ ., data = data.bal.un)
tree.ou <- rpart(retencion ~ ., data = data.bal.ou)
tree.rose <- rpart(retencion ~ ., data = data.rose)

# Predecir en los nuevos datos (prueba):
pred.tree.ov <- predict(tree.ov, newdata = testSplit)
pred.tree.un <- predict(tree.un, newdata = testSplit)
pred.tree.ou <- predict(tree.un, newdata = testSplit)
pred.tree.rose <- predict(tree.rose, newdata = testSplit)

# Trazar la curva ROC - Evaluación del modelo:
roc.curve(testSplit$retencion, pred.tree.rose[,2], col = 0, main= "AUC: 0.75", lty = 1)
roc.curve(testSplit$retencion, pred.tree.ov[,2], add.roc = TRUE, col = 12, lty = 2) 
roc.curve(testSplit$retencion, pred.tree.un[,2], add.roc = TRUE, col = 0, lty = 3) 
roc.curve(testSplit$retencion, pred.tree.ou[,2], add.roc = TRUE, col = 0, lty = 4)

### Comprobación de puntos de aprendizaje automático:
dataC <- dataC1[,c("retencion", "entresem", "sopfam_si", "Medu")]
dataC[,'retencion'] <- as.factor(as.character(dataC[,'retencion']))

dim(dataC)

# Crear datos de entrenamiento equilibrados para LR:
dataC <- ovun.sample(retencion ~ ., data = dataC, method = "both",
                     N = 1298, seed =2)$data # 1298 (649x2)
table(dataC$retencion)
str(dataC)


#dataC[sapply(dataC, is.factor)] <- lapply(dataC[sapply(dataC, is.factor)], as.numeric)

str(dataC)


install.packages("mlbench")
library(mlbench)
library(caret)

# Establecer opciones de prueba (validación cruzada de 10 veces con 3 repeticiones)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

# Establezca la métrica de evaluación:
metric <- "Accuracy"

# Escala y centra los datos:
preProcess=c("center", "scale")

# Ejecutar múltiples modelos ML:
# Análisis discriminante lineal
set.seed(seed)
fit.lda <- train(retencion~., data=dataC, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)

# Regresión Lineal
set.seed(seed)
fit.glm <- train(retencion~., data=dataC, method="glm", metric=metric, trControl=control)

# GLMNET
set.seed(seed)
fit.glmnet <- train(retencion~., data=dataC, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)

# SVM Radial
set.seed(seed)
fit.svmRadial <- train(retencion~., data=dataC, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)

# kNN
set.seed(seed)
fit.knn <- train(retencion~., data=dataC, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

# Naive Bayes
set.seed(seed)
fit.nb <- train(retencion~., data=dataC, method="nb", metric=metric, trControl=control)

# CART
set.seed(seed)
fit.cart <- train(retencion~., data=dataC, method="rpart", metric=metric, trControl=control)

# C5.0
set.seed(seed)
fit.c50 <- train(retencion~., data=dataC, method="C5.0", metric=metric, trControl=control)

# Bagged CART
set.seed(seed)
fit.treebag <- train(retencion~., data=dataC, method="treebag", metric=metric, trControl=control)

# Bosque aleatorio
set.seed(seed)
fit.rf <- train(retencion~., data=dataC, method="rf", metric=metric, trControl=control)

# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(retencion~., data=dataC, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Seleccionar modelo:
results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))

# Tabla de comparación
summary(results)

# boxplot de comparación
bwplot(results)
# Dot-plot de comparación
dotplot(results)
