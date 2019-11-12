
#####  Centro de Investigaciones en Matemáticas   #####
#              Curso propedéutico                     #
#                MAEC INEGI 2019                      #
#       Introducción a la programación con R          #
#            Alumno: Raúl Mejía González              #
#                                                     #
####                  Tarea 1                      #### 



#### Problema 1 ####
#
# Usando las instrucciones seq() y rep() genere las siguientes sucesiones: 
#  a) 10 10 10 10 10 9 9 9 9 8 8 8 7 7 6 5 4 4 3 3 3 2 2 2 2 1 1 1 1 1
rep(seq(10,1),c(seq(5,1),(seq(1,5))))
#
# b) 1 1 2 3 3 4 5 5 6 7 7 8 9 9 10
rep(seq(10),rep(c(2,1),5))
#
# c) 100.0000 100.2222 100.4444 100.6667 100.8889 101.1111 101.3333 101.5556
#    101.7778 102.0000
seq(100,102,length=10)




#### Problema 2 ####
# Genere un vector de nombre np cuyas componentes sean 100 números generados 
# al azar de la distribución de Poisson de parámetro 10.
np <- rpois(100, 10)
np
#
# a) Obtenga el máximo, mínimo, media, mediana y rango de np.
max(np)
min(np)
mean(np)
median(np)
range(np) # muestra el mínimo y máximo que constituyen el rango
# Estrictamente, el valor numérico del rango sería:
range(np)[2]-range(np)[1]
#
# b) ¿Cuántos valores distintos hay en el vector?
length(unique(np))
#
# c) ¿En qué lugar se encuentran el máximo y el mínimo?
order(np)[1] # posición del mínimo dentro del vector np
order(np)[100] # posición del máximo dentro del vector np




#### Problema 3 ####
# Genere un vector x con 10 números aleatorios de la distribución 
# chi^2 con 5 grados de libertad.
x <- rchisq(10,5)
x



#### Problema 4 ####
# Genere un vector y con 10 valores de la distribución de Poisson 
# con parámetro 10.
y <- rpois(10,10)
y



#### Problema 5 ####
# Obtenga los cuantiles 95 y 99 de las distribuciones exponencial 
# de parámetro 1, Cauchy con los parámetros por defecto, 
# t con 10 grados de libertad y chi^2 con 20 grados de libertad.
#
# exponencial:
qexp(c(0.95,0.99),1)
#
# Cauchy:
qcauchy(c(0.95,0.99))
#
# t:
qt(c(0.95,0.99),10)
#
# chi^2:
qchisq(c(0.95,0.99),20)




#### Problema 6 #### 
# Haga la gráfica de la densidad y la función de distribución t 
# con dos grados de libertad entre -4 y 4.
#
# Opción 1, fijamos 100 puntos como base:
x1 <- seq(-4,4,length=100)
densx <- dt(x1,2)
fdistx <- pt(x1,2)
plot(x1,densx,type='l', xlab = 'Valor de x', lwd=2,
     ylab='Prob', main='Densidad y Función de Distribución t',
     ylim = c(0,1), col='blue')
lines(x1,fdistx, col='red', lwd=2)
legend(x = "topleft", legend =c('Función de Densidad','Función de Distribución'), 
       lwd=c(2,2), col=c('blue','red'))
#
# Opción 2, se dibujan por separado cada curva con la función "curve":
# función de densidad:
curve(dt(x,2), from = -4, to = 4, 
     main = "Función de densidad t de student", 
     xlab = "x", ylab = "Probabilidad")
# función de distribución:
curve(pt(x,2), from = -4, to = 4,
      main = "Función de probabilidad t de student", 
      xlab = "x", ylab = "Probabilidad acumulada")




#### Problema 7 ####
# Calcule los valores de la siguiente función f en los pares (xi,yi) 
# con componentes tomadas a partir de los vectores que obtuvo en las 
# preguntas 3 y 4:
# f(x,y) = exp{x^y-log(x*y)}
#
f <- exp(x^y-log(x*y))
f




#### Problema 8 ####
# Defina una matriz de nombre AA de tamaño 4x4 con números seleccionados por
# usted.
#
AA <- matrix(c(7,3,4,2,11,4,5,16,6,8,21,15,1,0,2,7), nrow = 4)
AA




#### Problema 9 ####
# Halle la matriz inversa con dos decimales de precisión y verifique 
# que el producto de las matrices en ambos órdenes da la identidad.
#
AAinv <- solve(AA)
AAinv
round(AA%*%AAinv,0)




#### Problema 10 ####
# Resuelva el sistema de ecuaciones AAx = b, para b = (1, 2, 4, 3).
#
b <- matrix(c(1,2,4,3), nrow = 4)
x_sol <- AAinv %*% b
x_sol
#
# comprobamos que AA*x_sol = b
AA%*%x_sol




#### Problema 11 ####
# Obtenga los eigenvectores y eigenvalores de AA.
# 
eigen(AA)




#### Problema 12 ####
# Usando las funciones apply y sweep calcule la desviación típica 
# de las variables numéricas del arreglo iris3 separadas por especie 
# y luego obtenga un arreglo con cada dato dividido por el valor 
# de la desviación típica correspondiente.
#
# Analizamos los datos del arreglo iris3:
str(iris3)
dim(iris3)
# Se trata de una lista de 50 datos de 4 variables, para 3 distintas especies, 
# por lo que para separarse la desviación estándar por variable debe hacerse:
desv_tip <- apply(iris3, c(2,3), sd)
desv_tip
# y para dividir cada dato entre la desviación típica que le corresponde:
sweep(iris3, c(2,3), desv_tip,'/')




#### Problema 13 ####
# Calcule los valores de la siguiente función para un reticulado de valores 
# de las variables 'x' e 'y' de -1 a 1 en pasos de 0.1:
# f(x,y) = cos(y)/(1 + x^2)
#
# Se debe usar outer(), definiendo la función como se pide:
fxy <- function(x,y){
  cos(y)/(1 + x^2)
  }
x2 <- seq(-1,1,by=0.1)
y2 <- x2
retic <- outer(x2,y2,fxy)
retic




#### Problema 14 #### 
# Construya un cuadro de datos llamado notas con la siguiente información:
#           Examen1  Examen2  Tareas
# Antonio         7        9       8
# Berenice        6        6       7
# Carlos          8        8       9
# 
Examen1 <- c(7,6,8)
Examen2 <- c(9,6,8)
Tareas <- c(8,7,9)
notas <- data.frame(Examen1,Examen2,Tareas)
rownames(notas) <- c("Antonio","Berenice","Carlos")
#
# a) Use la función apply para obtener el promedio de todas las filas.
apply(notas,1,mean)
# 
# b) Agregue una nueva columna de nombre Def. que tenga los valores promedios.
notas$Def. <- round(apply(notas,1,mean),1)
#
# c) Agregue una nueva columna que indique si el estudiante aprobó (Ap.) o
#    reprobó (Rep.)
notas$Eval.Final <- ifelse(notas$Def.>= 6, "Ap.","Rep.")
notas




#### Problema 15 ####
# Definimos la siguiente lista:
# LL <- list(1:10, 11:22, 44:24,'string', T)
# ¿Cuál es el efecto de la siguiente instrucción?
# for (i in 1:4) { LL[[2]] <- NULL }
#
LL <- list(1:10, 11:22, 44:24,'string', T)
for (i in 1:4) { LL[[2]] <- NULL }
LL
#
# LL tiene la siguiente estructura:
# 
# [[1]]
# [1]  1  2  3  4  5  6  7  8  9 10
#
# [[2]]
# [1] 11 12 13 14 15 16 17 18 19 20 21 22
#
# [[3]]
# [1] 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24
#
# [[4]]
# [1] "string"
#
# [[5]]
# [1] TRUE
#
# La instrucción dentro del ciclo for, "LL[[2]] <- NULL" elimina el segundo
# elemento de la lista, [[2]].
# 
# Al eliminarse dicho elemento, los restantes se reenumeran, por lo que el
# elemento que ocupaba el lugar [[3]] pasa a ser el [[2]], y así sucesivamente.
#
# La instrucción repetida cuatro veces con la declaración "for (i in 1:4)", 
# hace que los elementos [[2]], [[3]], [[4]] y [[5]] sean borrados, 
# por lo que la lista resultante queda sólo con el elemento [[1]] original: 
#
# LL
# [[1]]
# [1]  1  2  3  4  5  6  7  8  9 10




#### Problema 16 ####
# Las siguientes instruciones tienen como objetivo borrar el segundo, 
# tercer y quinto elementos de la lista T
# ¿Qué hace realmente el programa? 
#
T <- list(1:11,'a','b','c','d','e')
T[[2]] <- NULL
T[[3]] <- NULL
T[[5]] <- NULL
T 
#
# Sucede lo mismo que en el problema anterior. Dado que las instrucciones se ejecutan
# en forma secuencial como está el código, la primera instrucción, "T[[2]] <- NULL", 
# elimina correctamente el segundo elemento de la lista. 
# 
# Sin embargo, la instrucción siguiente, "T[[3]] <- NULL", no toma en cuenta que 
# al eliminar [[2]], el elemento [[3]] se reenumera y se convierte en el elemento [[2]], 
# por lo que en realidad se termina borrando al elemento [[4]].
#
# La última instrucción no tiene ningún efecto, pues una vez que se han eliminado 
# dos de los elementos de la lista original, y se han reenumerado  los 
# que aún quedan, ya no existe un elemento [[5]] que borrar.
# 
# Para que la instrucción tenga el efecto deseado, debe ejecutarse simultáneamente:
# 
T <- list(1:11,'a','b','c','d','e')
T[[2]] <- T[[3]] <- T[[5]] <- NULL
T




#### Problema 17 ####
# Construya una lista que contenga el vector equipaje, el vector np, la matriz AA, y
# el cuadro de datos paises, conservando el mismo nombre para cada componente.
#
# El objeto "equipaje" se lee de la carpeta de trabajo del curso, y se fuerza a ser vector, 
# según el enunciado problema:
equipaje <- read.table("materiales/equipaje.txt")
equipaje <- as.vector(t(equipaje))
equipaje
#
# En caso de no disponer del archivo, correr siguente vector:
# equipaje <- c(6.6,6.0,2.6,9.3,7.8 10.4,8.4,8.0,6.0,7.8,4.9,3.8,6.4,0.7,4.7,0.0,8.4,7.1,9.1,7.3,5.8 10.0,4.0,9.5,4.0,
# 7.1,6.8,7.1,5.2,5.6,7.3,6.8,6.2,6.4 10.3,2.4,7.1,5.0,5.6,5.7)
#
# El objeto #paises# se construye como un data frame, según apuntes del CIMAT, de la siguiente forma:
#              GDP    Pob     Inflacion
# Austria      208      8         2.4
# Francia     1432     61         1.7
# Alemania    2112     82         2.0
# Suiza        259      7         1.6
#
GDP <- c(208,1432,2112,259)
Pob <- c(8,61,82,7)
Inflacion <- c(2.4,1.7,2.0,1.6)
paises <- data.frame(GDP,Pob,Inflacion)
rownames(paises) <- c("Austria","Francia","Alemania","Suiza")
paises
#
# La lista entonces queda como:
lista1 <- list(equipaje=equipaje,np=np,AA=AA,paises=paises)
lista1
#
# Ejecute las siguientes acciones sobre la lista:
#
# a) Elimine los tres últimos elementos de la componente equipajes
lista1[[1]] <- lista1[[1]][-seq(length(lista1[[1]]),length(lista1[[1]])-2)]
lista1
#
# b) Obtenga los elementos (2,3) y (4,2) de la matriz AA
lista1$AA[2,3]
lista1$AA[4,2]
#
# c) Obtenga los datos para Alemania y la Inflación para todos los paises.
lista1$paises[3,]
lista1$paises$Inflacion
#
# d) Usando la función apply o alguna de sus variantes, calcule la mediana de las
#    columnas de AA.
apply(lista1$AA,2,median)




#### Problema 18 ####
# Haga una gráfica del conjunto de datos women con líneas y puntos superpuestos.
# Coloque 'Altura' como etiqueta en el eje x y 'Peso' en el y. Como título ponga
# 'Valores promedio de altura y peso', subtitulo: 'Mujeres de 30 a 39 años'.
# 
plot(women, xlab = "Altura", ylab = "Peso", main = "Valores promedio de altura y peso",
     sub = 'Mujeres de 30 a 39 años', type = "o", cex.main=1.5, cex.sub=1.2, pch=16)




#### Problema 19 ####
# Repita el gráfico anterior con una línea cortada de color azul y grueso 2. 
# Modifique la escala del eje 'x' para que vaya de 55 a 75, y la del eje 'y' 
# para que vaya de 110 a 170.
#
plot(women, xlab = "Altura", ylab = "Peso", main = "Valores promedio de altura y peso",
     sub = 'Mujeres de 30 a 39 años',  type = "o", cex.main=1.5, cex.sub=1.2, pch = 19, lty = 2,
     lwd = 2, col = 'blue', xlim = c(55,75), ylim=c(110,170))




#### Problema 20 ####
# Haga una gráfica de la función cos(3x) de 0 a 3, de color azul. Superponga la 
# gráfica de sin(2x) de color rojo.
#
x3 <- seq(0,3,length=100)
fcos <- function(x){
  cos(3*x)
  }
fsen <- function(x){
  sin(2*x)
  }
#
plot(x3, fcos(x3), type = 'l', xlab = 'Valor de x', lwd=2,
     ylab='Valor de la función', main='Gráfica de funciones cos(3x) y sen(2x)',
     xlim = c(0,3), col='blue')
lines(x3,fsen(x3), col='red', lwd=2)
legend(x = "topright", legend =c('cos(3x)','sen(2x)'), 
       lwd=c(2,2), col=c('blue','red'))



