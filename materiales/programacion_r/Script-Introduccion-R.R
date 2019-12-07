#-----------------------------------------------------
#
#       Maestria MAEC INEGI
#       Propedeutico
#       Introduccion a la Programación
#       Nociones Basicas de R
#
#-----------------------------------------------------

#-----------------------------------------------------
# Librerías necesarias
library(MASS)
library(lattice)

#-----------------------------------------------------
#       LA CONSOLA DE R
#-----------------------------------------------------
# Manera clasica de consultar la ayuda:
help(sd)
?sd
help.search("solve")  # Archivos relacionados con "solve"
help()    # Ayuda para caracteres especiales o palabras reservadas

#Librerias y objetos disponibles
library()  # Muestra las librerias disponibles que pueden ser cargadas
search()   # Muestra las librerias ya cargadas y disponibles
ls()       # Objetos usados

# Se puede usar R como una simple calculadora
5 - 1 + 10  # Suma y resta
7 * 10 / 2  # multiplica y divida
pi      # el número pi
sqrt(2) # Raiz cuadrada 

# Crear objetos mediante asignaciones
x <- 5    # El objeto x toma el valor 5
x       # imprime x   
x = 6     # x ahora toma el valor 6
x
(x <- pi) # asigna el numero pi a x e imprime

# R es sensible a mayúsculas y minúsculas
# Dos objetos diferentes
b <- 2
B <- 4

# Comandos utiles
ls()  #muestra los objetos usados
rm(b) #borra objetos usados
ls()

# Saber si un nombre es una función de R
c

#-----------------------------------------------------
#       TIPOS DE OBJETOS EN R
#-----------------------------------------------------

#-----------------------------------------------------
# Vectores
x <- 0:19    # una secuencia de 20 números
x[5]       # muestra el elemento en la 5ta. posición
x <- seq(1, 10, length = 5)
seq(1, 20, by = 2) # Secuencia de dos en dos
# Vector de ceros.
numeric(6)
integer(3)
rep(2,6) # Vector de constantes

#vectores a traves de "c"
x <- c(1.3,2.5,3.6,4.7,8.2)
y <- c(x, 0, x) # Generamos un vector a partir de otro vector.

# Se pueden agregar nombres a los vectores.
z <- c(1, 3, 45)
names(z)
names(z) <- c("primero","segundo","tercero")
z

#vector logico
x <- 1:5
cond1 <- x < 4
cond1
cond2 <- x >= 3
cond1 & cond2 # Hacemos una ’y’ lógica
logical(3) # Vector de tres elementos lógicos

# Generacion de caracteres
(x <- character(3)) #vector de 3 entradas vacias
# Vector con las 4 primeras letras del alfabeto
x2 <- c("A","B","C","D")
# Otra forma
x3 <- LETTERS[1:4]

# Atributos de un objeto
is.logical(x)
is.numeric(x)
is.integer(x)

# Uso de NA, NaN e Inf
(x <- c(NA,0/0,Inf-Inf,Inf,5)) #diferencias entre NA, NaN e Inf

# Longitud de un vector
length(y) 

# Convertir vectores
h1 <- c(1.3,0.6,-3.8)
h1
h2 <- as.integer(h1) # convierte a enteros
h2 
as.logical(h2) # convierte en logico

# Operaciones con vectores
a <- 5:2
b <- (1:4) * 2
a
b

a + b   # Sumamos vectores
a - b # Diferencia de vectores
a * b # Producto de vectores (elemento a elemento)
a / b # Cociente de vectores (elemento a elemento)
a ^ b # Potencia de vectores (elemento a elemento)

# Producto de un escalar por un vector
2 * a
# Operación con más de dos vectores
d <- rep(2,4)
a + b + d

# Evaluacion de la funcion
#                 x^2 + 2y 
#  f(x,y) = log (-----------)
#                 (x + y)^2
# Definimos los vectores x e y
(x <- 10:6)
(y <- seq(1,9,2))
# Definimos f(x,y) en términos de estos vectores.
# Guardamos los resultados en z
(z <- log(x^2 + 2*y) / (x + y)^2)


#-----------------------------------------------------
# Matrices y arreglos
# definir la matriz
#  | 1 4 7 |
#  | 2 5 8 |
#  | 3 6 9 |
# USaremos
matriz <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)

# dimensiones de la matriz
dim(matriz)

# Elementos especficos de una matriz
matriz[2,3]
matriz[1:2,2:3] 
matriz[,c(1,3)]

# Multiplicacion de matrices
matriz %*% matriz  # Multiplicacion de matrices
matriz * matriz    # Multiplicacion elemento a elemento

# Dos ejemplos mas
matrix(1:6)
matrix(1:6, nrow = 3)

# Arreglos
x <- array (1:24, c(3,4,2))

array(1:4, 6) # Vector de tamaño 6
array(1:6, c(2,5)) # Matriz 2x5, se llena por columnas

# Matrices (Por defecto llena por columnas)
matrix(c(4,5,9,52,67,48), nrow = 2, ncol = 3)

# Para llenarla por filas, se le agrega byrow=TRUE
matrix(c(4,5,9,52,67,48), nrow = 2, ncol = 3, byrow = TRUE)

# Colocando vectores como columna
(y <- cbind(letters[1:4], LETTERS[1:4]))

#-----------------------------------------------------
#  Listas y factores
# Listas
lista <- list(Marca = "Chevrolet", Modelo = "Aveo", n.puertas = 5, Año = c(2006,2007))
lista
# Seleccionamos posiciones de la lista
lista[[1]] # Posición 1 de la lista
lista[[4]][2] # Posición 4 de la lista, subposición2

# También podemos referirnos a las posiciones por los nombres.
lista$Marca
lista$Modelo

# Factores
# Con la funcion gl()
edades <- gl(4,5,16,labels=c("Niños","Jovenes","Adulto","Anciano"))
edades
# Grafica del factor
plot(edades)

# Con la funcion factor()
sangre <- factor(rep(c("A","B","AB","O"),4,15))
sangre
# Grafica del factor
plot(sangre)

# Ordenar
escolaridad <- factor(rep(c("MedioSuperior","Primaria","Secundaria","Superior","Prescolar"),
                          5,15))
escolaridad
ordered(escolaridad,levels = c("Superior","MedioSuperior","Secundaria","Primaria","Prescolar"))

#-----------------------------------------------------
# Data.frame
# Creando una data.frame
dataf <- data.frame(Nombre = c("Juan","Maria","Jose","Carla"),
                    Edad = c(27,34,40,39),
                    Poblacion = c("Monterrey","Apodaca","Guadalupe", "San Pedro"),
                    Sexo = c("M","F","M","F"),
                    Edo.Civil = c("C","S","S","C"))
dataf
# attach y detach
attach(lista)
Marca
attach(dataf)
Edad

# Otro ejemplo
ojos <- factor(c("Azules","Marrones","Marrones"),
             levels = c("Azules","Marrones","Verdes","Negros"))
datos <- data.frame(Color.ojos = ojos, 
                    Peso = c(68,75,88),
                    Altura = c(1.65,1.79,1.85))
datos
# Convertir matriz en data.frame
datos2 <- as.data.frame(matriz)
# Nombre por defecto de las variables
names(datos2)
# Cambiando los nombres de las varaibles
names(datos2) <- c("Variable 1","Variable 2","Variable 3")
# Uso del operador $
datos$Color.ojos

#-----------------------------------------------------
#     LECTURA/ESCRITURA DE DATOS
#-----------------------------------------------------

#-----------------------------------------------------
# Leer datos de un archivo
# Uso de la funcion read.table()
PreciosCasas <- read.table("datos.casas.txt") 
PreciosCasas
# Otro ejemplo de uso de read.table()
MEXpob <- read.table("MEXpob.txt", header = TRUE)
MEXpob
# Grafico de los datos
plot(Pob. ~ Año, data = MEXpob, pch = 16)
# La funcion scan()
misdatos <- scan("entrada.txt", list("",0,0)) #uso de la funcion scan
misdatos
# La función read.fwf()
misdatos2 <- read.fwf("datos.txt", widths=c(1, 4, 3))
misdatos2

#-----------------------------------------------------
# Guardar datos
# La función write.table()
write.table(PreciosCasas, "./Datos/PreciosCasas2.txt")
write.table(MEXpob, "./Datos/MEXpob2.txt")
# La función write()
# guardando un vector
x <- c(1,2,3,4,5)
write(x, "./Datos/x.txt")
# guardando una matriz
x <- matrix(1:9, ncol = 3, byrow = T)
x
write(t(x), "./Datos/xx.txt", ncol = ncol(x))

#-----------------------------------------------------
#     COMO PROGRAMAR EN R
#-----------------------------------------------------
# Condicional if

# Un ejemplo de if
z <- 1:10
z1 <- sample(z,1)
if (z1 > 7) {w = 1} else {w = 0}
w

# Un ejemplo con condición lógica
x <- TRUE
if(x) y <- 1 else y <- 0
y

# Dos asignaciones a ejecutar
x <- FALSE
if(!x){y <- 1 ; z <- 2}
y
z
# Dos condiciones a verificar
# Raíz n-ésima de un número real
n <- 7; x <- -32
if(n%%2 == 1 || x >=0 ){
  sign(x)*abs(x)^(1/n)
  } else{
  NaN
}

# Ciclos 
# Usando un for y creando vectores logicos
x1 <- as.logical(as.integer(runif(5, 0, 2)))
x1
y1 <- vector() 
y1
for(i in 1 : length(x1)){
  if(x1[i]){y1[i] <- 1}
  else {y1[i] <- 0}
  }
y1

# Un ejemplo de repeat y break
x2 <- 1:10; x3 <- 10:20; i <- 1
repeat{
  y <- x2[i] + x3[i]
  i <- i+1
  if(i == 10) break
}
y

# Funciones
# Definimos la función cubo
cubo <- function(x)
  {return(x^3)}
# Ejecutamos la función cubo con x=2
cubo(3)

# Definimos la funcion ff
ff <- function (x = 1:10, y = (1:10)^2, showgraph = T)
{
  if (showgraph) plot (x,y)
  else print (cbind (x,y))
  return (invisible ())
}
# Ejecuciones de la funcion ff
ff (1:10, (1:10)^2, T)
ff (1:10, (1:10)^2)
ff (1:20, (1:20)^2)

#-----------------------------------------------------
#     GRAFICOS EN R
#-----------------------------------------------------
# Demos
demo(graphics)
demo(persp)
demo(image)

#-----------------------------------------------------
# Distribucion Weibull
tt <- seq(0,2,length=200)
par(mfrow=c(2,2),mar=c(3, 3, 2, 2), oma=c(0,0,2,0))
# Primera funcion
b1 <- .5; t1 <- 1/gamma(1+1/b1)
plot(tt, exp(-(tt/t1)^b1), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main= expression(paste(beta == .5, ", ", theta == .5)))
# Segunda funcion 
b2 <- 1.5; t2 <- 1/gamma(1+1/b2)
plot(tt, exp(-(tt/t2)^b2), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main= expression(paste(beta == 1.5, ", ", theta == 1.108)))
# Tercera funcion
b3 <- 2.5; t3 <- 1/gamma(1+1/b3)
plot(tt, exp(-(tt/t3)^b3), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main= expression(paste(beta == 2.5, ", ", theta == 1.127)))
# Cuarta funcion
b4 <- 5; t4 <- 1/gamma(1+1/b4)
plot(tt, exp(-(tt/t4)^b4), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main=
       substitute(paste(beta == 5, ", ", theta, " = ",t4),list(t4=round(t4,3))))
mtext("Funciones de Supervivencia", outer=TRUE, cex=1.2)
# (en esta grafica, notar en la ultima, el uso de substitute)

#-----------------------------------------------------
# Uso de la funcion layout():
layout( matrix(c(1,1,2,3), ncol = 2, byrow = T), heights = c(2,1))
par(mar=c(3, 3, 2, 2))
b2 <- 1.5; t2 <- 1/gamma(1+1/b4)
# Funcion de densidad
plot(tt, dweibull(tt,shape=b2, scale=t2), xlim=c(0,2), cex.axis=.7, cex.lab=1.2,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="Funcion de Densidad",
     ylab="", ylim=c(0,.8),main= expression(paste(beta == 1.5, ", ", theta == 1.108)))
# Funcion de supervivencia
plot(tt, exp(-(tt/t2)^b2), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main= "Funcion de Supervivencia")
# Funcion de distribucion
plot(tt, 1-exp(-(tt/t2)^b2), xlim=c(0,2), cex.axis=.7, cex.lab=.7,
     mgp=c(1.5,.5,0), lwd=2, col="blue", type="l", xlab="t", ylab="",
     main= "Funcion de Distribucion")

#-----------------------------------------------------
# Caminata aleatoria
par(mfrow=c(1,1))
n <- 100
xx <- 1:n
cam <- cumsum(sample(c(-1,1), size = n, replace = T))
camina <- function(k){
  plot(1:k, cam[1:k], xlim = c(1,n), ylim = c(-n/10,n/10), type = "l",
       col = "blue", lwd = 2, mgp = c(2,1,0), ylab = "Caminata",
       xlab = "", cex.axis = .8)
  abline(h = 0, col = gray(.8))
  Sys.sleep(0.1) } # Sys.sleep() controla la rapidez de la animacion
trash <- sapply(xx,camina)

#-----------------------------------------------------
# Los siguientes graficos fueron tomados del demo(graphics)
#-----------------------------------------------------
#-----------------------------------------------------
# Una grafica simple, ilustrando el uso de colores en sus distintos elementos.
opar <- par(bg = "white") # guardar parametros
x <- rnorm(50)
plot(x, ann = FALSE, type = "n")
abline(h = 0, col = gray(.90))
lines(x, col = "green4", lty = "dotted")
points(x, bg = "limegreen", pch = 21)
title(main = "Ejemplo simple de uso de color en Plot",
      xlab = "Informacion con un color desvanecido",
      col.main = "blue", col.lab = gray(.7),
      cex.main = 1.2, cex.lab = 1.0, font.main = 4, font.lab = 3)

#-----------------------------------------------------
# Diagrama de pastel.
#x11()
par(bg = "gray")
pie(rep(1,24), col = rainbow(24), radius = 0.9)
title(main = "Una muestra del catalogo de colores",
      cex.main = 1.4, font.main = 3)
title(xlab = "(Use esto como una prueba de la linealidad del monitor)(?)",
      cex.lab = 0.8, font.lab = 3)

#-----------------------------------------------------
# Diagrama de pastel (de nuevo).
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", 
                      "Other", "Vanilla Cream")
pie(pie.sales,
    col = c("purple","violetred1","green3","cornsilk","cyan","white"))
title(main = "Ventas de Pasteles en Enero", cex.main = 1.8, font.main = 1)
title(xlab = "Pasteleria Lety", cex.lab = 1.2, font.lab = 3)

#-----------------------------------------------------
# Boxplots.
par(bg="cornsilk")
n <- 10
g <- gl(n, 100, n*100)
x <- rnorm(n*100) + sqrt(as.numeric(g))
boxplot(split(x,g), col = "lavender", notch = TRUE)
title(main = "Boxplots con intervalos de confianza para las medianas",
      xlab = "Grupo", font.main = 4, font.lab = 1, cex.main = .9)

#-----------------------------------------------------
# Area sombreada entre dos graficas.
par(bg="white")
n <- 100
x <- c(0,cumsum(rnorm(n)))
y <- c(0,cumsum(rnorm(n)))
xx <- c(0:n, n:0)
yy <- c(x, rev(y))
plot(xx, yy, type = "n", xlab = "Tiempo", ylab = "Distancia")
polygon(xx, yy, col = "gray")
title("Distancia entre dos movimientos Brownianos")

#-----------------------------------------------------
# Graficas tipo Excel, o algo parecido.
x <- c(0.00, 0.40, 0.86, 0.85, 0.69, 0.48, 0.54, 1.09, 1.11, 1.73, 2.05, 2.02)
par(bg = "lightgray")
plot(x, type = "n", axes = FALSE, ann = FALSE)
usr <- par("usr") # c(x1,x2,y1,y2) con coordenadas de region de graficacion
rect(usr[1], usr[3], usr[2], usr[4], col = "cornsilk", border = "black")
lines(x, col = "blue")
points(x, pch = 21, bg = "lightcyan", cex = 1.25)
axis(2, col.axis = "blue", las = 1)
axis(1, at = 1:12, lab = month.abb, col.axis = "blue")
title(main = "Nivel de interes en R", font.main = 4, col.main = "red")
title(xlab = "1996", col.lab = "red")

#-----------------------------------------------------
# Histograma.
par(bg = "cornsilk")
x <- rnorm(1000)
hist(x, xlim = range(-4, 4, x), col = "lavender", main = "", ylab = "Frecuencia")
title(main = "1000 realizaciones simuladas de una variable normal", font.main = 3)

#-----------------------------------------------------
# Grafica por parejas.
pairs(iris[1:4], main = "Datos de Iris de Edgar Anderson", font.main = 4, pch = 19)

#-----------------------------------------------------
# Grafica por parejas (colores diferentes para cada especie).
aa <- iris
names(aa) <- c("Long.Sepalo","Ancho.Sepalo",
               "Long.Petalo","Ancho.Petalo","Especie")
pairs(aa[1:4], main = "Datos de Iris de Edgar Anderson", pch = 21,
      bg = c("red", "green3", "blue")[unclass(iris$Species)])

#-----------------------------------------------------
# Grafica de contornos.
# volcano es la matriz 87 x 61 de elevaciones del volcan Maunga Whau en NZ
x11()
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
lev <- pretty(range(volcano), 10)
par(bg = "lightcyan")
pin <- par("pin")
xdelta <- diff(range(x))
ydelta <- diff(range(y))
xscale <- pin[1]/xdelta
yscale <- pin[2]/ydelta
scale <- min(xscale, yscale)
xadd <- 0.5*(pin[1]/scale - xdelta)
yadd <- 0.5*(pin[2]/scale - ydelta)

plot(numeric(0), numeric(0),
     xlim = range(x)+c(-1,1)*xadd, ylim = range(y)+c(-1,1)*yadd,
     type = "n", ann = FALSE)
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col = "green3")
contour(x, y, volcano, levels = lev, col = "yellow", lty = "solid", add = TRUE)
title("Mapa Topografico del Maunga Whau, NZ", font = 4)
title(xlab = "Direccion Norte (metros)", ylab = "Direccion Oeste (metros)",
      font = 3)
mtext("Curvas de nivel cada 10 Metros", side = 3, line = 0.35, outer = FALSE,
      at = mean(par("usr")[1:2]), cex = 0.7, font = 3)

#-----------------------------------------------------
# Graficas condicionales.
# El conjunto de datos quakes es un data frame con 1000 observaciones
# en 5 variables:
# lat = Latitud del evento
# long = Longitud
# depth = Profundidad (km)
# mag = Magnitud en escala de Richter
# stations = Numero de estaciones reportando el evento
par(bg = "cornsilk")
coplot(lat ~ long | depth, data = quakes, pch = 21, bg = "green3")

#-----------------------------------------------------
#   Algunas figuras interesantes
#-----------------------------------------------------
# Espiral de Ulam
# prim = Un programa para calcular los primeros n primos
prim <- function(n){
  if(n==1){return(2)}
  primos <- 2
  notyet <- TRUE
  probar <- 2
  while(notyet){
    probar <- probar + 1
    pritst <- primos[ primos<=sqrt(probar) ]
    aa <- (probar %% pritst)
    if( any(aa==0) ){next}
    primos <- c(primos,probar)
    if( length(primos)==floor(n) ){ return(primos) }
  }
}
# Variables 
m <- 100
pp <- prim( (m+1)^2 )
ii <- seq(3,m+1,by=2)
jj <- length(ii)
# Grafico
par(mar=c(0,0,0,0)+1); xylim <- c(1,m+1)
plot(1, 1, xlim = xylim, ylim = xylim, type = "n", xaxt = "n", yaxt = "n",
     bty = "n", xlab = "", ylab = "")
aa <- c(floor(m/2)+1,floor(m/2)+1)
for(k in 1:jj){
  r <- ii[k]
  co <- cbind(c(rep(r,r),(r-1):2,rep(1,r),2:(r-1)),c(r:1,rep(1,r-2),1:r,rep(r,r-2)))
  co <- co + (jj-k)
  n <- dim(co)[1]
  uu <- (r^2):((r-2)^2)
  rr <- is.element(uu[-(n+1)],pp)
  bb <- co[n,]
  segments(aa[1], aa[2], bb[1], bb[2], col = "black", lwd = 1)
  aa <- co[1,]
  for(i in 1:(n-1)){ 
    segments(co[i,1], co[i,2], co[i+1,1], co[i+1,2], col = "black", lwd = 1)
    }
  points(co[rr,1], co[rr,2], col = "blue", pch = 20) 
  }
title("Espiral de Ulam", cex = .9, line = -.3)

#-----------------------------------------------------
# Laberinto circular
M <- 40; m <- 120; n <- M; xylim <- .95*c(-M,M)
par(mar = c(0,0,0,0)+.6)
plot(0, 0, type = "n", xlim = xylim, ylim = xylim, xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", bty = "n")
pp <- c(0,0)
tet1 <- runif(1, min = 0, max = 2*pi)
for( r in 1:n ){
  qq <- r*c(cos(tet1),sin(tet1))
  segments(pp[1],pp[2],qq[1],qq[2], col = "blue", lwd = 2)
  tet2 <- tet1 + runif(1, min = 0, max = 2*pi)
  ts <- seq(tet1, tet2, length = 200)
  nc <- r*cbind( cos(ts), sin(ts) )
  lines( nc[,1], nc[,2], col = "red", lwd = 2 )
  tet1 <- tet2
  pp <- nc[200,] 
} 

#-----------------------------------------------------
# El copo de nieve de Koch 

# En este ejemplo se tiene anidadas varias funciones
KochSnowflakeExample <- function(){ # Funcion general
  iterate <- function(T,i){ # Primera funcion anidada
    A = T[ ,1]; B=T[ ,2]; C = T[,3];
    if (i == 1){
      d = (A + B)/2; h = (C-d); d = d-(1/3)*h;
      e = (2/3)*B + (1/3)*A; f = (1/3)*B + (2/3)*A;
    }
    if (i == 2){
      d = B; e = (2/3)*B + (1/3)*C; f = (2/3)*B + (1/3)*A;
    }
    if (i == 3){
      d = (B + C)/2; h = (A-d); d = d-(1/3)*h;
      e = (2/3)*C + (1/3)*B; f = (1/3)*C + (2/3)*B;
    }
    if (i == 4){
      d = C; e = (2/3)*C + (1/3)*A; f = (2/3)*C + (1/3)*B;
    }
    if (i == 5){
      d = (A + C)/2; h = (B-d); d = d-(1/3)*h;
      e = (2/3)*A + (1/3)*C; f = (1/3)*A + (2/3)*C;
    }
    if (i == 6){
      d = A; e = (2/3)*A + (1/3)*C; f = (2/3)*A + (1/3)*B;
    }
    if (i == 0){
      d = A; e = B; f = C;
    }
        Tnew = cbind(d,e,f)
    return(Tnew); # Devuelve un triángulo más pequeño.
  }
  # Segunda funcion anidada
  draw <- function(T, col=rgb(0.5,0.2,0),border=rgb(0.5,0.2,0)){
    polygon(T[1,], T[2,], col = col, border = border)
  }
  # Tercera funcion anidada
  Iterate = function(T,v,col=rgb(0.5,0.2,0),border=rgb(0.5,0.2,0)){
    for (i in v) T = iterate(T,i);
    draw(T, col = col, border = border);
  }
  
  # Los vértices del triángulo inicial:
  A = matrix(c(1,0),2,1);
  B = matrix(c(cos(2*pi/3), sin(2*pi/3)),2,1);
  C = matrix(c(cos(2*pi/3),-sin(2*pi/3)),2,1);
  T0 = cbind(A,B,C);
  
  plot(numeric(0), xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), axes = FALSE,
       frame = FALSE, ann = FALSE);
  par(mar = c(0,0,0,0), bg = rgb(1,1,1));
  par(usr = c(-1.1,1.1,-1.1,1.1));
  
  # Dibujar copo de nieve:
  for (i in 0:6) for (j in 0:6) for (k in 0:6) for (l in 0:6) Iterate(T0,c(i,j,k,l));
}
# Ejecucion de la funcion
KochSnowflakeExample()

#-----------------------------------------------------
# Final del script


