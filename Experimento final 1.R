require(gplots)
require(graphics)
require(lmtest)
require(agricolae)
require(pwr)

#Exploración gráfica inicial de los datos.
Datos$Dispositivos <- as.factor(Datos$Dispositivos)
Datos$`Tiempo descarga`<-as.numeric(Datos$`Tiempo descarga`)

tapply(Datos$`Tiempo descarga`, Datos$Dispositivos, summary)
#graficas de efectos principales
plot.design(`Tiempo descarga` ~  Dispositivos,data =Datos, col="blue", xlab="Efecto", ylab="Tiempo promedio descarga")

#Se ven los comportamientos de las variables 
 

par(mfrow=c(1,2))
boxplot(`Tiempo descarga` ~ Dispositivos, data = Datos)
plotmeans(`Tiempo descarga` ~ Dispositivos, data = Datos)
#En la grafica se observa que el dispositivo Xiaomi y el Iphone tiene mayor eficiencia en
#la descarga de diferentes aplicaciones


m1 <- lm(`Tiempo descarga` ~ Dispositivos, data = Datos)
summary(m1)



##Analisis de medias descriptivas

#modelo de efectos: 𝑦_𝑖𝑗=𝜇+𝜏_𝑖+𝜀_𝑖𝑗, 𝑖=1,2,…,𝑎 y 𝑗=1,2,…,𝑛

#modelo de medias: 𝑦_𝑖𝑗=𝜇_𝑖+𝜀_𝑖𝑗, 𝑖=1,2,…,𝑎 y 𝑗=1,2,…,𝑛

#a es el nuemero de niveles del experimento siendo un factor controlable, en
#este caso es igual a 3.
#n es el numero de muestras o replicas que tiene el experimento, en 
#este caso n tiene un valor de 3
#Yij es el tiempo que se demora en descargar una app en el
#dispositivo # i en la replica # j
# mu es el tiempo promedio de descarga  en el dispositivo i
#e es nuestro error aleatorio

#modelo de efectos: 𝑦_𝑖𝑗=𝜇+𝜏_𝑖+𝜀_𝑖𝑗, 𝑖=1,2,…,𝑎 y 𝑗=1,2,…,𝑛

#mu es el tiempo de descarga promedio de las app en las 12 observaciones o mediciones
#de manera general
#tau sub i es el efecto del dispositivo i
modelo <- aov(`Tiempo descarga` ~ Dispositivos, data = Datos)
summary(modelo)




#Estimacion de parametros 
mu<-mean(Datos$`Tiempo descarga`)
mu

#Tau
t1 <- 31.53 - mu
t2 <- 136.35 - mu
t3 <- 20.57 - mu
#Hay 2 efectos negativos en los tiempos de descarga en el dispositivo 1 y 3 
#(Iphone y xiaomi), lo que quiere decir que los datos estan sobreestimados 


residuales <- rstandard(modelo)
residuales
hist(residuales)

shapiro.test(residuales) # normalidad, los datos son normales
bptest(modelo)#homocedasticidad, no son homocedasticos
bgtest(modelo)# independencia, los datos son independientes.

#Comparaciones por pares
LSD.test(modelo, "Dispositivos" , console = T, group = FALSE)
HSD.test(modelo, "Dispositivos", group = FALSE, console = T)
plot(TukeyHSD(modelo, "Dispositivos"))

pwr.anova.test(f=sqrt(0.53), k=5, power= 0.90, sig.level= 0.05)

# Se debieron de usar 5 replicas para que la potencian alcanzara el 90%

