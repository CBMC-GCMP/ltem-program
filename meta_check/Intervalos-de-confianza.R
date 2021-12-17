# ************ Intervalos de confianza en R ************

# Define una variable n para poder cambiar fÃ¡cilmente el tamaÃ±o de la muestra
n = 1000

# Define una variable para el alfa
alpha=0.05

# Genera dos conjuntos de n muestras aleatorias 
# Ambas siguen una distribuciÃ³n normal de parÃ¡metros mu=20 y sigma=3
x=rnorm(n,20,3)
x2=rnorm(n,20,3)

# Genera dos conjuntos de n muestras aleatorias para los intervalos de proporciones
# Ambas siguen una distribuciÃ³n bernouilli con probabilidad 0.3
xb=rbinom(n,1,0.3)
xb2=rbinom(n,1,0.3)

# Muestra el histograma de la primera muestra
hist(x,probability=TRUE)

## Media de una poblaciÃ³n normal con varianza conocida

sigma=3
media = mean(x)
margenError = qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)*sigma/sqrt(length(x))
extrIzq= media - margenError
extrDer= media + margenError
print(c(extrIzq,extrDer))

#install.packages("TeachingDemos")
library(TeachingDemos)

z.test(x, mu=media, stdev=3, conf.level=1-alpha, alternative="two.sided")

## Media de una poblaciÃ³n normal con varianza desconocida

desviacion = sd(x)
media = mean(x)
n=length(x)
gl=n-1
margenError = qt(alpha/2, gl, lower.tail=FALSE)*desviacion/sqrt(length(x))
extrIzq= media - margenError
extrDer= media + margenError
print(c(extrIzq,extrDer))

t.test(x, conf.level = 0.95)
t.test(x, conf.level = 0.99)

## Varianza de una poblaciÃ³n normal

n=length(x)
desviacion=sd(x)
gl=n-1
chi_iz=qchisq(alpha/2, gl, lower.tail=FALSE)
chi_der=qchisq(1-(alpha/2), gl, lower.tail=FALSE)
extrIzq=((n-1)*desviacion^2)/chi_iz
extrDer=((n-1)*desviacion^2)/chi_der
print(c(extrIzq,extrDer))
izDtipica=sqrt(extrIzq)
derDtipica=sqrt(extrDer)
print(c(izDtipica,derDtipica))

sigma.test(x, conf.level=1-alpha, alternative="two.sided")

## Diferencia de medias en poblaciones normales - varianzas desconocidas pero iguales

desviacion1=sd(x)
desviacion2=sd(x2)
n1=length(x)
n2=length(x2)
varianzaComun=((n1-1)*desviacion1^2+(n2-1)*desviacion2^2)/(n1+n2-2)
desviacionComun=sqrt(varianzaComun)
gl=n1+n2-2
media1 = mean(x)
media2 = mean(x2)
margenError = qt(alpha/2, gl, lower.tail=FALSE)*desviacionComun*sqrt((1/n1)+(1/n2))
extrIzq= (media1-media2) - margenError
extrDer= (media1-media2) + margenError
print(c(extrIzq,extrDer))

t.test(x, x2, alternative='two.sided', conf.level=1-alpha, var.equal=TRUE)

## Diferencia de medias en poblaciones normales - varianzas desconocidas

desviacion1=sd(x)
desviacion2=sd(x2)
n1=length(x)
n2=length(x2)
A=desviacion1^2/n1
B=desviacion2^2/n2
delta=round(((n2-1)*A-(n1-1)*B)^2/((n2-1)*A^2+(n1-1)*B^2))
gl=n1+n2-2-delta
media1 = mean(x)
media2 = mean(x2)
margenError = qt(alpha/2, gl, lower.tail=FALSE)*sqrt((desviacion1^2/n1)+(desviacion2^2/n2))
extrIzq= (media1-media2) - margenError
extrDer= (media1-media2) + margenError
print(c(extrIzq,extrDer))

t.test(x, x2, alternative='two.sided', conf.level=1-alpha, var.equal=FALSE)

# Nota: la funciÃ³n t.test para varianzas desconocidas aproxima los grados de libertad utilizando la ecuaciÃ³n Welchâ€“Satterthwaite
# gl=(A+B)^2/((desviacion1^4/(n1^2*(n1-1)))+(desviacion2^4/(n2^2*(n2-1))))

## RazÃ³n de varianzas en poblaciones normales

desviacion1=sd(x)
desviacion2=sd(x2)
teta=desviacion1^2/desviacion2^2
n1=length(x)
n2=length(x2)
gl1=n2-1
gl2=n1-1
Fder=qf(alpha/2, gl1, gl2, lower.tail=FALSE)
Fiz=qf(1-(alpha/2), gl1, gl2, lower.tail=FALSE)
extrIzq=teta*Fiz
extrDer=teta*Fder
print(c(extrIzq,extrDer))

var.test(x, x2, alternative='two.sided', conf.level=1-alpha)

## Media en poblaciones no normales 

n=length(x)
desviacion=sd(x)
media=mean(x)
margenError = qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)*desviacion/sqrt(n)
extrIzq= media1 - margenError
extrDer= media1 + margenError
print(c(extrIzq,extrDer))

z.test(x, mu=media, stdev=desviacion, conf.level=1-alpha, alternative="two.sided")

## ProporciÃ³n

n=length(xb)
p=sum(xb)/n
q=1-p
margenError = qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)*sqrt((p*q)/n)
extrIzq= p - margenError
extrDer= p + margenError
print(c(extrIzq,extrDer))

prop.test(sum(xb), n, alternative='two.sided', conf.level=1-alpha, correct=FALSE)
# Nota: El intervalo proporcionado por prop.test es distinto al anterior 
# porque la funciÃ³n usa una aproximaciÃ³n que es mÃ¡s complicada de calcular pero mÃ¡s precisa

## Diferencia de medias en poblaciones no normales 

n1=length(x)
n2=length(x2)
desviacion1=sd(x)
desviacion2=sd(x2)
media1=mean(x)
media2=mean(x2)
margenError = qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)*sqrt((desviacion1^2/n1)+(desviacion2^2/n2))
extrIzq= (media1-media2) - margenError
extrDer= (media1-media2) + margenError
print(c(extrIzq,extrDer))

#install.packages("BSDA")
#library(BSDA)

#z.test(x, x2, sigma.x=desviacion1, sigma.y=desviacion2, conf.level=1-alpha, alternative="two.sided")

## Diferencia de proporciones

n1=length(xb)
n2=length(xb2)
p1=sum(xb)/n1
p2=sum(xb2)/n2
q1=1-p1
q2=1-p2
margenError = qnorm(alpha/2, mean=0, sd=1, lower.tail=FALSE)*sqrt((p1*q1/n1)+(p2*q2/n2))
extrIzq= (p1-p2) - margenError
extrDer= (p1-p2) + margenError
print(c(extrIzq,extrDer))

prop.test(c(sum(xb),sum(xb2)), c(n1,n2), alternative='two.sided', conf.level=1-alpha, correct=FALSE)