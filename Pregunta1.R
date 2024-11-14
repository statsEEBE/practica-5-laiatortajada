#exercici 1 -> distribució normal

#distribució poblacional normal
mu<-95.7
sigma<-5.7
#N-> N(mu,sigma^2)
curve(dnorm(x, mean=mu, sd=sigma),xlim=c(80,120), col='red')

sum(rnorm(4,mu,sigma))

Y<- function (i){sum(rnorm(4,mu,sigma))} #passar-ho  a variable aleatòria
mean(sapply(1:5,Y)) #fer un bucle 5 vegades

Y1000000<- sapply (1:100000,Y)
mean(Y1000000)
hist(Y1000000, freq=FALSE) 
curve(dnorm(x,4*mu,2*sigma), col='red', add=TRUE)

#per fer-ho infinit vegdades apliquem teorema fet a teoria
#a)
E<- 4*mu

#b)
Var<- 100*sigma^2

#c)
1-pnorm(103,mu,sigma)

#d)
pnorm(98, mu, sigma/sqrt(4))

#e)
#var>32?
Ssq<- function (i){var(rnorm(100,mu,sigma))} 
Ssq100000<- sapply(1:10000,Ssq)
mean(Ssq100000>32)

w<- 32*(100-1)/sigma^2
1-pchisq(w,99)
1-pchisq(99*32/sigma^2,df=99)





