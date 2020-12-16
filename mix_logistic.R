setwd("~/Documentos/projeto_bayesiana")
load("~/Documentos/projeto_bayesiana/env.RData")
library(rstan)

#------------- Some useful functions ----------------------------------
rDiscreta<-function(p){
  u<-runif(1)
  P<-cumsum(p)
  val<-sum(P<u)+1
  val
}
logit_inversa <- function(x) { 1/(1+exp(-x)) }

#---------------Fitting the Model-----------------------------------------

dados<-read.csv("dados.csv", header = TRUE)
n<-length(dados$y)  
N<-50
k=2
stan_data <- list(k=k, n=n, x=dados$confusum, y=dados$y, N=N)
fit <- stan("mix_logistic.stan", iter=70000, warmup = 10000, thin = 100, chains=2, data=stan_data, control = list(max_treedepth = 20))
summary(fit33)


#---------------Traceplots and Posterior Density--------------------------------

traceplot(fit33, pars = "w" )
traceplot(fit33, pars = c("beta_0[1]","beta_1[1]") )
traceplot(fit33, pars = c("beta_0[2]","beta_1[2]") )+ scale_y_continuous(limits = c(-1,1))


stan_dens(fit33, pars = c("w[1]","w[2]"),separate_chains = T)
stan_dens(fit33, pars = c("beta_0[1]","beta_1[1]"),separate_chains = T)
stan_dens(fit33, pars = c("beta_0[2]","beta_1[2]"),separate_chains = T) + scale_x_continuous(limits = c(0,1))


#------------------------Intervals --------------------------------------------

plot(fit33, pars = c("w[1]","w[2]")) + scale_x_continuous(limits = c(0,1))
plot(fit33, pars = c("beta_0[1]","beta_1[1]")) 
plot(fit33, pars = c("beta_0[2]","beta_1[2]")) + scale_x_continuous(limits = c(-1,0.5))


#----------Posterior Predictive Check-------------------------------------------


pdf(file = "preditiva_12345",width = 10, height = 6)
par(mfrow = c(2,3))

plot(density(dados$y), xlab = "", col = "chocolate", main = "y")
x<-extract(fit33)$y_rep[,1]
plot(density(x), xlab = "", main = "y_rep[1]")
x<-extract(fit33)$y_rep[,2]
plot(density(x), xlab = "", main = "y_rep[2]")
x<-extract(fit33)$y_rep[,3]
plot(density(x), xlab = "", main = "y_rep[3]")
x<-extract(fit33)$y_rep[,4]
plot(density(x), xlab = "", main = "y_rep[4]")
x<-extract(fit33)$y_rep[,5]
plot(density(x),xlab = "",  main = "y_rep[5]")
mtext("Number of remaining eggs", line = -45, side = 3, outer = T)
dev.off()


pdf(file = "preditiva_5678910",width = 10, height = 6)
par(mfrow = c(2,3))

plot(density(dados$y), xlab = "", col = "chocolate", main = "y")
x<-extract(fit33)$y_rep[,6]
plot(density(x), xlab = "", main = "y_rep[6]")
x<-extract(fit33)$y_rep[,7]
plot(density(x), xlab = "", main = "y_rep[7]")
x<-extract(fit33)$y_rep[,8]
plot(density(x), xlab = "", main = "y_rep[8]")
x<-extract(fit33)$y_rep[,9]
plot(density(x), xlab = "", main = "y_rep[9]")
x<-extract(fit33)$y_rep[,10]
plot(density(x),xlab = "",  main = "y_rep[10]")
mtext("Number of remaining eggs", line = -45, side = 3, outer = T)
dev.off()


#----------- Classifying the data ----------------------------------------------

w<-c(mean(extract(fit33)$w[,1]), mean(extract(fit33)$w[,2]))
beta_0<-c(mean(extract(fit33)$beta_0[,1]),mean(extract(fit33)$beta_0[,2]))
beta_1<-c(mean(extract(fit33)$beta_1[,1]),mean(extract(fit33)$beta_1[,2]))

s<-matrix(0,nrow = n,ncol = k)
u<-matrix(0,nrow = n,ncol = k)

for (i in 1:n) {
  for (p in 1:k) {
    s[i,p] <- w[p]*dbinom(dados$y[i], N, logit_inversa(beta_0[p]+beta_1[p]*dados$confusum[i]), log = FALSE)
  }
}
u <- s/rowSums(s)
slatent <- function(a) rDiscreta(u[a,])
d <- 1:n
s_latente <-unlist(lapply(d,slatent))



#------------Ploting the classified dada ---------------------------------------

dados$latente <- s_latente
palette(c("blueviolet", "chocolate"))

plot(dados$confusum, dados$y,col=as.factor(dados$latente),pch=19, ylab = "y", xlab= "covariate:confusum", main =" Classified Data")
legend(0.4, 30,legend=c("Component 1", "Component 2"),col=c("blueviolet", "chocolate"),pch=19, cex=0.8)

plot(dados$y,col=as.factor(dados$latente),pch=19, ylab = "y", xlab= "index", main =" Classified Data")
legend(9, 32,legend=c("Component 1", "Component 2"),col=c("blueviolet", "chocolate"),pch=19, cex=0.8)

#conclusão : vemos que y>25 pertence a componente 1 com covariável 0.



p=1
Y1=NULL
y1 <- function(i) rbinom(27,50,logit_inversa( beta_0[p]+beta_1[p]*dados$confusum))
for (i in 1:1000) {Y1=c(Y1,y1(i)) }
plot(density(Y1))

p=2
Y2=NULL
y2 <- function(i) rbinom(27,50,logit_inversa( beta_0[p]+beta_1[p]*dados$confusum))
for (i in 1:1000) {Y2=c(Y2,y2(i)) }
plot(density(Y2), col = "red")

hist(Y2, col = "red",xlim = c(0,50),ylim = c(0,5000))
hist(Y1, add = T)

l =function (i) sample(c(1,2),1,prob=c(0.46,0.54),replace = TRUE)
Y=NULL
lab=NULL
for (i in 1:10000) {
  lab=c(lab,l(i))
  if(l(i)==1){
    Y=c(Y,sample(Y1,1))
  }else{
    Y=c(Y,sample(Y2,1))
  }
}
