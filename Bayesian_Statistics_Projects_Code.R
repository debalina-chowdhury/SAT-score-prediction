library("coda")
library("rjags")
?dexp
getwd()
setwd("D:\\Bayesian_statistics_codes")
data = read.csv("insurance.csv")
head(data)
data$Smoker = as.numeric(data$smoker == "yes")
data$Age_Smoker = data$age*data$Smoker
data$Bmi_Smoker = data$bmi*data$Smoker
data$Region = as.numeric(as.factor(data$region))
dev.new()
boxplot(data$charges)
data1<- subset(data, charges <= 35000)
data2 <- subset(data, charges <= 10000)
dim(data1)
head(data)
pairs(data[3:4])
plot(charges ~ age, data = data)
dev.new()
boxplot(charges ~ sex, data = data)
boxplot(charges ~ children, data = data)
boxplot(charges ~ smoker, data = data)
boxplot(charges ~ region, data = data)
table(data$region)
hist(data$charges)
curve(dexp(x, rate=5))

hist(data$bmi)
hist(data$age)
boxplot(charges ~ region, data = data)
summary(data)
mean(data$charges) #13270.42
mean(data$bmi) #30.6634
mean(data$age) #39.20703
mean(data$children) #1.094918
curve(dexp(data$charges, 1))

curve( 0.4*dexp(x, 1.0))

plot(charges ~ bmi, data = data)



mod_string = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(1.0e4, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:2) {
        b[k] ~ dnorm(1.0e4, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params = c("a0", "alpha", "b", "sigma", "tau")        

mod = jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1e3)
mod_sim = coda.samples(model = mod, variable.names = params, n.iter = 5e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim))
#dim(mod_csim)

dic = dic.samples(mod,1e3)



mod_string2 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Sex[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(1.0e4, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:3) {
        b[k] ~ dnorm(1.0e4, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags2 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                 Bmi = data$bmi,
                 Children = data$children,
                 Smoker = as.numeric(data$smoker=="yes"), 
                 Region = as.numeric(as.factor(data$region)),
                 y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params2 = c("a0", "alpha", "b", "sigma", "tau")        

mod2 = jags.model(textConnection(mod_string2), data = data_jags2, n.chains = 3)
update(mod2, 1e3)
mod_sim2 = coda.samples(model = mod2, variable.names = params2, n.iter = 5e3)

mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))
#dim(mod_csim)

dic2 = dic.samples(mod2,1e3)



mod_string3 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Age[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(1.0e4, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:3) {
        b[k] ~ dnorm(1.0e4, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags3 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params3 = c("a0", "alpha", "b", "sigma", "tau")        

mod3 = jags.model(textConnection(mod_string3), data = data_jags3, n.chains = 3)
update(mod3, 1e3)
mod_sim3 = coda.samples(model = mod3, variable.names = params3, n.iter = 5e3)

mod_csim3 = as.mcmc(do.call(rbind, mod_sim3))
#dim(mod_csim)

gelman.diag(mod_sim3)
autocorr.diag(mod_sim3)
autocorr.plot(mod_csim3)


dic3 = dic.samples(mod3,1e3)
dic
#Mean deviance:  27549 
#penalty 6.878 
#Penalized deviance: 27555

dic2
#Mean deviance:  27564 
#penalty 7.927 
#Penalized deviance: 27572 

dic3
#Mean deviance:  27140 
#penalty 7.767 
#Penalized deviance: 27148 


#Mean deviance:  27131 
#penalty 8.726 
#Penalized deviance: 27139

summary(mod_sim4)



mod_string5 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Age[i] + b[4]*Children[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:4) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags5 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params5 = c("a0", "alpha", "b", "sigma", "tau")        

mod5 = jags.model(textConnection(mod_string5), data = data_jags5, n.chains = 3)
update(mod5, 1e3)
mod_sim5 = coda.samples(model = mod5, variable.names = params5, n.iter = 5e3)

mod_csim5 = as.mcmc(do.call(rbind, mod_sim5))
#dim(mod_csim)

dic5 = dic.samples(mod5, 1e3)
dic5
#Mean deviance:  27131 
#penalty 8.726 
#Penalized deviance: 27139
summary(mod_sim5)

dev.new
autocorr.plot(mod_sim5)


table(data$region)



mod_string6 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Age[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:3) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags6 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params6 = c("a0", "alpha", "b", "sigma", "tau")        

mod6 = jags.model(textConnection(mod_string6), data = data_jags6, n.chains = 3)
update(mod6, 1e3)
mod_sim6 = coda.samples(model = mod6, variable.names = params6, n.iter = 5e3)

mod_csim6 = as.mcmc(do.call(rbind, mod_sim6))
#dim(mod_csim)

dic6 = dic.samples(mod6, 1e3)
dic6
#Mean deviance:  27199 
#penalty 13.08 
#Penalized deviance: 27212
dic5
#Mean deviance:  27203 
#penalty 23.9 
#Penalized deviance: 27227

summary(mod_sim6)



mod_string7 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] 
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:2) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags7 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params7 = c("a0", "alpha", "b", "sigma", "tau")        

mod7 = jags.model(textConnection(mod_string7), data = data_jags7, n.chains = 3)
update(mod7, 1e3)
mod_sim7 = coda.samples(model = mod7, variable.names = params7, n.iter = 5e3)

mod_csim7 = as.mcmc(do.call(rbind, mod_sim7))
#dim(mod_csim)

dic7 = dic.samples(mod7, 1e3)
dic6
#Mean deviance:  27199 
#penalty 13.08 
#Penalized deviance: 27212
dic5
#Mean deviance:  27203 
#penalty 23.9 
#Penalized deviance: 27227
dic7
#Mean deviance:  27622 
#penalty 3.917 
#Penalized deviance: 27626 
summary(mod_sim6)


mod_string8 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Smoker[i] 
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:1) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags8 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params8 = c("a0", "alpha", "b", "sigma", "tau")        

mod8 = jags.model(textConnection(mod_string8), data = data_jags8, n.chains = 3)
update(mod8, 1e3)
mod_sim8 = coda.samples(model = mod8, variable.names = params8, n.iter = 5e3)

mod_csim8 = as.mcmc(do.call(rbind, mod_sim8))
#dim(mod_csim)

dic8 = dic.samples(mod8, 1e3)
dic6
#Mean deviance:  27199 
#penalty 13.08 
#Penalized deviance: 27212
dic5
#Mean deviance:  27203 
#penalty 23.9 
#Penalized deviance: 27227
dic7
#Mean deviance:  27622 
#penalty 3.917 
#Penalized deviance: 27626 
dic8
#Mean deviance:  27761 
#penalty 5.908 
#Penalized deviance: 27767


mod_string9 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] + b[2]*Age[i] 
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:2) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags9 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params9 = c("a0", "alpha", "b", "sigma", "tau")        

mod9 = jags.model(textConnection(mod_string9), data = data_jags9, n.chains = 3)
update(mod9, 1e3)
mod_sim9 = coda.samples(model = mod9, variable.names = params9, n.iter = 5e3)

mod_csim9 = as.mcmc(do.call(rbind, mod_sim9))
#dim(mod_csim)

dic9 = dic.samples(mod9, 1e3)
dic9
dic5
dic6

head(data)
3


mod_string10 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Age[i] + b[4]*Bmi[i]*Smoker[i]
                
        }
        
        
        for (j in 1:max(Region)) {        
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        
        a0 ~ dnorm(0.0, 1.0/1.0e6)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)
        
        
        
        for (k in 1:4) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

set.seed(183)
data_jags10 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                  Bmi = data$bmi,
                  Children = data$children,
                  Smoker = as.numeric(data$smoker=="yes"), 
                  Region = as.numeric(as.factor(data$region)),
                  y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params10 = c("a0", "alpha", "b", "sigma", "tau")        

mod10 = jags.model(textConnection(mod_string10), data = data_jags10, n.chains = 3)
update(mod10, 1e3)
mod_sim10 = coda.samples(model = mod10, variable.names = params10, n.iter = 5e3)

mod_csim10 = as.mcmc(do.call(rbind, mod_sim10))
#dim(mod_csim)

dic10 = dic.samples(mod10, 1e3)
dic10
#Mean deviance:  26617 
#penalty 8.27 
#Penalized deviance: 26625 
dic6
#Mean deviance:  27199 
#penalty 13.08 
#Penalized deviance: 27212
dic5
#Mean deviance:  27203 
#penalty 23.9 
#Penalized deviance: 27227

summary(mod_sim6)
summary(data)
dev.new()
x=1:20
y=x^2
plot(lm(y~x)) 
plot(lm(dist~speed, data=cars))
?dexp
curve((dexp(2,60000)))
hist(jitter(data$charges))
set.seed(183)
mod_string11 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha[Region[i]] + b_Smoker*Smoker[i] 
                + b_Age*Age[i] +b_Bmi*Bmi[i] + b_Children*Children[i]
           
                #log(mu[i]) = alpha[Region[i]] + b_Smoker*log(1+Smoker[i]) 
                #+ b_Age*log(Age[i]) +b_Bmi*log(Bmi[i]) + b_Children*log(Children[i])
           
        }
        
        
        
        for (j in 1:max(Region)) {     
                alpha[j] ~  dnorm(a0, prec_a)       
        }
        a0 ~ dnorm(3.0e2, 1.0/1.0e2)
        prec_a ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        tau2 = 1.0 / prec_a
        tau = sqrt(tau2)

        b_Smoker ~ dnorm(5.0e3, 1.0/1.0e4)
        b_Age ~ dnorm(2.0e2, 1.0/1.0e2)
        b_Bmi ~ dnorm(7.0e1, 1.0/1.0e2)
        b_Children ~ dnorm(3.0e1, 1.0/1.0e2)
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

?dgamma

data_jags11 = list(Age=data$age, #Sex=as.numeric(data$sex == "male"),
                   Bmi = data$bmi,
                   Children = data$children,
                   Smoker = as.numeric(data$smoker=="yes"), 
                   Region = as.numeric(as.factor(data$region)),
                   y = data$charges)
head(data$charges)
params11 = c("a0", "alpha", "b_Smoker", "b_Age", "b_Bmi","b_Children", "sigma", "tau")        
mod11 = jags.model(textConnection(mod_string11), data = data_jags11, n.chains = 3)
update(mod11, 1e3) #burn-in
mod_sim11 = coda.samples(model = mod11, variable.names = params11, n.iter = 5e3)
mod_csim11 = as.mcmc(do.call(rbind, mod_sim11))

#gelman.diag(mod_sim11)
#dev.new()
#autocorr.plot(mod_csim11)
#effectiveSize(mod_sim11)
#dim(mod_csim)

dic11 = dic.samples(mod11, 1e3)
dic11
#Mean deviance:  26611 
#penalty 6.018 
#Penalized deviance: 26617 

dev.new()
traceplot(mod_csim11)

traceplot(mod_sim11)
summary(mod_csim11)
gelman.diag(mod_sim11)

 dic10
#Mean deviance:  26617 
#penalty 8.27 
#Penalized deviance: 26625 

pmed_coef11 = apply(mod_csim11, 2, mean) ##2 for columns

data$Region = as.numeric(as.factor(data$region))
data$Smoker = as.numeric(data$smoker=="yes")
table(data$Smoker)
str(data$Smoker)
str(data)
mu_int11 =  as.matrix(data[c("bmi")]) %*% (as.matrix(data[c("Smoker")]) 
mu_int12 = mu_int11 %*%   pmed_coef11[c("b[4]")]
mu_1 = data[c("bmi")] %*% mu_int11
data$Region[1]

for (i in 1:length(data$Region)) {
        #print(i)
        #print(data$Region[i])
        data$alpha_values[i] = pmed_coef11[data$Region[i]+1]
        #print(data$Region[i]+1)
        #print(data$alpha_values[i])
        #print(pmed_coef11)
}
str(data)
data$charges_predicted11 = drop(as.matrix(data[c("alpha_values")]) + as.matrix(data[c("Smoker", "bmi", "age")]) %*% pmed_coef11[c( "b_Smoker", "b_Bmi", "b_Age")])
str(mu_int1)
head(data)
View(data)
#+ as.matrix(data[c("bmi")]) %*% mu_int11

data$resid11 = data$charges_predicted11 - data$charges

hist(data$charges)
table(data$charges>50000)
curve(dgamma(x, 0.5, 1.0))
dev.new()
plot(jitter(data$charges_predicted11), data$resid11)
ind <- resid11`<= 0
table(ind)
dev.new()
qqnorm(mod_csim11)
?plot()
#+ data[c("age")] %*% as.matrix(data[c("smoker")]) %*% pmed_coef11[c("b[5]")])
}
View(data)
#b[1]*Bmi[i] +b[2]*Smoker[i] + b[3]*Age[i] 
#+ b[4]*Bmi[i]*Smoker[i] + b[5]*Age[i]*Smoker[i]


l1 = 30.6634*pmed_coef11[c("b[1]")]+1*pmed_coef11[c("b[2]")]+40*pmed_coef11[c("b[3]")]+30.6634*1*pmed_coef11[c("b[4]")]+40*1*pmed_coef11[c("b[5]")]
l2 = 30.6634*pmed_coef11[c("b[1]")]+0*pmed_coef11[c("b[2]")]+40*pmed_coef11[c("b[3]")]+30.6634*0*pmed_coef11[c("b[4]")]+40*1*pmed_coef11[c("b[5]")]


str(data)
mean(data$bmi)
set.seed(183)
mod_string12 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = b[1] + b[2]*Bmi[i] + b[3]*Age[i] + b[4]*Bmi*Smoker[i]
                
        }
        
        
        
        for (k in 1:4) {
        b[k] ~ dnorm(0.0, 1.0/1.0e6) 
        }
        
        prec ~ dgamma(1.0/2.0, 1.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

data_jags12 = list(Age=data$age, Sex=as.numeric(data$sex == "male"),
                   Bmi = data$bmi,
                   Children = data$children,
                   Smoker = as.numeric(data$smoker=="yes"), 
                   Region = as.numeric(as.factor(data$region)),
                   y = data$charges)
head(Region)
head(as.factor(data$region))
str(data)
str(dat)
params12 = c( "b", "sigma")        

mod12 = jags.model(textConnection(mod_string12), data = data_jags12, n.chains = 3)
update(mod12, 10e3)
mod_sim12 = coda.samples(model = mod12, variable.names = params12, n.iter = 50e3)

mod_csim12 = as.mcmc(do.call(rbind, mod_sim12))
#dim(mod_csim)

dic12 = dic.samples(mod12, 1e3)
dic12
#Mean deviance:  26645 
#penalty 5.505 
#Penalized deviance: 26650 

dic11
#Mean deviance:  26611 
#penalty 7.18 
#Penalized deviance: 26618

gelman.diag(mod_sim12)
dev.new()
autocorr.plot(mod_csim12)
effectiveSize(mod_sim12)
dim(mod_csim)
raftery.diag(mod_sim12)

pmed_coef12 = apply(mod_csim12, 2, mean) ##2 for columns




                                           
str(data)
data$charges_predicted12 =  as.matrix(data[c("bmi", "age", "Bmi_Smoker")]) %*% pmed_coef12[c("b[1]", "b[2]", "b[3]")]
str(mu_int1)
data$charges_predicted = mu_int1      
head(data)
View(data[c("charges", "charges_predicted")])
#+ as.matrix(data[c("bmi")]) %*% mu_int11

resid12 = data$charges - data$charges_predicted12
dev.new()
qqnorm(resid12)

qqnorm(resid)

hist(data$charges)

set.seed(117)
n = 1000
z = numeric(n)
y = numeric(n)
for (i in 1:n) {
        z[i] = sample.int(2, 1, prob=c(0.3, 0.7)) # returns a 1 with probability 0.4, or a 2 with probability 0.6
        if (z[i] == 1) {
                y[i] = rexp(1, rate=100.0)
        } else if (z[i] == 2) {
                y[i] = rnorm(1, mean=2.0, sd=3.0)
        }
}
hist(y, breaks=30)

?rexp






set.seed(183)
mod_string13 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dexp(mu[i])
                mu[i] = alpha[Region[i]] + b[2]*Smoker[i] 
                + b[3]*Bmi[i]*Smoker[i] + b[4]*Age[i]*Smoker[i]
                
        }
        for (j in 1:max(Region)) {        
                alpha[j] ~  dgamma(1000, 10)       
        }
        
        
        for (k in 2:4) {
        b[k] ~ dgamma(1000, 10) 
        }
        
                
} "

?dgamma

data_jags13 = list(Age=data$age, #Sex=as.numeric(data$sex == "male"),
                   Bmi = data$bmi,
                   #Children = data$children,
                   Smoker = as.numeric(data$smoker=="yes"), 
                   Region = as.numeric(as.factor(data$region)),
                   y = data$charges)

params13 = c("alpha", "b")        
mod13 = jags.model(textConnection(mod_string13), data = data_jags13, n.chains = 3)
update(mod13, 1e3) #burn-in
mod_sim13 = coda.samples(model = mod13, variable.names = params13, n.iter = 5e3)
mod_csim13 = as.mcmc(do.call(rbind, mod_sim13))

dic13 = dic.samples(mod13, 1e3)
curve(rexp(x, rate=1.0e2))

gelman.diag(mod_sim13)
dev.new()
autocorr.plot(mod_csim13)
effectiveSize(mod_sim13)
dim(mod_csim)
raftery.diag(mod_sim13)

pmed_coef13 = apply(mod_csim13, 2, mean) ##2 for columns

for (i in 1:length(data$region)) {
        data$alpha_values[i] = pmed_coef13[data$Region[i]+1]
}

str(data)
data$charges_predicted13 = drop(as.matrix(data[c("alpha_values")]) + as.matrix(data[c("Smoker", "Bmi_Smoker", "Age_Smoker")]) %*% pmed_coef13[c( "b[2]", "b[3]", "b[4]")])

head(data)
View(data[c("charges", "charges_predicted13")])
#+ as.matrix(data[c("bmi")]) %*% mu_int11
curve(dgamma(x,10.0, 1.0*100.0/2.0))
curve( 0.4*dexp(x, 1.0) + 0.6*dnorm(x, 3.0, 1.0), from=-2.0, to=7.0, ylab="density", xlab="y", main="40/60 mixture of exponential and normal distributions", lwd=2)
resid13 = data$charges - data$charges_predicted12
dev.new()
qqnorm(resid12)

qqnorm(resid)