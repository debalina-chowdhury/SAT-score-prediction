library("faraway")
library("rjags")
library("coda")
data(sat)
?dic
data = sat
head(sat)
hist(data$total)
dev.new()
dev.new()
par(mfcol =c(2,2))
plot(total ~ expend, data=data, main = "SAT score vs expenditure")
plot(total ~ ratio, data = data, main = "SAT score vs pupil/teacher ratio")
plot(total ~ salary, data = data, main = "SAT score vs teacher's salary")
plot(total ~ takers, data = data, main = "SAT score vs SAT candidates")
summary(data)
pairs(data)
?sat
?par


set.seed(183)
mod_string11 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha + b_Expend*Expend[i] + b_Ratio*Ratio[i] 
                + b_Salary*Salary[i] + b_Takers*Takers[i]
               
        }
        alpha ~ dnorm(9.0e2, 1.0/4.0e4)
        b_Expend ~ dnorm(0.0, 1.0/1.0e1)
        b_Ratio ~ dnorm(0.0, 1.0/1.0e1)
        b_Salary ~ dnorm(0.0, 1.0/1.0e1)
        b_Takers ~ dnorm(0.0, 1.0/1.0e1)
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "
data_jags11 = list(Expend=data$expend,
                   Ratio = data$ratio,
                   Salary = data$salary,
                   Takers = data$takers, 
                    y = data$total)

params11 = c("alpha","b_Expend", "b_Ratio", 
             "b_Salary", "b_Takers","sigma")        
mod11 = jags.model(textConnection(mod_string11), 
                   data = data_jags11, n.chains = 3)
update(mod11, 10e3) #burn-in
mod_sim11 = coda.samples(model = mod11, 
                         variable.names = params11, n.iter = 50e3)
mod_csim11 = as.mcmc(do.call(rbind, mod_sim11))



#gelman.diag(mod_sim11)
#dev.new()
#autocorr.plot(mod_csim11)
#effectiveSize(mod_sim11)
#dim(mod_csim)

dic11 = dic.samples(mod11, 1e3)
dic11
dic13
dev.new()
plot(mod_csim11)
traceplot(mod_csim11)
str(summary(mod_csim11))
autocorr.plot(mod_csim11)
setwd("D:\\Bayesian_statistics_codes\\Bayesian_Statistics_project")
write.csv(summary(mod_csim11)$statistics, "summary_bayesian_model.csv")
gelman.diag(mod_sim11)
?sat
head(data)
pmed_coef11 = apply(mod_csim11, 2, mean)
yhat11 = drop(as.matrix(pmed_coef11[c("alpha")])) + as.matrix(data[c("expend", "ratio", "salary", "takers")]) %*% pmed_coef11[c("b_Expend", "b_Ratio", "b_Salary", "b_Takers")]
write.csv(mod_csim11, "mod_csim11.csv")
head(mod_csim11)
resid11 = data$total - yhat11
dev.new()
par(mfrow = c(2,2))
plot(yhat11, resid11, xlab = "yhat", ylab = "resid", main = "Residual plot")
qqnorm(resid11)
raftery.diag(mod_sim11)
head(rownames(data)[order(resid11, decreasing = TRUE)]) 
[1] "North Dakota"  "New Hampshire" "Iowa"          "Massachusetts"
[5] "Utah"          "South Dakota" +
setwd("D:\\Bayesian_statistics_codes\\Bayesian_Statistics_project")
write.csv(pmed_coef11, "pmed_coef11.csv")        

ind_Expend = mod_csim11[,2] > 0      ##posterior probability that b_Expend > 0  
mean(ind_Expend)      #0.694  
ind_Ratio = mod_csim11[,3] < 0  ##posterior probability that b_Ratio < 0 
mean(ind_Ratio) ##0.942
ind_Salary = mod_csim11[,4] > 0 ##posterior probability that b_Salary > 0
mean(ind_Salary) ##0.967
ind_Takers = mod_csim11[,5] < 0  ##posterior probability that b_Takers < 0
mean(ind_Takers) ##1

sample1 = c(6.5, 16, 33, 28)        
sample2 = c(4.8, 16, 33, 28)

yhat1 = drop(as.matrix(pmed_coef11[c("alpha")])) + sample1 %*% pmed_coef11[c("b_Expend", "b_Ratio", "b_Salary", "b_Takers")]
yhat2 = drop(as.matrix(pmed_coef11[c("alpha")])) + sample2 %*% pmed_coef11[c("b_Expend", "b_Ratio", "b_Salary", "b_Takers")]

yhat1 - yhat2

summary(data)        
        
        
        
set.seed(183)
mod_string12 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha + b_Expend*Expend[i] + b_Ratio*Ratio[i] + b_Salary*Salary[i] + b_Takers*Takers[i] +b_int*Ratio[i]*Salary[i]
               
        }
        
        
        alpha ~ dnorm(5.0e2, 1.0/1.0e4)
        b_Expend ~ dnorm(0.0, 1.0*4.0/1.0e2)
        b_Ratio ~ dnorm(0.0, 1.0/1.0e2)
        b_Salary ~ dnorm(0.0, 1.0*4.0/1.0e2)
        b_Takers ~ dnorm(0.0, 1.0/1.0e2)
        b_int ~ dnorm(0.0, )
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "

data_jags12 = list(Expend=data$expend,
                   Ratio = data$ratio,
                   Salary = data$salary,
                   Takers = data$takers, 
                   y = data$total)
head(data$charges)
params12 = c("alpha","b_Expend", "b_Ratio", "b_Salary", "b_Takers","sigma")        
mod12 = jags.model(textConnection(mod_string12), data = data_jags12, n.chains = 3)
update(mod12, 10e3) #burn-in
mod_sim12 = coda.samples(model = mod12, variable.names = params12, n.iter = 50e3)
mod_csim12 = as.mcmc(do.call(rbind, mod_sim12))



#gelman.diag(mod_sim11)
#dev.new()
#autocorr.plot(mod_csim11)
#effectiveSize(mod_sim11)
#dim(mod_csim)

dic12 = dic.samples(mod12, 1e3)
dic12
dic11
dev.new()
plot(mod_csim12)
traceplot(mod_csim12)
str(summary(mod_csim12))
autocorr.plot(mod_csim12)
write.csv(summary(mod_csim12)$statistics, "summary_bayesian_baseline_model.csv")
gelman.diag(mod_sim12)
effectiveSize(mod_sim11)



require(caTools)
set.seed(101) 
sample = sample.split(data$total, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
dim(data)
View(data)



set.seed(183)
mod_string13 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[i], prec)
                mu[i] = alpha + b_Expend*Expend[i] + b_Ratio*Ratio[i] 
                        + b_Salary*Salary[i] + b_Takers*Takers[i]
               
        }
        
        alpha ~ dnorm(0.0, 1.0/1.0e6)
        b_Expend ~ dnorm(0.0, 1.0/1.0e6)
        b_Ratio ~ dnorm(0.0, 1.0/1.0e6)
        b_Salary ~ dnorm(0.0, 1.0/1.0e6)
        b_Takers ~ dnorm(0.0, 1.0/1.0e6)
        
        prec ~ dgamma(5.0/2.0, 5.0*10.0/2.0)
        sigma2 = 1.0 / prec
        sigma = sqrt(sigma2)
                
} "
data_jags13 = list(Expend=data$expend,
                   Ratio = data$ratio,
                   Salary = data$salary,
                   Takers = data$takers, 
                   y = data$total)
params13 = c("alpha","b_Expend", "b_Ratio", 
             "b_Salary", "b_Takers","sigma")        
mod13 = jags.model(textConnection(mod_string13), 
                   data = data_jags13, n.chains = 3)
update(mod13, 10e3) #burn-in
mod_sim13 = coda.samples(model = mod13, 
                         variable.names = params13, n.iter = 50e3)
mod_csim13 = as.mcmc(do.call(rbind, mod_sim13))



#gelman.diag(mod_sim13)
#dev.new()
#autocorr.plot(mod_csim11)
#effectiveSize(mod_sim13)
#dim(mod_csim)

dic13 = dic.samples(mod13, 1e3)
dic13
dic11

pmed_coef13 = apply(mod_csim13, 2, mean)
yhat13 = drop(as.matrix(pmed_coef13[c("alpha")])) + as.matrix(data[c("expend", "ratio", "salary", "takers")]) %*% pmed_coef13[c("b_Expend", "b_Ratio", "b_Salary", "b_Takers")]
write.csv(mod_csim13, "mod_csim13.csv")
summary(mod_csim13)
head(mod_csim13)
resid13 = data$total - yhat13
dev.new()
par(mfrow = c(2,2))
plot(yhat13, resid13, xlab = "yhat", ylab = "resid", main = "Residual plot")
qqnorm(resid13)
raftery.diag(mod_csim13)

raftery.diag(mod_csim11)

dev.new()
plot(mod_csim13)
traceplot(mod_csim13)
str(summary(mod_csim12))
autocorr.plot(mod_csim13)
write.csv(summary(mod_csim13)$statistics, "summary_bayesian_baseline_model.csv")
gelman.diag(mod_sim13)
effectiveSize(mod_sim12)
effectiveSize(mod_sim11)
