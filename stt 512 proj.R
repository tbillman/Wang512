dat = read.csv(file = url("https://raw.githubusercontent.com/tbillman/Wang512/master/ships.csv"), header = FALSE)
colnames(dat) = c("Ship Name", "Brand", "Age", "Tonnage", "Passengers",
                  "Length", "Cabins", "Density", "Crew")
dat$`Ship Name` = as.factor(dat$`Ship Name`); dat$Brand = as.factor(dat$Brand)
head(dat, n = 1)
mod = lm(Passengers~ Brand + Age + Tonnage + Length + Density + Crew, data = dat)
summary(mod)
anova(mod)
#####Simple Linear Model#####
mod1 = lm(Passengers ~ Tonnage, data = dat)
summary(mod1)
x = dat$Tonnage;mx = mean(x)
y = dat$Passengers; my = mean(y)
confint(mod1)
##### Looking into the Model######
plot(y = mod1$residuals, x = dat$Tonnage)
boxplot(mod1$residuals)
qqplot(y = mod1$residuals, x= dat$Tonnage)
modr = lm(mod1$residuals~ dat$Tonnage)
summary(modr)
#generally looks good, qqplot is a little weird though

#####Bonferonni CI for b0 and b1#####
confint(mod1, level = .975)

#####With matrices#####
x = cbind(rep(1,dim(dat)[1]),dat$Tonnage)
y = dat$Passengers
b = solve(t(x) %*% x) %*% t(x) %*% y
yhat = x %*% b
H = x %*% solve(t(x) %*% x) %*% t(x)
all(round(H %*% H,2) == round(H,2)) #checking if H is idempotent
#finding different sum squares
sst = t(y) %*% (diag(1,dim(dat)[1],) - 1/dim(dat)[1]) %*% y;sst
sse = t(y) %*% (diag(1,dim(dat)[1],) - H) %*% y;sse
ssr = t(y) %*% (H - 1/dim(dat)[1]) %*% y;ssr
mse = sse / (dim(dat)[1] - length(b))
s2b = as.numeric(mse) * solve(t(x) %*% x); s2b

#finding E(Yh) and E(Ynew)
#let's look at a ship with a Tonnage of 100
syh = sqrt(mse * (1/dim(dat)[1] + ((100 - mean(x))^2)/sum((x - mean(x))^2)))
eyh = b[1] + 100*b[2]
#confidence interval
eyh - qt(.975,length(x) - length(b)) * syh; eyh + qt(.975,length(x) - length(b)) * syh

#Predicting the passenger value for a ship with Tonnage of 100
syn = sqrt(mse + syh^2)
eyn = eyh
eyn - qt(.975,length(x) - length(b)) * syn; eyn + qt(.975,length(x) - length(b)) * syn

#####Now with Multiple Regression Models#####
mod2 = lm(Passengers~ Brand + Age + Tonnage + Length + Density + Crew, data = dat)
summary(mod2)
brandcoord = lapply(levels(dat$Brand), function(x){
  which(dat$Brand == x)
})
brandscoord =lapply(1:length(brandcoord), function(x){
  vect = rep(0, dim(dat)[1])
  vect[brandcoord[[x]]] = 1
  return(vect)
})
brands = data.frame(rep(0,dim(dat)[1]))
for(x in 2:length(brandscoord)){
  brands = cbind(brands,brandscoord[[x]])
}
colnames(brands) = levels(dat$Brand)
brands[,1] = rep(0, dim(dat)[1])
x = as.matrix(cbind(rep(1,dim(dat)[1]), brands, dat$Age, dat$Tonnage, dat$Length, dat$Density, dat$Crew))
x = x[,-2]
y = dat$Passengers

b = solve(t(x) %*% x) %*% t(x) %*% y
H = x %*% solve(t(x) %*% x) %*% t(x)
all(round(H,2) == round(H %*% H,2)) #confirming H is idempotent

sst = t(y) %*% (diag(1,dim(dat)[1],) - 1/dim(dat)[1]) %*% y;sst
sse = t(y) %*% (diag(1,dim(dat)[1],) - H) %*% y;sse
ssr = t(y) %*% (H - 1/dim(dat)[1]) %*% y;ssr
mse = sse / (dim(dat)[1] - dim(x)[2])
msr = ssr / (dim(x)[2] - 1)
s2b = diag(as.numeric(mse) * solve(t(x) %*% x)); s2b
fval = msr/mse
pval = 1 - pf(fval, dim(x)[2] - 1, dim(dat)[1] - dim(x)[2])
#very strong
r2a = 1 - ((dim(dat)[1] - 1)/(dim(dat)[1] - dim(x)[2]))*(sse/sst);r2a
#still very solid
#bonferonni CIs for this data est
tstar = qt(1-.05/(dim(x)[2] - 1),dim(dat)[1] - dim(x)[2])
CIs = cbind(b - tstar*s2b, b + tstar*s2b);colnames(CIs) = c("lower","upper");CIs

#####Scaling data and looking at the correlation matrix#####
numvars = cbind(dat$Age,dat$Tonnage, dat$Length, dat$Density, dat$Crew)
numvars = lapply(1:dim(numvars)[2],function(x){
  scale(numvars[,x])
})
numvars
dats = dat
dats$Age = numvars[[1]]; dats$Tonnage = numvars[[2]]; dats$Length = numvars[[3]]; dats$Density = numvars[[4]]; dats$Crew = numvars[[5]]
dats$Passengers = scale(dat$Passengers)
usef = cbind(dats$Passengers, dats$Age, dats$Tonnage, dats$Length, dats$Density, dats$Crew)
cor(usef)
#looks like Tonnage, Length, and Crew all have high correlation with Passengers, but also high correlation with one another
mod3 = lm(Passengers~ Tonnage + Length + Crew, data = dat);summary(mod3)
anova(mod3)
#find vif
library("car")
vif(mod3)
vif(mod2)