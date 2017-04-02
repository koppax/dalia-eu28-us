library(survey)

td <- 
  svydesign(
    id = ~0 ,
    data = dgesl ,
    weight = ~weight
  )
sum(dgesl$weight)
degf(td)

data(api)

vars<-names(apiclus1)[c(12:13,16:23,27:37)]
vars
str(vars)

load(url("http://knutur.at/wsmt/R/RData/small.RData"))
summary(small)
summary(dgesl)

small.w <- svydesign(ids = ~1, data = small, weights = small$weight)
summary(small.w)

prop.table(table(small$sex))
prop.table(svytable(~sex, design = small.w))

svytable(~sex, design = small.w)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
a<-svymean(~interaction(stype,comp.imp), design=dclus1)
b<-ftable(a, rownames=list(stype=c("E","H","M"),comp.imp=c("No","Yes")))
b
ftable(dgesl$gender)
st<-svytable(~dgesl$gender, design=td)
st
a
ftable(a)
table(a)

library(descr)
sjmisc::descr(dgesl$financial_security)
crosstab(dgesl$change_household_finances_next12months, dgesl$financial_security, weight = dgesl$weight)
crosstab(dgesl$financial_security, dgesl$degree_of_urbanisation, weight = dgesl$weight, sresid = TRUE,chisq = TRUE)
freq(dgesl$gender, dgesl$weight)
library(faraway)
data(UCBAdmissions)
UCB.data <-
  as.data.frame.table(UCBAdmissions) 
table(UCBAdmissions)
view(UCBAdmissions)

qchisq(.999999999998,14)
qbinom(.75, 2, 0.5)
qbinom(0.999, size=100, prob=0.5)
pnorm(1.95)
x<-seq(-3,3,length=1000)
y<-dnorm(x, mean = 0, sd = 1)
plot(x,y, type = "l", lwd=1)

pmf<-dbinom(0:14,13,0.5)
df<-data.frame(w=0:14, prob=round(pmf,digits=4))
df
plot(x = df$w, y = df$prob, type = "h", xlab = "w", 
     ylab = "P(W=w)", main = "Plot of a binomial PMF for n=5, 
     pi=0.6", panel.first = grid(col="gray", lty="dotted"), 
     lwd = 1)
sjp.xtab(x, wei)

qnorm(.025)
str(UCBAdmissions)
table(UCBAdmissions)
