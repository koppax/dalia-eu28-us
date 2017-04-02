dges<-read.csv("../../DaliaTrumpEffect2/coded_csv/data_coded_e28.csv - data_coded_e28.csv.csv",header = TRUE)

names(dges)<-sub("X\\..+\\.\\.(.+)", "\\1", names(dges));
str(dges);
names(dges);

names(dges)[6]

library(sjPlot)
sjp.frq(testVar, weight=dges$weight)
sjp.frq(immig, weight.by = dges$weight)
table(immig)
table(dges$immigration)

x<-factor(dges$gender,levels=c(1,2), labels=c("male", "female"))
str(x)
sjp.frq(x,weight.by = dges$weight)
barplot(table(x))
sjt.frq(dges$gender,weight.by = dges$weight)
sjt.frq(x,weight.by = dges$weight)
sjt.frq(immig,weight.by = dges$weight)
table(testVar)
sjp.frq(testVar, weight.by = dges$weight)

# read codes
codes<-read.csv("../../DaliaTrumpEffect2/codebook.csv",header = TRUE, stringsAsFactors = FALSE)
codes$question_id_mapped <- sub("\\[.+\\] (.+)", "\\1", codes$question_id_mapped)
# build lists that map varNames->codes and labels
dgesLevels<-aggregate(codes$code, by=list(varname=codes$question_id_mapped),FUN=function(v) return(v))
dgesLabels<-aggregate(codes$value, by=list(varname=codes$question_id_mapped), FUN=function(v) return(as.character(v)))

s<-tapply(dges$weight, immig, sum)
barplot(s)
s
# varName<-"gender"
unlist(dgesLevels$x[dgesLevels$varname==varName])
unlist(dgesLabels$x[dgesLabels$varname==varName])
# immig<-factor(dges[,varName],levels=unlist(dgesLevels$x[dgesLevels$varname==varName]),
#              labels = unlist(dgesLabels$x[dgesLabels$varname==varName]))
immig

makeToFactor<-function(varName, ord=FALSE) {
  varName<-deparse(substitute(varName))
  varName<-unlist(strsplit(varName, "\\$"))
  varName<-varName[length(varName)]
  return (factor(dges[,varName],ordered = ord, levels=unlist(dgesLevels$x[dgesLevels$varname==varName]),
                labels = unlist(dgesLabels$x[dgesLabels$varname==varName])))
}
tv<-makeToFactor(dges$media_tv_hours, ord = TRUE)
opeu<-makeToFactor(dges$opinion_eu, ord = TRUE)
freqsharingopin<-makeToFactor(dges$frequent_sharing_of_politicalviews, ord = TRUE)
jobsec<-makeToFactor(dges$job_security)

sjp.frq(dgesl$media_tv_hours)
str(opeu)
str(dges$opinion_eu)
opeurec<-dges$opinion_eu
opeurec[dges$opinion_eu<3]<-"positive"
opeurec[dges$opinion_eu==3]<-"neutral"
opeurec[dges$opinion_eu>3]<-"negative"
opeurec
table(dgesl$media_tv_hours)

education<-makeToFactor(dges$education_level)

sjp.xtab(tv, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(jobsec, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(tv, jobsec, weight.by = dges$weight, margin = "row", bar.pos = "stack")

curiosity_manners_child<-makeToFactor(dges$curiosity_or_good_manners)
sjp.xtab(curiosity_manners_child, opeurec, margin = "row", bar.pos = "stack")
sjp.xtab(tv, curiosity_manners_child, margin = "row", bar.pos = "stack")

democracy_own_country<-makeToFactor(dges$democracy_own_country_satisfaction, ord = TRUE)
sjp.xtab(democracy_own_country, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(democracy_own_country, jobsec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(curiosity_manners_child, democracy_own_country, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(democracy_own_country, tv, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(tv, democracy_own_country, weight.by = dges$weight, margin = "row", bar.pos = "stack")

sjp.xtab(dges$frequent_sharing_of_politicalviews, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(freqsharingopin, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")

gov_contr_by_elite<-makeToFactor(dges$government_controlled_by_elite, ord = TRUE)
sjp.xtab(gov_contr_by_elite, jobsec, weight.by = dges$weight, margin = "row", bar.pos = "stack")

disp_income<-makeToFactor(dges$disposable_income, ord = TRUE)
worktype_routine<-makeToFactor(dges$work_type_routine)
education<-makeToFactor(dges$education_level)

sjp.xtab(worktype_routine, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(education, opeurec, weight.by = dges$weight, margin = "row", bar.pos = "stack")

sjp.xtab(worktype_routine, curiosity_manners_child, weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(worktype_routine, tv, weight.by = dges$weight, margin = "row", bar.pos = "stack")

economyChangePast12Month<-NA
economyChangePast12Month[dges$change_economy_country_past12months<3]<-"better"
economyChangePast12Month[dges$change_economy_country_past12months==3]<-"same"
economyChangePast12Month[dges$change_economy_country_past12months>3]<-"worse"
# economyChangePast12Month[dges$change_economy_country_past12months>5]<-"Don't know"
length(economyChangePast12Month)

economyChangeNext12Month<-NA
economyChangeNext12Month[dges$economy_country_next12months<3]<-"better"
economyChangeNext12Month[dges$economy_country_next12months==3]<-"same"
economyChangeNext12Month[dges$economy_country_next12months>3]<-"worse"
# economyChangeNext12Month[dges$economy_country_next12months>5]<-"Don't know"
length(economyChangeNext12Month)
table(dges$economy_country_next12months)

opinionChange<-NA
opinionChange[dges$change_economy_country_past12months<dges$economy_country_next12months]<-"better"
opinionChange[dges$change_economy_country_past12months>dges$economy_country_next12months]<-"worse"
opinionChange[dges$change_economy_country_past12months==6]<-NA
opinionChange[dges$economy_country_next12months==6]<-NA
opinionChange[dges$change_economy_country_past12months==6 & dges$economy_country_next12months<3]<-"better"
opinionChange[dges$change_economy_country_past12months==6 & dges$economy_country_next12months>3]<-"worse"
table(opinionChange)

sjp.xtab(dges$worldview, curiosity_manners_child, weight.by = dges$weight, margin = "row", bar.pos = "stack")

worldview<-factor(dges$worldview, labels = c("more pessimistic", "more optimistic"))
table(worldview)
table(dges$worldview)
sjp.xtab(economyChangePast12Month, economyChangeNext12Month , weight.by = dges$weight, margin = "row", bar.pos = "stack")
sjp.xtab(opinionChange, opeurec , weight.by = dges$weight, margin = "row", bar.pos = "stack")


library(descr)
crosstab(tv,opeurec, weight = dgesl$weight, percent = T,sresid = T, chisq = T)
crosstab(dgesl$financial_security, dgesl$degree_of_urbanisation, weight = dgesl$weight, sresid = TRUE,chisq = TRUE)
freq(dgesl$gender, dgesl$weight)

freq(opinionChange)
freq(opeu, weight = dges$weight)

xtab(disp_income, opeurec)
xtab<-function(x,y) {
  sjp.xtab(x,y, weight.by = dges$weight, margin = "row", bar.pos = "stack")  
}

n<-tapply(df$z, df$x, function(v) return(as.character(v)))
n
str(n)
          
o<-aggregate(df$z, by=list(name=df$x), FUN=function(v) return(as.character(v)))
o

library("readxl")

t<-replicate(90, "text");
t[2]<-"numeric";
t[4]<-"numeric";

dgesl<-read_excel("../../DaliaTrumpEffect2/dalia_research_challenge_europulse.xlsx",sheet=1, col_types=t, na="NA");
names(dgesl)<-sub("\\[.+\\] (.+)", "\\1", names(dgesl));
names(dgesl)
table(dgesl$gender)
lsum<-tapply(dgesl$weight, dgesl$gender, sum)
lsum
tsum<-tapply(dges$weight, testVar, sum)
tsum
