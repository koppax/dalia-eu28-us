us<-read.csv("../../DaliaTrumpEffect2/coded_csv/data_coded_US.csv - data_coded_US.csv.csv",header = TRUE)

names(us)<-sub("X\\..+\\.\\.(.+)", "\\1", names(us));
str(us);
names(us);

names(us)[6]

usLevels<-aggregate(codes$code, by=list(varname=codes$question_id_mapped),FUN=function(v) return(v))
usLabels<-aggregate(codes$value, by=list(varname=codes$question_id_mapped), FUN=function(v) return(as.character(v)))


makeToFactorUS<-function(varName, ord=FALSE) {
  varName<-deparse(substitute(varName))
  varName<-unlist(strsplit(varName, "\\$"))
  varName<-varName[length(varName)]
  return (factor(us[,varName],ordered = ord, levels=unlist(usLevels$x[usLevels$varname==varName]),
                 labels = unlist(usLabels$x[usLabels$varname==varName])))
}
voteForUSElection<-makeToFactorUS(us$vote_for_in_us_election)


curioMannerUS<-makeToFactorUS(curiosity_or_good_manners)

opinionChangeUS<-NA
opinionChangeUS[us$change_economy_country_past12months<us$economy_country_next12months]<-"better"
opinionChangeUS[us$change_economy_country_past12months>us$economy_country_next12months]<-"worse"
opinionChangeUS[us$change_economy_country_past12months==6]<-NA
opinionChangeUS[us$economy_country_next12months==6]<-NA
opinionChangeUS[us$change_economy_country_past12months==6 & us$economy_country_next12months<3]<-"better"
opinionChangeUS[us$change_economy_country_past12months==6 & us$economy_country_next12months>3]<-"worse"
table(curioMannerUS)

sjp.xtab(opinionChangeUS, curioMannerUS, weight.by = us$weight, margin = "row", bar.pos = "stack")

sjp.xtab(voteForUSElection, curioMannerUS, weight.by = us$weight, margin = "row", bar.pos = "stack")

childShouldBeIndOrRespUS<-makeToFactorUS(us$independence_or_respect)
sjp.xtab(voteForUSElection, childShouldBeIndOrRespUS, weight.by = us$weight, margin = "row", bar.pos = "stack")

childShouldBeObedientORSelfUS<-makeToFactorUS(us$obedience_or_selfreliance)
sjp.xtab(voteForUSElection, childShouldBeObedientORSelfUS, weight.by = us$weight, margin = "row", bar.pos = "stack")

childShouldBeConsiderateOrBehaveUS<-makeToFactorUS(us$consideration_or_good_behaviour)
sjp.xtab(voteForUSElection, childShouldBeConsiderateOrBehaveUS, weight.by = us$weight, margin = "row", bar.pos = "stack")
