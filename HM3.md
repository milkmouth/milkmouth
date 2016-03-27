###NBA 2014-2015球季 各隊分析
if(!require('SportsAnalytics')){
    
    install.packages("SportsAnalytics")
    install.packages("knitr")
    library(SportsAnalytics)
    
}

NBA1415<-fetch_NBAPlayerStatistics("14-15")
NBA1415



##各隊最辛苦的球員
TotalMinutesPlayedOutput<-NULL
for(team in unique(NBA1415$Team)){
    selectTeam<-subset(NBA1415,Team==team)
    TotalMinutesPlayedOutput<-rbind(TotalMinutesPlayedOutput,
                       selectTeam[
                           order(selectTeam$TotalMinutesPlayed,decreasing = T)[1],
                           c("Name","Team","TotalMinutesPlayed")])
}
library(knitr)

kable(TotalMinutesPlayedOutput[order(-TotalMinutesPlayedOutput$TotalMinutesPlayed),], digit=2)







##各隊得分王
TotalPointsOutput<-NULL
for(team in unique(NBA1415$Team)){
    selectTeam<-subset(NBA1415,Team==team)
    TotalPointsOutput<-rbind(TotalPointsOutput,
                       selectTeam[
                           order(selectTeam$TotalPoints,decreasing = T)[1],
                           c("Name","Team","TotalPoints")])
}
library(knitr)

kable(TotalPointsOutput[order(-TotalPointsOutput$TotalPoints),], digit=2)




##最有效率的球員
Maxefficiency<-aggregate(TotalPoints/TotalMinutesPlayed~Name,NBA1415,max)
NBA1415Maxefficiency<-merge(NBA1415,Maxefficiency)
output<-NBA1415Maxefficiency[order(NBA1415Maxefficiency$'TotalPoints/TotalMinutesPlayed',decreasing = T),
                             c("Team","Name","TotalPoints/TotalMinutesPlayed")]
library(knitr)




outputs<-NULL
for(team in unique(output$Team)){
    selectTeam<-subset(output,Team==team)
    outputs<-rbind(outputs,
                             selectTeam[
                                 order(selectTeam$'TotalPoints/TotalMinutesPlayed',decreasing = T)[1],
                                 c("Name","Team","TotalPoints/TotalMinutesPlayed")])
}
library(knitr)
kable(outputs, digits=2)






##各隊三分球出手最準的球員
Maxthree<-aggregate(ThreesMade/ThreesAttempted~Name,NBA1415,max)
NBA1415Maxthree<-merge(NBA1415,Maxthree)
outputthree<-NBA1415Maxthree[order(NBA1415Maxthree$'ThreesMade/ThreesAttempted',decreasing = T),
                             c("Team","Name","ThreesMade/ThreesAttempted")]
library(knitr)




outputthrees<-NULL
for(team in unique(outputthree$Team)){
    selectTeam<-subset(outputthree,Team==team)
    outputthrees<-rbind(outputthrees,
                   selectTeam[
                       order(selectTeam$'ThreesMade/ThreesAttempted',decreasing = T)[1],
                       c("Name","Team","ThreesMade/ThreesAttempted")])
}
library(knitr)
kable(outputthrees, digits=2)