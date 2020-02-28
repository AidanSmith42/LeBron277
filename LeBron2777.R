library(XML)
library(RCurl)
library(tidyverse)
library(mvtnorm)


url <- paste0("https://www.basketball-reference.com/players/j/jamesle01/gamelog/", 2004, "/")
html <- getURL(url, ssl.verifypeer=F)
# download table
data <- readHTMLTable(html, which =8)
data <- data[,-6]
data <- data %>% select(G,PTS,TRB,AST) %>% filter(G != "G") %>% na.omit(data) %>% mutate(Year= 2004)
total <- data

for(i in 2005:2019){
url <- paste0("https://www.basketball-reference.com/players/j/jamesle01/gamelog/", i, "/")
html <- getURL(url, ssl.verifypeer=F)
# download table
data <- readHTMLTable(html, which =8)
data <- data[,-6]
data <- data %>% select(G,PTS,TRB,AST) %>% filter(G != "G") %>% na.omit(data) %>% mutate(Year= i)
total <- rbind(data, total)
}


total$PTS<- as.numeric(as.character(total$PTS))
total$TRB <- as.numeric(as.character(total$TRB))
total$AST <- as.numeric(as.character(total$AST))
paste0("AVG PTS: ", round(mean(total$PTS),0), " AVG REB: ", round(mean(total$TRB),0), " AVG AST: ", round(mean(total$AST),0))

total %>% group_by(Year) %>% summarize(PT= mean(PTS), AS= mean(AST), RB= mean(TRB)) %>% ungroup()
total %>% filter(PTS==27 & TRB== 7)
total %>% filter(AST==7 & TRB== 7)

nrow(total %>% filter(PTS==27))

69/1198

nrow(total %>% filter(TRB==7))

167/1198

nrow(total %>% filter(AST==7))

175/1198

#joint probability if assume independent
(nrow(total %>% filter(PTS==27))/nrow(total)) * (nrow(total %>% filter(TRB==7))/nrow(total)) * (nrow(total %>% filter(AST==7))/nrow(total))


correlations <- total[,c("PTS", "TRB", "AST")]
round(cov(correlations),2)


ggplot(total, aes(x=PTS)) + 
  geom_histogram(aes(y=..density..), bins=31, colour="black", fill="white")+
  geom_density(alpha=.4, fill="#FF6677")

ggplot(total, aes(x=TRB)) + 
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white")+
  geom_density(alpha=.4, fill="dodgerblue3")

ggplot(total, aes(x=AST)) + 
  geom_histogram(aes(y=..density..), bins = 20, colour="black", fill="white")+
  geom_density(alpha=.4, fill="green")

shapiro.test(total$AST)
shapiro.test(total$PTS)
shapiro.test(total$TRB)

SIGMA <- cov(correlations)
MEAN <- c(mean(total$PTS), mean(total$TRB), mean(total$AST))
probAll3 <- dmvnorm(c(27,7,7), MEAN, SIGMA )

probNever <- 1-probAll3
probNever
#for a single game

totalGames <- nrow(total)

#over 1198
round((probNever^totalGames)*100, 2)

##assuming 70 games/yr for 5 more yrs
probNever^(totalGames +5*70)


#joint independent probability from earlier 
joint <- (nrow(total %>% filter(PTS==27))/nrow(total)) * (nrow(total %>% filter(TRB==7))/nrow(total)) * (nrow(total %>% filter(AST==7))/nrow(total))
jointNever <- 1-joint
jointNever^totalGames
