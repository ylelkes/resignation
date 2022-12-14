library(tidyverse)
week2 <- rio::import("../../survey data/week2_2022_label.csv")
week3 <- rio::import("../../survey data/week3_2022_label.csv")
week2 <- week2 %>% select(!starts_with("w2"),res1=w2_2022_2_resignation_1,res2=w2_2022_2_resignation_2)
week3 <- week3 %>% select(!starts_with("w3"),res1=w3_2022_2_resignation_1,res2=w3_2022_2_resignation_2)
resignation <- rbind(week2,week3)
save(resignation,file="resignation.RData")

resignation <- data %>% left_join(formerge,by="caseid")
resignation$w2_2022_2_resignation_1 <- factor(resignation$w2_2022_2_resignation_1,levels=c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree"))
resignation$w2_2022_2_resignation_2 <- factor(resignation$w2_2022_2_resignation_2,levels=c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree"))
resignation <- resignation %>% filter(engaged=="Engaged")
label(resignation$w2_2022_2_resignation_1)
ggplot(resignation, aes(w2_2022_2_resignation_1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") + theme_bw(base_size = 15)+ggtitle("America is heading toward the end of democracy, where free and fair elections will no longer occur.") +xlab("")

resignation %>% group_by(pid_r) %>% count(w2_2022_2_resignation_1,wt = weight) %>% mutate(p=n/sum(n))  %>% ggplot( aes(x=w2_2022_2_resignation_1,y=p)) + 
  geom_bar(stat="identity")+facet_wrap(~pid_r)+ggtitle("America is heading toward the end of democracy, where free and fair elections will no longer occur.") 


ggplot(resignation, aes(w2_2022_2_resignation_2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") + theme_bw(base_size = 15)+ggtitle("Very little can be done to stop American democracy from ending in the next decade.") +xlab("")
resignation$res1 <- resignation$w2_2022_2_resignation_1 %in% c("Somewhat agree","Strongly agree")
resignation$inparty

resignation$res2 <- resignation$w2_2022_2_resignation_2 %in% c("Somewhat agree","Strongly agree")

resignation <- resignation %>% mutate(pid_r = case_when(pid7<4 ~ 'Democrat',
                                         pid7>4 ~ 'Republican',
                                         pid7==4 ~ 'Independent'))
prop.table(xtabs(resignation$weight~resignation$pid_r+resignation$res1),1)
prop.table(xtabs(resignation$weight~resignation$res2+resignation$res1))

resignation$resigned <- interaction(resignation$res1,resignation$res2)
resignation$resigned <- factor(resignation$resigned,labels=c("Not worried","Worried but not resigned","Not worried but resigned","Resigned"))
  ifelse(resignation$res1==T & resignation$res2==T,1,0)
prop.table(table((as.numeric(resignation$res1)-1)*(as.numeric(resignation$res2)-1)))
prop.table(xtabs(resignation$weight~resignation$resigned))


ggplot(resignation, aes(w2_2022_2_resignation_2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") + theme_bw(base_size = 15)+ggtitle("Very little can be done to stop American democracy from ending in the next decade.") +xlab("")

resignation %>% group_by(resigned) %>% summarise(m=mean(5-vote_importance))
ggplot(resignation, aes(w2_2022_2_resignation_2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") + theme_bw(base_size = 15)+ggtitle("Very little can be done to stop American democracy from ending in the next decade.") +xlab("")
resignation %>% group_by(pid_r) %>% count(resigned,wt = weight) %>% mutate(p=n/sum(n)) 
%>% ggplot( aes(x=resigned,y=p)) + 
  geom_bar(stat="identity")+facet_wrap(~pid_r)

ggplot(resignation, aes(w2_2022_2_resignation_2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") + theme_bw(base_size = 15)+ggtitle("Very little can be done to stop American democracy from ending in the next decade.") +xlab("")

resignation %>% group_by(pid_r) %>% count(resigned,wt = weight) %>% mutate(p=n/sum(n))  %>% ggplot( aes(x=resigned,y=p)) + 
  geom_bar(stat="identity")+facet_wrap(~pid_r)

resignation %>% group_by(resigned,pid_r) %>% summarise(m=mean(5-vote_importance))

data <- rio::import("../../survey data/week7_2022_label.csv")
formerge <- data %>% select(caseid,w2_2022_2_resignation_1,w2_2022_2_resignation_2)
