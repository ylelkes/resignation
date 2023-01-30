library(tidyverse)
week2 <- rio::import("../../survey data/week2_2022_label.csv")
week3 <- rio::import("../../survey data/week3_2022_label.csv")
week3$pid7

week2$engagement_measure <-  week2$w2_2022_1_wright_state
week2 <- week2 %>% select(!starts_with("w2"),res1=w2_2022_2_resignation_1,res2=w2_2022_2_resignation_2)
week3 <- week3 %>% select(!starts_with("w3"),res1=w3_2022_2_resignation_1,res2=w3_2022_2_resignation_2)
week2 <- week2 %>% select(intersect(names(week2),names(week3)))
week3 <- week3 %>% select(intersect(names(week2),names(week3)))
resignation <- rbind(week2,week3)
resignation <- filter(resignation,engagement_measure=="Iowa")
save(resignation,file="resignation.RData")

library(tidyverse)
week14_l <- rio::import("../../survey data/week14_2022_label.csv")
table(week14_l$resig_treat)

table(week14$resig_treat)
week14 <- rio::import("../../survey data/week14_2022.csv")
week15 <- rio::import("../../survey data/week15_2022.csv")
week16 <- rio::import("../../survey data/week16_2022.csv")
week14 %>% select(resig_treat,resignation1,resignation2,engagement_measure,weight,pid7) -> a 
week15 %>% select(resig_treat,resignation1,resignation2,engagement_measure,weight,pid7) -> b
week16 %>% select(resig_treat,resignation1,resignation2,engagement_measure,weight,pid7) -> c

out <- rbind(a,b,c)
table(week14_l$resig_treat,week14_l$resignation1)

out$pid3a <- car::recode(out$pid7,"1:3='Democrat';4='Independent';5:7='Republican'")

out <- out %>% mutate(resig1 = case_when(resignation1 %in% c(1,2)~1,
                                  TRUE~0),
                      resig2 = case_when(resignation2 %in% c(1,2)~1,
                                         TRUE~0),
                      resigned=resig1*resig2)

out$resigned_ln <- 5-rowMeans(data.frame(out$resignation1,out$resignation2))
table(week14$resignation1)
out$resignation1 <- 6-out$resignation1
out$resignation2 <- 6-out$resignation2
out$resigned_ln <- rowMeans(data.frame(out$resignation1,out$resignation2))

prop.table(table(out$resignation1,out$resignation2))
out$resign_treat1 <- relevel(as.factor(out$resig_treat),ref=4)
summary(lm(resignation1~as.factor(resig_treat),subset(out,engagement_measure==1 & pid3a=='Democrat',weights = weight)))


a <- as.table(c(364,676,2711,1748,1811))
names(a) <- c("Agree Strongly","Agree somewhat","Neither agree nor disagree","Disagree somewhat","Disagree strongly")
long <- as.data.frame.table(a)
longer <- long[rep(1:nrow(long), long$Freq), -3]
sd(as.numeric(longer$Var1))
library(ggplot2)
library(tidyverse)
j <- long %>% mutate(p=Freq/sum(Freq))
j
ggplot(j,aes(x=Var1,y=p))+geom_col()+theme_bw(base_size = 20)+scale_y_continuous(labels=scales::percent) 
prop.table(table(out$resig1,out$resig2))

week3 <- rio::import("../../survey data/week3_2022_label.csv")
week2$engagement_measure <-  week2$w2_2022_1_wright_state
week2 <- week2 %>% select(!starts_with("w2"),res1=w2_2022_2_resignation_1,res2=w2_2022_2_resignation_2)
week3 <- week3 %>% select(!starts_with("w3"),res1=w3_2022_2_resignation_1,res2=w3_2022_2_resignation_2)
week2 <- week2 %>% select(intersect(names(week2),names(week3)))
week3 <- week3 %>% select(intersect(names(week2),names(week3)))
resignation <- rbind(week2,week3)
resignation <- filter(resignation,engagement_measure=="Iowa")
save(resignation,file="resignation.RData")
