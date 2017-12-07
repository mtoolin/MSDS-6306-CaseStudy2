#load packages
library("readxl")
library(tidyr)
library(dplyr)
library(ggplot2)

library(stringr)
#load raw data from ~/data , save the data set as a CSV file called refine_original.csv and load it

rawdata.xls <- "data/CaseStudy2-data.xlsx"
rawdata.csv <- "data/original.csv"
yacmonthly.csv <- "data/yacmonthly2.csv"


refine <- read_excel(rawdata.xls,1)
View(refine)
refine %>% data.table::fwrite(rawdata.csv)
yacmonthly <- read.csv(yacmonthly.csv, stringsAsFactors = TRUE, skip = 1)
dframe <- read.csv(rawdata.csv)
rawdata <- dframe

plot_data <- dframe %>% 
  group_by(Attrition, Gender) %>% 
  tally %>% 
  mutate(percent = n/sum(n))

ggplot(plot_data, aes(x = Gender, y = percent,fill=Gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(..y..)), vjust = -0.5) +
  labs(title = "Gender by Attrition", y = "Percent", x = "Gender") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  facet_wrap(~Attrition) 

ggplot(rawdata, aes(x = Attrition)) + 
  geom_bar(aes(y = (..count..)/sum(..count..),fill=Gender)) + 
  labs(y="Percent")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)
  #facet_wrap(~Attrition) 
  scale_y_continuous(labels = scales::percent)
  
  ggplot(rawdata, aes(x=Attrition, y=value, fill = Gender)) +
    geom_bar(aes(y = (..count..)/sum(..count..),fill=Gender))  +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = paste0(..y..*100,"%")), 
              position = position_stack(vjust = 0.5), size = 2)

ggplot(rawdata,aes(Gender,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(Gender, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",col="Gender")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)


ggplot(rawdata,aes(HourlyRate,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(HourlyRate, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5,check_overlap = TRUE)+
  labs(y="Percent",fill="HourlyRate")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)


ggplot(rawdata,aes(JobInvolvement,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(JobInvolvement, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobInvolvement")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

ggplot(rawdata,aes(JobLevel,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(JobLevel, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobLevel")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

ggplot(rawdata,aes(JobRole,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(JobRole, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobRole")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  facet_grid(~Attrition)

ggplot(rawdata,aes(JobSatisfaction,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(JobSatisfaction, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobSatisfaction")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

ggplot(rawdata,aes(MaritalStatus,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(MaritalStatus, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="MaritalStatus")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# ggplot(rawdata,aes(MonthlyIncome,fill=Attrition)) + geom_bar(position="dodge")
# ggplot(rawdata,aes(MonthlyIncome, group=Attrition))+
#   geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
#   geom_text(aes(label=scales::percent(..prop..),
#                 y=..prop..),stat="count",vjust=-.5)+
#   labs(y="Percent",fill="MonthlyIncome")+
#   scale_y_continuous(labels=scales::percent)+
#   facet_grid(~Attrition)
# 
# ggplot(rawdata,aes(MonthlyRate,fill=Attrition)) + geom_bar(position="dodge")
# ggplot(rawdata,aes(MonthlyRate, group=Attrition))+
#  geom_boxplot()
#   geom_text(aes(label=scales::percent(..prop..),
#                 y=..prop..),stat="count",vjust=-.5)+
#   labs(y="Percent",fill="MonthlyRate")+
#   scale_y_continuous(labels=scales::percent)+
#   facet_grid(~Attrition)


  
#dens <- density(rawdata$MonthlyRate)
#df <- data.frame(x=dens$x, y=dens$y)
#probs <- c(0, 0.25, 0.5, 0.75, 1)
#quantiles <- quantile(rawdata$y, prob=probs)
#rawdata$quant <- factor(findInterval(df$x,quantiles))
#ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + scale_fill_brewer(guide="none")


# define the summary function
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(rawdata, aes(Attrition, MonthlyRate)) + stat_summary(fun.data = f, geom="boxplot")


# define outlier as you want 0-25%  and 75-100% outeliers   
o <- function(x) {
  subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
}

# do it
ggplot(rawdata, aes(Attrition, MonthlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")

ggplot(rawdata, aes(Attrition, MonthlyIncome)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")

ggplot(rawdata, aes(Attrition, HourlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")



ggplot(rawdata,aes(NumCompaniesWorked,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(NumCompaniesWorked, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="NumCompaniesWorked")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

ggplot(rawdata,aes(Over18,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(Over18, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Over18")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)
