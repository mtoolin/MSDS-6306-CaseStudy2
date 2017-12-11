#load packages
library("readxl")
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

##########################################################
#                                                        #
# Load raw data from ~/data , save the data set as a CSV #
# file called refine_original.csv and load it            #
#                                                        #
##########################################################

rawdata.xls <- "data/CaseStudy2-data.xlsx"
rawdata.csv <- "data/original.csv"

refine <- read_excel(rawdata.xls,1)
View(refine)
refine %>% data.table::fwrite(rawdata.csv)
dframe <- read.csv(rawdata.csv)
rawdata <- dframe

########################################################
#                                                      #
# Group some of the data together for better analysis  #
#                                                      #
########################################################

#
# Calculate Number Average years on Job vs Age based on quantiles
#
quantile (rawdata$Age)
ma <- tapply(rawdata$YearsAtCompany, rawdata$Age, mean)
madf<-data.frame(Age=names(ma),YearsWithCompany=ma)
madf$AgeGroup = cut(as.numeric(madf$Age),c(0,12,19,26,43), 
                    labels=c("18-29","30-36","37-43","44-60"))
ma.mean <- tapply(madf$Age, madf$AgeGroup, mean)
#
# Income Gouping Code
#
quantie.val <- quantile(rawdata$MonthlyIncome)
rawdata$IncomeGroup[rawdata$MonthlyIncome < quantie.val[1]] <- "Low"

rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[1] & 
                    rawdata$MonthlyIncome < quantie.val[2]] <- "Low Medium"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[2] &
                    rawdata$MonthlyIncome < quantie.val[3]] <- "Medium"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[3] &
                    rawdata$MonthlyIncome < quantie.val[4]] <- "Medium High"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[4] & 
                    rawdata$MonthlyIncome < quantie.val[5]] <- "High"
					
plot_data <- dframe %>% 
group_by(Attrition, Gender) %>% 
tally %>% 
mutate(percent = n/sum(n))

# Create category buckets for Age, MonthlyIncome, and YearsAtCompany based on quartiles
Agecut <- cut(rawdata$Age, breaks=c(17,30,36,43,100), labels = c("18-30","30-36","36-43", "43+"))
MonthlyIncomecut <- cut(rawdata$MonthlyIncome, breaks=c(1008,2911,4919,8379,20000), labels = c("1009-2911","2912-4919","4920-8379", "8380+"))
YearsAtCompanycut <- cut(rawdata$YearsAtCompany, breaks=c(-1,3,5,9,100), labels = c("0 to 3","3 to 5","5 to 9", "9+"))

# Tack new columns created in previous step onto rawdata
rawdata <- cbind(rawdata, Agecut, MonthlyIncomecut, YearsAtCompanycut)

# Create a series of mean variables for each category of Attrition & YearsAtCompanycut
zto3yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "0 to 3")
zto3yes_mean <- as.integer((mean(zto3yes$MonthlyIncome)))
threeto5yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "3 to 5")
threeto5yes_mean <- as.integer((mean(threeto5yes$MonthlyIncome)))
fiveto9yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "5 to 9")
fiveto9yes_mean <- as.integer((mean(fiveto9yes$MonthlyIncome)))
nineplusyes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "9+")
nineplusyes_mean <- as.integer((mean(nineplusyes$MonthlyIncome)))
zto3no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "0 to 3")
zto3no_mean <- as.integer((mean(zto3no$MonthlyIncome)))
threeto5no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "3 to 5")
threeto5no_mean <- as.integer((mean(threeto5no$MonthlyIncome)))
fiveto9no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "5 to 9")
fiveto9no_mean <- as.integer((mean(fiveto9no$MonthlyIncome)))
nineplusno <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "9+")
nineplusno_mean <- as.integer((mean(nineplusno$MonthlyIncome)))

# Tack a column named MeanMonthly with a 1 to hold a numeric space
addblank <- rep.int(1,1470)
rawdata <- cbind(rawdata, addblank)
names(rawdata[,39]) <- "MeanMonthly"

# Write the appropriate monthly mean value into the MonthlyMean column
for (i in 1:1470) {
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "0 to 3") {rawdata$MeanMonthly[i] <- zto3yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "3 to 5") {rawdata$MeanMonthly[i] <- threeto5yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "5 to 9") {rawdata$MeanMonthly[i] <- fiveto9yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "9+") {rawdata$MeanMonthly[i] <- nineplusyes_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "0 to 3") {rawdata$MeanMonthly[i] <- zto3no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "3 to 5") {rawdata$MeanMonthly[i] <- threeto5no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "5 to 9") {rawdata$MeanMonthly[i] <- fiveto9no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "9+") {rawdata$MeanMonthly[i] <- nineplusno_mean}
}

# Group Attrition, YearsAtCompanycut, and MeanMonthly 
yacmonthly <- group_by(rawdata, Attrition, YearsAtCompanycut, MeanMonthly)
       


########################################################
#                                                      #
# Create all the charst needed for analysis            #
# Not all charts will be used in the final report      #
#                                                      #
########################################################

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

  # GendervsAttrition Summary
  ggplot(rawdata,aes(Gender,fill=Attrition)) + geom_bar(position="dodge")
  ggplot(rawdata,aes(Gender, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

  # hourlyRateAttritionSummary Summary

  ggplot(rawdata,aes(HourlyRate,fill=Attrition)) + geom_bar(position="dodge")
  ggplot(rawdata,aes(HourlyRate, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5,check_overlap = TRUE)+
    labs(y="Percent",fill="HourlyRate")+
    scale_y_continuous(labels=scales::percent)+
    facet_grid(~Attrition)
  
# jobInvolvementAttrition summary

  ggplot(rawdata,aes(JobInvolvement,fill=Attrition)) + geom_bar(position="dodge")
  rawdata$JobInvolvement <- factor(rawdata$JobInvolvement,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
  ggplot(rawdata,aes(JobInvolvement, group=Attrition,fill=JobInvolvement))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

  # joblevelAttrition Summary
  ggplot(rawdata,aes(JobLevel,fill=Attrition)) + geom_bar(position="dodge")
  ggplot(rawdata,aes(JobLevel, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

  #jobRoleAttrition Summary
  ggplot(rawdata,aes(JobRole,fill=Attrition)) + geom_bar(position="dodge")
  ggplot(rawdata,aes(JobRole, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

  #jobsatisfactionAttrition Summary
  ggplot(rawdata,aes(JobSatisfaction,fill=Attrition)) + geom_bar(position="dodge")
  rawdata$JobSatisfaction <- factor(rawdata$JobSatisfaction,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
  ggplot(rawdata,aes(JobSatisfaction, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent",fill="JobSatisfaction")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

  #maritalStatusAttrition Summary
  ggplot(rawdata,aes(MaritalStatus,fill=Attrition)) + geom_bar(position="dodge")
  ggplot(rawdata,aes(MaritalStatus, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
    facet_grid(~Attrition)

#monthlyRateAttrition Summary

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

# MonthlyRate Attrition Boxplot
ggplot(rawdata, aes(Attrition, MonthlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")
# HourlyRate Attrition Boxplot
ggplot(rawdata, aes(Attrition, HourlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")
# MonthlyIncome Attrition Boxplot
ggplot(rawdata, aes(Attrition, MonthlyIncome)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")

#IncomeGroup vs Attrition Summary
ggplot(rawdata,aes(IncomeGroup, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)


# numberCompaniesWorkedAttrition Summary
ggplot(rawdata,aes(NumCompaniesWorked,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(NumCompaniesWorked, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
  facet_grid(~Attrition)

# Age18Attrition Summary
ggplot(rawdata,aes(Over18,fill=Attrition)) + geom_bar(position="dodge")
ggplot(rawdata,aes(Over18, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Over18")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Overtime plot
ggplot(rawdata,aes(OverTime, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Overtime")+
  theme(axis.text.x = element_text(angle=0),
        legend.position="none")+
   scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Percent Salary Hike Plot
ggplot(rawdata,aes(PercentSalaryHike, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="% Salary Hike")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Performance Rating Plot
ggplot(rawdata,aes(PerformanceRating, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Perf Rating")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Relationship Satisfaction Plot
ggplot(rawdata,aes(RelationshipSatisfaction, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Rel Sat")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Standard Hours Plot
ggplot(rawdata,aes(StandardHours, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Standard Hours")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Stock Option Level Plot
ggplot(rawdata,aes(StockOptionLevel, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Stock Level")+
  theme(axis.text.x = element_text(angle=0),
        legend.position="none")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Total working years plot
ggplot(rawdata,aes(TotalWorkingYears, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Working Years")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Training time last year plot
ggplot(rawdata,aes(TrainingTimesLastYear, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Training Time")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Work Life Balance plot
ggplot(rawdata,aes(WorkLifeBalance, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Balance")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Years at Company Plot
ggplot(rawdata,aes(YearsAtCompany, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5,check_overlap = TRUE)+
  labs(y="Percent",fill="Years")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Years in current role Plot
ggplot(rawdata,aes(YearsInCurrentRole, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Years")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Years Since last Promotion Plot
ggplot(rawdata,aes(YearsSinceLastPromotion, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Years")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Years With Current Mananger Plot
ggplot(rawdata,aes(YearsWithCurrManager, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Years")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

ggplot(data=rawdata,aes(Age,mean(YearsAtCompany)))+
  geom_histogram(stat="identity")

# Calculate Number Average years on Job vs Age
ma <- tapply(rawdata$YearsAtCompany, rawdata$Age, mean)
madf<-data.frame(Age=names(ma),YearsWithCompany=ma)
madf$AgeGroup = cut(as.numeric(madf$Age),c(0,12,19,26,43), 
                    labels=c("18-29","30-36","37-43","44-60"))

# Plot Avg Years with Company vs Age
ggplot(madf, aes(x=Age, y=YearsWithCompany, fill = AgeGroup)) +
  geom_bar(stat ="identity",colour="black",position="dodge")+
  labs(fill="Age",y="Avg Years With Company")

# Calculate Avg years on job vs job role
mjobrole <-tapply(rawdata$YearsAtCompany, rawdata$JobRole, mean)
mjobrole.df <-data.frame(JobRole=names(mjobrole),YearsWithCompany=mjobrole)

ggplot(mjobrole.df, aes(x=JobRole, y=YearsWithCompany,fill=JobRole)) +
  geom_bar(stat ="identity",colour="black",position="dodge")+
  labs(fill="Age",y="Avg Years With Company",x="Job Role")+
  theme(axis.text.x = element_text(angle=60, hjust=1),
        legend.position="none")+
#  coord_polar("y",start=0)


boxplot(quantile(cleandata$Age),col="red", horizontal = TRUE,xlab="Age")

ggplot(rawdata,aes(IncomeGroup, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Income Group")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)

# Create graph of Attrition vs MonthlyIncome grouped by Years at Company
ggplot(yacmonthly,aes(x = YearsAtCompanycut, y = MeanMonthly)) + 
  xlab("Years at Company") + 
  ylab("Average Monthly Income") +
  geom_bar(aes(fill = Attrition), stat = "identity", position = position_dodge(NULL)) +
  geom_text(aes(label = MeanMonthly, group=Attrition), position = position_dodge(.9),vjust = -.5, size=5) +
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.x = element_text(face = "bold"))

# Code for cross-comparisons with jobsatisfaction, environmentsatisfaction, worklifebalance, relationshipsatisfaction
RelEnvSat <- lapply(rawdata, function(x){aggregate(EnvironmentSatisfaction~x, data=rawdata, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelWLB <- lapply(rawdata, function(x){aggregate(WorkLifeBalance~x, data=rawdata, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelJobSat <- lapply(rawdata, function(x){aggregate(JobSatisfaction~x, data=rawdata, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelRelSat <- lapply(rawdata, function(x){aggregate(RelationshipSatisfaction~x, data=rawdata, FUN=function(x) c(mean=mean(x), count=length(x)))})