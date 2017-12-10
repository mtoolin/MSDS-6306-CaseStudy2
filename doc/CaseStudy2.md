# Case Study2
Michalel Toolin, JaimeVillanueva,Venkat Kasarla  
11/23/2017  




```r
rawdata.xls <- '../data/CaseStudy2-data.xlsx'
rawdata.csv <- '../data/original.csv'
refine <- read_excel(rawdata.xls,1)
refine %>% data.table::fwrite(rawdata.csv)
dframe <- read.csv(rawdata.csv)
rawdata <- dframe
```

<br>


```r
ggplot(rawdata,aes(Gender,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/genderAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(Gender, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/genderAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(HourlyRate,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/hourlyRateAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(HourlyRate, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5,check_overlap = TRUE)+
  labs(y="Percent",fill="HourlyRate")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/hourlyRateAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(JobInvolvement,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/jobInvolvementAttritionsummary-1.png)<!-- -->

```r
rawdata$JobInvolvement <- factor(rawdata$JobInvolvement,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
ggplot(rawdata,aes(JobInvolvement, group=Attrition,fill=JobInvolvement))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/jobInvolvementAttritionsummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(JobLevel,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/joblevelAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(JobLevel, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/joblevelAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(JobRole,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/jobRoleAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(JobRole, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/jobRoleAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(JobSatisfaction,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/jobsatisfactionAttritionSummary-1.png)<!-- -->

```r
rawdata$JobSatisfaction <- factor(rawdata$JobSatisfaction,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
ggplot(rawdata,aes(JobSatisfaction, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobSatisfaction")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/jobsatisfactionAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(MaritalStatus,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/maritalStatusAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(MaritalStatus, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/maritalStatusAttritionSummary-2.png)<!-- -->

<br>


```r
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(rawdata, aes(Attrition, MonthlyRate)) + stat_summary(fun.data = f, geom="boxplot")
```

![](CaseStudy2_files/figure-html/monthlyRateAttrition-1.png)<!-- -->

```r
# define outlier as you want 0-25%  and 75-100% outliers   
o <- function(x) {
  subset(x, x < quantile(x)[2] | quantile(x)[4] < x)
}

#Monthly Rate
ggplot(rawdata, aes(Attrition, MonthlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")
```

![](CaseStudy2_files/figure-html/monthlyRateAttrition-2.png)<!-- -->

```r
#Hourly Rate
ggplot(rawdata, aes(Attrition, HourlyRate)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")
```

![](CaseStudy2_files/figure-html/monthlyRateAttrition-3.png)<!-- -->

<br>


```r
#Monthly Income
ggplot(rawdata, aes(Attrition, MonthlyIncome)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point")
```

![](CaseStudy2_files/figure-html/monthlyIncomeAttritionSummary-1.png)<!-- -->

```r
quantie.val <- quantile(rawdata$MonthlyIncome)
rawdata$IncomeGroup[rawdata$MonthlyIncome < quantie.val[1]] <- "Low"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[1] & rawdata$MonthlyIncome < quantie.val[2]] <- "Low Medium"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[2] & rawdata$MonthlyIncome < quantie.val[3]] <- "Medium"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[3] & rawdata$MonthlyIncome < quantie.val[4]] <- "Medium High"
rawdata$IncomeGroup[rawdata$MonthlyIncome >= quantie.val[4] & rawdata$MonthlyIncome < quantie.val[5]] <- "High"

ggplot(rawdata,aes(IncomeGroup, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/monthlyIncomeAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(NumCompaniesWorked,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/numberCompaniesWorkedAttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(NumCompaniesWorked, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
   theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/numberCompaniesWorkedAttritionSummary-2.png)<!-- -->

<br>


```r
ggplot(rawdata,aes(Over18,fill=Attrition)) + geom_bar(position="dodge")
```

![](CaseStudy2_files/figure-html/Age18AttritionSummary-1.png)<!-- -->

```r
ggplot(rawdata,aes(Over18, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Over18")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/Age18AttritionSummary-2.png)<!-- -->

<br>


