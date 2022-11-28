#install.packages("readr")
library(readr)
#vgsales <- read_csv("/Users/akovbasiuk/Documents/stats for ml/EDA - 02/Univariate/vgsales.csv")

vgsales <- read_csv("vgsales.csv") #not working untill you will have
#wprking direstory set
#to do this go to session -> Working Directory -> To source file location

setwd("/Users/akovbasiuk/Documents/Documents_Anna_MacBook_Pro/Classes/stats for ml/EDA - 02/Univariate")

vgsales <- read_csv("vgsales.csv")

#to see your working directory in the Files (on the right)
#Go to Working Directory

#indexing $
vgsales$Global_Sales


dim(vgsales)
str(vgsales)

#Managing Datasets using dplyr package
install.packages("dplyr")
library(dplyr)
glimpse(vgsales)


#Action VG, 2009, Global_Sales highest, total sales from NA and EU

#Conduct the analysis only for the top sales more than 1million


vgsales <- vgsales %>%
  mutate(sum_NA_EU = NA_Sales+EU_Sales)

vgsales$sum_NA_EU
max(vgsales$Global_Sales)

global_sales_top <- vgsales %>% 
  filter(Global_Sales > 1)

min(global_sales_top$Global_Sales)


global_sales_top_2009 <- vgsales %>% 
  filter(Year == "2009")

global_sales_top_2009_action <- vgsales %>% 
  filter(Global_Sales > 1 & Year == "2009" & Genre == "Action") %>%
  arrange(desc(Global_Sales))


head(global_sales_top_2009_action)




#functions same names in  different packages

vgsales %>% dplyr :: select(Genre, Global_Sales) #if you have function 
#named the same in different packages


vgsales_new_col_name <- vgsales %>%
  rename ("total_Europe_America_Sales"="sum_NA_EU")

#names(vgsales) <- make.names(names(vgsales))

vgsales_new_col_name_1 <- vgsales %>% 
  select(col1 = 1, everything() )

#Univariate analysis - analyse one variable

summary(vgsales)

#CATEGORICAL
summary(vgsales$Genre) #not working because its character

#we need to change to factor
vgsales$Genre <- factor(vgsales$Genre)

summary(vgsales$Genre)

round(100*prop.table(table(vgsales$Genre)))

table(vgsales$Genre) %>%
  barplot(las = 2)
#las argument perpendicular to axis orientation = 2 (0, 1, 2, 3)


#Aggregating data in dplyr
vgsales %>%
  count(Genre) %>%
  arrange(desc(n))


vgsales %>%
  group_by(Genre) %>%
  summarise(Counts = n()) %>%
  mutate(Freq = Counts/sum(Counts)) %>%
  arrange (desc(Counts))
  


colnames(vgsales)

vgsales_new <- vgsales %>%
  rename(global_new = 11)

colnames(vgsales_new)





#Univariate Exploratory Data Analysis
summary(vgsales) #you do not see different genres summarized
#example for categorical variable how to explore
factor_my <- c("Genre", "Publisher")
vgsales[ c("Genre", "Publisher")] <- lapply(vgsales[ c("Genre", "Publisher")], factor)

vgsales$Genre <- factor(vgsales$Genre)


summary(vgsales)
table(vgsales$Genre)
prop.table(table(vgsales$Genre))



#continuous variable summary
mean(vgsales$Global_Sales)
sd(vgsales$Global_Sales)
summary(vgsales$Global_Sales)

#if mean is lower than median we have more low results
#if mean is closer to min -> more low values in the data (positive, right-skewed)
#IQR interpretation:
#25% (1 / 4) of the vg sales is below 0.06 
#25% (1/4) of the highest sales were 0.47 and more



#install.packages("psych")

library(psych)

psych :: describe(vgsales$Global_Sales) # :: this means take describe function from the 
#package psych

describe(vgsales)

#trimmed / truncated mean - we cut .1 (10%) of the low and high points and calculate mean for them
#if it will differ from mean with those points -> we have outliers in the data
#robust measure of central tendency - less sensitive for outliers 
#mad - Median Absolute Deviation (SD for the median)
#Skewness and Kurtosis - should be close to 0 -> normal distribution
#Kurtosis - measure of how heavy the tails of the distribution are
#SEM - how precise the ESTIMATION of the mean of the sample is based on the true 
#mean of the population

library(dplyr)
vgsales %>% 
  count(Global_Sales, sort = TRUE)


#mode is the most frequent value 0.02 
hist(vgsales$Global_Sales)
rug(vgsales$Global_Sales) #show actual data points


hist(vgsales$Global_Sales, xlim = c(0,5), breaks = 100)
rug(vgsales$Global_Sales) 

#boxplot
boxplot(vgsales$Global_Sales)
q1 <- quantile (vgsales$Global_Sales, 0.25)
q1
q3 <- quantile(vgsales$Global_Sales, 0.75)
q3
iqr <- q3-q1
iqr


#cutoffs for outliers q1-1.5*iqr; q3 + 1.5*iqr
lower <- q1 - 1.5 * iqr
lower
upper <- q3+1.5*iqr
upper


global_sales_new <- vgsales %>%
  filter (Global_Sales < upper & Global_Sales > lower)

#in the new dataset it will leave those which are lower than 
#1.085 and higher than -0.555 


boxplot(global_sales_new$Global_Sales)


#using the test
install.packages("outliers")
library(outliers)

outlier_scores <- scores(vgsales$Global_Sales)


#vgsales[outlier_scores > 3 | outlier_scores < -3, "Global_Sales"] <- NA

vgsales$Global_Sales

#checking distribution
library(fitdistrplus)
par(mar = c(1,1,1,1))
descdist(vgsales$Global_Sales, discrete = FALSE)
#gamma distribution - skewed

#normality test
ks.test(vgsales$Global_Sales, "pnorm", mean = mean(vgsales$Global_Sales), sd = sd(vgsales$Global_Sales))
#p < 0.05 - distribution is different from normal

#how many samples we want to generate 5000
shapiro.test(vgsales$Global_Sales[0:5000])


#Conduct analysis for sum_NA_EU_Sales and Platform
#Outliers step can be skipped

library(dplyr)
vgsales %>%
  group_by(Platform)%>%
  summarise(n = sum(sum_NA_EU))%>%
  arrange(desc(n))


boxplot(vgsales$sum_NA_EU ~ vgsales$Platform)

install.packages("beanplot", type = "source")
library('beanplot')


install.packages("lattice", type = "source")
library("lattice")

xyplot(NA_Sales~EU_Sales | Platform, data = vgsales)


mean(vgsales$sum_NA_EU)
median(vgsales$sum_NA_EU)
sort(-table(vgsales$sum_NA_EU))[1]

# sum_Na_EU - the mean is 0.411 ant the median is 0.12 and mode is 0 median has property that is less than mean so the data is skewed right distributed

