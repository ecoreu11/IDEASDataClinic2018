#### Author: Nikki Solano
#### Date: May 14-16, 2018
#### Purpose: Introduction to R: IDEAS Data Clinic
#### Instructors: John Drake and Ana Bento


#### Topics covered ####

#### 1. Data Visualization
#### 2. Scientific Programming
#### 3. Wrangling 
#### 4. Modeling 
#### 5. Project Management 

################### 1. DATA VISUALIZATION ###################

#load data
setwd("~/./mers")
mers <- read.csv("cases.csv")
head(mers)
mers$hospitalized[890] <- c("2015-02-20")
mers$mers[-471,]
mers <- mers[-471,]

#install.packages("lubridate")
library(lubridate)
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized) #5 failed to parse
class(mers$onset2)


day0 <- min(na.omit(mers$onset2))
mers$epi.day <- as.numeric(mers$onset2 - day0)

### PLOT ###
library(ggplot2)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day , fill=country)) + 
  labs(x="Epidemic day", y="Case count", 
       title="Global count of Mers cases by date of symptom onset",
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

### Univariate plots ###
mers$infectious.period <- mers$hospitalized2 - mers$onset2 #calculate "raw" infectious period
class(mers$infectious.period) #these data are class "difftime"
mers$infectious.period <- as.numeric(mers$infectious.period, units="days") #convert to days
ggplot(data=mers)+
  geom_histogram(aes(x=infectious.period)) +
  labs(x="Infectious period" , y="Frequency", title = "Distribution of calculated MERS infectious period",
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#have negative values, so let's remove those and only keep zeros and positive values

mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) + 
  geom_histogram(aes(x=infectious.period2)) +
  labs(x="Infectious period", y="Frequency", 
       title="Distribution of calculated MERS infectious period (positive values only)",
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x="Infectious period", y="Frequency",
       title="Probability density for MERS infectious period (positive values only)",
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##area plot

ggplot(data=mers) +
  geom_area(stat="bin", mapping=aes(x=infectious.period2)) +
  labs(x="Infectious period", y="Frequency",
       title="Area plot for MERS infectious period (positive values only)",
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

### Bivariate plots ###

#smooth fit overall 
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  geom_smooth(mapping = aes(x=epi.day, y=infectious.period2), method ="loess") +
  labs(x="Epidemic day", y="Infectious period", 
       title="Bar plot looking at the change in the infectious period over the course of the MERS epidemic",
       caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

##smooth fit per country
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  geom_smooth(mapping = aes(x=epi.day, y=infectious.period2, fill=country), method ="loess") +
  labs(x="Epidemic day", y="Infectious period", 
       title="Bar plot looking at the change in the infectious period over the course of the MERS epidemic",
       caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

## Faceting 

#plot by country
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country))+
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x="Epidemic day", y="Infectious period",
       title="MERS infectious period (positive values only) over time",
       caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

## plot by gender and country
ggplot(data=subset(mers, gender %in% c("M", "F") & country %in% c("KSA", "Oman", "Iran", "Jordan", "Qatar", "South Korea", "UAE")), 
       mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country))+
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x="Epidemic day", y="Infectious period",
       title="MERS infectious period by gender and country",
       caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

### Variation in case fatality 

fatality <- data.frame(cbind(mers$outcome, mers$clinical, mers$death)) #create new data frame
colnames(fatality) <- c("outcome", "clinical", "death")

fat2<-fatt[fatt$Clinical=="fatal" & fatt$Outcome=="fatal",] #create df with only fatality included

################### 2. SCIENTIFIC PROGRAMMING ###################


#load data 
WNV <- read.csv("https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv")

# histogram with total number of cases per year per state
ggplot(data=WNV) +
  geom_histogram(mapping = aes(x=Total))


#log of case level state burden

#creating a new column that has calculated the log of the total
WNV$LogTotal <- log(WNV$Total)

ggplot(data=WNV) +
  geom_histogram(mapping = aes(x=LogTotal)) 

#simply asking it to plot the log of the existing "Total" column
ggplot(data=WNV) +
  geom_histogram(mapping = aes(x=log(Total)))


# calculating the raw case fatality rate
WNV$CFR <- WNV$Fatal / WNV$Total

#histogram 
ggplot(data=WNV) +
  geom_histogram(mapping = aes(x=CFR))

# check to see if "Total" column is actually 
# the sum of the febrile, neuroinvasive cases, and other cases
WNV$SUMMMM <- rowSums(WNV[,3:5])


##Could also use an ifelse statement to practice 
WNV$Check <- ifelse((WNV$SUMMMM==WNV$Total), TRUE, FALSE)


### FUNCTIONS

WNV$EncephRate <- WNV$EncephMen / WNV$Total


#function for mean
mean <- function(x){
  s <-sum(x)
  n <-length(x)
  m <- s/n
  return(m)  
}

#function for standard error 
stderror <- function(x){
  e <-sd(x)/sqrt(x)
  
  return(e)
}

### pull out New York cases to calculate mean and stder for them 

NY <- subset(WNV, State == "New York")

mean(NY$EncephRate)
stderror(NY$EncephRate)

#Colorado
CO <- subset(WNV, State == "Colorado")

mean(CO$EncephRate)
stderror(CO$EncephRate)

#California
CA <- subset(WNV, State == "California")

mean(CA$EncephRate)
stderror(CA$EncephRate)

##### LOOPS

year <- subset(WNV, Year == "2003")

for (year in c())
  

################### 3. WRANGLING ###################
## Date: May 15th, 2018 

#load packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

#read in data as tibble 
ld <- read_csv("lyme.csv")
pop <- read_csv("pop.csv")
prism <- read_csv("climate.csv")


## the 'pop' data doesn't conform with the tidy format given how it has NAs

#look within the fips column, and select columns that begin with 'pop2', then make that the new pop
pop %<>% select (fips,starts_with("pop2"))

#for those that begin with "pop2", also gather the year and size associated with each, 
#also omit NAs, then make that the new pop
pop %<>% gather(starts_with("pop2"), key="str_year", value="size") %>% na.omit

#make a new column called "year" and that will be the column 'str_year' without the 'pop' at the beginning 
pop %<>% mutate(year=str_replace_all(str_year, "pop", ""))

#make the 'year' column values integers
pop %<>% mutate(year=as.integer(year))

#within the 'fips' column, for the fips that beging with a 0 replace all of those with a space 
# in other words, remove the 0 at the beginning of the fips code
pop %<>% mutate(fips=str_replace_all(fips, "^0", ""))

# make the values in the fips column integers
pop %<>% mutate(fips=as.integer(fips))


### One way that we can remove the state summary information is by
### setting fips back to a character and asking R to remove those values 
### that end in zero. Example:
### pop %<>% mutate(fips=str_replace_all(fips, "$0", ""))


##clean up lyme disease data

# gather the values that begin with 'Cases' in the str_year column
ld %<>% gather(starts_with("Cases"), key="str_year", value="cases")

#remove 'cases' from the column str_year, and create a new column 'year'with the remaining characters
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))

#make the characters in the new year column an integer
ld %<>% mutate(year=as.integer(year))



###use a function to create fips code and add it to the newly cleaned lyme disease tibble
fips.new <- function(st,ct){
  if (str_length(ct)==3){ 
    fips <- paste(as.character(st), as.character(ct), sep ="") %>% as.integer
  } #if the county numbers' length is 3, paste the state and seperate with nothing, then make all of that an integer
  else if (str_length(ct)==2){
    fips <-paste(as.character(st), "0", as.character(ct), sep ="") %>% as.integer
  } #if the county length is 2, add a zero to the state and paste the county. Don't seperate state and county and make it an integer
  else {
    fips <-paste(as.character(st), "00", as.character(ct), sep="") %>% as.integer
  } #if the county length is neither 3 nor 2, add two zeros to the state and paste the county. Do not add anything between county and state
  return(fips) #now show me the fips codes i've created 
}

#create a new column called 'fips'. it will include the new column I made ('fips.new') but only use
#match this per row into the lyme disease data
ld %<>% rowwise() %<>% mutate(fips=fips.new(STCODE,CTYCODE))

#remove the state code, county code, and str_year from the lyme disease data
ld %<>% select(-c(STCODE,CTYCODE,str_year))

#rename the columns "STNAME" and "CTYNAME" to "state" and "county" respectively
ld %<>% rename(state=STNAME)
ld %<>% rename(county=CTYNAME)


### combining lyme disease and prism 

#only keep the rows where we have climate data 
ld.prism <- inner_join(ld,prism)
# 
# ## add demographic data from 'pop' (population size) to 'tog'
tog <- inner_join(ld.prism,pop)
# 
# #remove the column str_year from 'ld.prism' because we only want population size
tog %>% select(-str_year)
# 
# ## save data as csv
write_csv(tog, "ld.prism.csv")

## Obtaining summary information
ld_per_year <- ld %>% ungroup %>% group_by(year) %>% 
  summarize(total=sum(cases)) %>% arrange(desc(total))

################### 4. MODELING ###################

#load packages
library(tidyverse)
library(magrittr)
library(GGally)

#load data
ld.prism <- read_csv("ld.prism.csv")

#create 4x4 summary plot of precipitation, average temperature,
# population size, and lyme disease cases
ggpairs(ld.prism, columns = c("prcp", "avtemp", "size", "cases"))

#use log since density plots are too clumped
ld.prism %<>% rowwise %>% 
  mutate(logSize=log(size), logCases=log(cases+1)) 
##we had to add one to cases because you cannot take the log of zero

#new summary plot using log cases and log sizes
ggpairs(ld.prism, columns = c("prcp", "avtemp", "logSize", "logCases"))


## fun fact
## could create a new data frame with column in a new arrangement 
##ld.prism2 <- ld.prism[,c(1,5,4,6,7,3,8,9,2,10)]

#random sample of ld.prism with 100 rows 
set.seed(222); random <- ld.prism %>% sample_n(100)

#make a scatter plot of precipitation and average temperature using random data frame 
#and adda line through the plot
myPlot <- ggplot(data=random) +
  geom_point(mapping = aes(x=random$prcp, y=random$avtemp))+
  geom_smooth(mapping = aes(x=random$prcp, y=random$avtemp), method = "lm")+
  labs(x= "precipitation", y="average temperature", title= "Precipitation vs. Avg temp")
myPlot

#create a linear model  and store it 
lm.model <- lm(random$avtemp ~ random$prcp, data=random)

#look at the summary for the model
summary(lm.model)

#try looking at the slope 
slope <-summary(lm.model)$coefficients[2,1]

#now try looking at the p-value
p.value <- summary(lm.model)$coefficients[2,4]

## The slope of the line in myPlot is 0.002 and it is not significantly 
#different from 0

#### modelr package

## create a ggplot of total population size by year 
ld.prism$year <- as.numeric(ld.prism$year)

ld.prism %>% ungroup %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+ geom_point(aes(x=year,y=total))


## Grouped data frame vs. nested data frames

#create a dataset grouped by state
by_state <- ld.prism %>% group_by(state) 

#create a nested dateset, where each state has the data associated with it
#in the data column
by_state %<>% nest
by_state

#check out GA's tibble 
by_state$data[[10]]

## create a function that takes the dataframe as the argument and returns a linear model
linGrowth_model <- function(df){
  lm(size ~ year, data=df)
}

#add models to its appropriate state/data
models <- purrr::map(by_state$data, linGrowth_model)

#add column in by_state where each state has it's own model 
by_state %<>% mutate(model=map(data,linGrowth_model))

#store residuals for each model and add that to the appropriate state/data/model
library(modelr)
by_state %<>% mutate(resids=map2(data, model, add_residuals))
## resdiuals are lists (in tibble format)

#create a function that calculates the sum of absolute value of the residuals within a 
#dataframe. Then add this to the by_state dataframe 
sumresid <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid=map(resids,sumresid))

## create a function that calculates the slope for every model and add this as a column to by_state
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope=map(model,get_slope))

#un-nest the data we have included in by_state in order to be able to visualize it 
slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResids)


