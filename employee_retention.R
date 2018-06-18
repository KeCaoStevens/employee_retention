install.packages("dplyr")
library(dplyr)
install.packages('rpart')
library(rpart)
install.packages("ggplot2")
library('ggplot2')
install.packages("scales")
library(scales)
data=read.csv(file='C:/Users/38933/Desktop/datasc/employee_retention_data.csv', head=TRUE)
str(data)
head(data)
data$company_id=as.factor(data$company_id)#
data$join_date=as.Date(data$join_date) #make it a date 
data$quit_date=as.Date(data$quit_date) #make it a date 
summary(data)

# create list of unique dates for the table 
unique_dates = seq(as.Date('2011/01/24'),as.Date('2015/12/13'),by= 'day')
#create list of unique companies
unique_companies=unique(data$company_id)
#cross join to get all combinations of dates and companies
data_headcount=merge(unique_dates,unique_companies,by=NULL)
colnames(data_headcount)=c('date','company_id')
#count how many people quit/got hired on that day
data_join = data %>%
  group_by(join_date,company_id)%>%
  summarize(join_count=length(join_date))

data_quit = data %>%
  group_by(quit_date, company_id) %>%
  summarise(quit_count = length(quit_date))

#left outer join with data_headcount
#NA means no people were hired or quit on that day cause there is no match
data_headcount = merge(data_headcount,data_join,
                       by.x = c('date','company_id'),
                       by.y = c('join_date','company_id'),
                       all.x=TRUE)
data_headcount = merge(data_headcount, data_quit,
                       by.x = c('date','company_id'),
                       by.y = c('quit_date','company_id'),
                       all.x = TRUE)
#repalce the NAs with 0
data_headcount$join_count[is.na(data_headcount$join_count)]=0
data_headcount$quit_count[is.na(data_headcount$quit_count)]=0

data_headcount= data_headcount %>%
  group_by(company_id) %>%
  mutate(join_cumsum=cumsum(join_count),quit_cumsum=cumsum(quit_count))

data_headcount$count=data_headcount$join_cumsum-data_headcount$quit_cumsum

data_headcount_table=data.frame(data_headcount[,c('date','company_id','count')])

data$employment_length = as.numeric(data$quit_date - data$join_date)
data$week_of_year =  as.numeric(format(data$quit_date,'%U'))

hist(data$employment_length,breaks = 100)
hist(data$week_of_year,breaks=length(unique(data$week_of_year)))
#create binary class
data = subset(data, data$join_date < as.Date("2015/12/13") - (365 + 31)) 
data$early_quitter = as.factor((ifelse(is.na(data$quit_date)| as.numeric(data$quit_date-data$join_date) >396,0,1)))

tree = rpart(early_quitter ~.,data[,c('company_id','dept','seniority','early_quitter','salary')],
             control = rpart.control(minibucket = 30, maxdepth = 3, cp=0.000001),
             parms =list(prior=c(0.5,0.5)))
tree
data$salary_percentile = cut (data$salary,breaks= quantile(data$salary,probs=seq(0,1,0.02)),
                              include.lowest = TRUE,labels = 1:50)
data_proportion_by_percentile = data %>%
  group_by(salary_percentile) %>%
  summarize(proportion_early_quitters = length(early_quitter[early_quitter==1])/length(early_quitter))
qplot(salary_percentile, proportion_early_quitters, data=data_proportion_by_percentile, geom='line', group = 1) + 
        scale_x_discrete(breaks = seq(1,50,by=2))
