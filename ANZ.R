library(chron)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(varhandle)
library(plotly)
library(matrixStats)
library(corrplot)
library(caret)
library(caTools)
library(GGally)
library(Hmisc)
library(PerformanceAnalytics)
library(mltools)
library(data.table)
library(gghighlight)
options(scipen=999)

anz = read.csv("D:/LaTrobe/ANZ/ANZ.csv",na.strings=c(""," ","NA"), header = TRUE)
anz = select(anz, -c(bpay_biller_code, merchant_code))
colSums(is.na(anz))
summary(anz)
anz_auth = subset(anz, status == "authorized")
anz_posted = subset(anz, status == "posted")
anz%>% keep(is.numeric) %>% gather() %>% 
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()+theme_stata()
anz$date = as.character(anz$date)
anz$date = as.Date(anz$date, '%m/%d/%Y')

anz = mutate(anz, month = format(date, "%m"), year = format(date, "%Y"))

avg_daily_tran = aggregate(amount~date, anz, mean)
total_daily_tran = aggregate(amount~date, anz, sum)

avg_mon_tran = aggregate(amount~month, anz, mean)
total_mon_tran = aggregate(amount~month, anz, sum)

avg_daily = ggplot(avg_daily_tran, aes(x = date, y = amount))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  geom_bar(stat = "identity", position =  "identity", width = 1.0,colour=alpha("cornflowerblue", 0.7), fill = alpha("#FF0000", 0.5)) +
  gghighlight(amount>495,unhighlighted_params = list(size = 1,colour = alpha("cornflowerblue", 0.7), fill = alpha("steelblue", 0.5)),label_key = confirmed)+
  theme_stata()+
  labs(title = "Average Transaction Amount",
       subtitle = "Daily Basis",
       x = "Date", y = "Amount in AUD")

total_daily = ggplot(total_daily_tran, aes(x = date, y = amount))+
  geom_bar(stat = "identity", position =  "identity", width = 1.0,colour=alpha("cornflowerblue", 0.7), fill = alpha("#FF0000", 0.5)) +
  gghighlight(amount>51300,unhighlighted_params = list(size = 1,colour = alpha("cornflowerblue", 0.7), fill = alpha("steelblue", 0.5)),label_key = confirmed)+
  theme_stata()+
  labs(title = "Total Transaction Amount",
       subtitle = "Daily Basis",
       x = "Date", y = "Amount in AUD")

avg_mon = ggplot(avg_mon_tran, aes(x = month, y = amount, fill = month))+
  scale_y_continuous(breaks = seq(0, 600, 50))+
  scale_x_discrete(labels = c("August", "September", "October"))+
  geom_bar(stat = "identity", position =  "identity", width = 1.0) +
  #gghighlight(amount>300,unhighlighted_params = list(size = 1,colour = alpha("cornflowerblue", 0.7), fill = alpha("steelblue", 0.5)),label_key = confirmed)+
  theme_stata()+
  scale_fill_brewer(palette= "Blues")+
  labs(title = "Average Transaction Amount",
       subtitle = "Monthly Basis",
       x = "Month", y = "Amount in AUD")

total_mon = ggplot(total_mon_tran, aes(x = month, y = amount, fill = month))+
  scale_x_discrete(labels = c("August", "September", "October"))+
  geom_bar(stat = "identity", position =  "identity", width = 1.0) +
  #gghighlight(amount>300,unhighlighted_params = list(size = 1,colour = alpha("cornflowerblue", 0.7), fill = alpha("steelblue", 0.5)),label_key = confirmed)+
  theme_stata()+
  scale_fill_brewer(palette= "Blues")+
  labs(title = "Total Transaction Amount",
       subtitle = "Monthly Basis",
       x = "Month", y = "Amount in AUD")

fig1 = ggarrange(avg_daily, total_daily, avg_mon, total_mon)
annotate_figure(fig1, top = text_grob("Average Transaction Amount on a Daily and Monthly Basis", color = "Black", size = 14))

names = length(unique(anz$customer_id))
print(names)

salary = subset(anz, txn_description == "PAY/SALARY")
names = length(unique(salary$customer_id))
print(names)

sal_avg_monthly = aggregate(amount~age+gender, salary, mean)

salVage = ggplot(sal_avg_monthly, aes(x = age, y = amount, color= gender))+
  scale_x_continuous(breaks = seq(18, 100, 5))+
  scale_y_continuous(breaks = seq(0, 10000, 500))+
  geom_line(stat = "identity", position =  "dodge", size = 1.0) +
  theme_stata()+
  labs(title = "Average Salary vs Age",
       #subtitle = "Daily Basis",
       x = "Age", y = "Amount in AUD")

earnings = aggregate(amount~first_name+customer_id, salary, mean)
earnings = arrange(earnings, desc(amount))
top10earners = earnings[1:10, 1:3]

topSal = ggplot(top10earners, aes(x = amount, y = reorder(first_name, amount), fill = customer_id))+
  geom_bar(stat = "identity", position =  "identity", width = 1,colour=alpha("steelblue", 0.7)) +
  #gghighlight(amount>51300,unhighlighted_params = list(size = 1,colour = alpha("cornflowerblue", 0.7), fill = alpha("steelblue", 0.5)),label_key = confirmed)+
  theme_stata()+
  scale_fill_brewer(palette= "Set3")+
  labs(title = "Top 10 Earners",
       subtitle = "Average Monthly Salary",
       x = "Amount in AUD", y = "First Name")

typeOfTrans_daily = anz %>%
  group_by(txn_description, date) %>%
  summarise(count=n())

typeOfTrans_monthly = anz %>%
  group_by(txn_description, month) %>%
  summarise(count=n())

tot_daily = ggplot(typeOfTrans_daily, aes(x = date, y = count, color= txn_description))+
  #scale_x_continuous(breaks = seq(18, 100, 5))+
  #scale_y_continuous(breaks = seq(0, 10000, 500))+
  geom_line(stat = "identity", position =  "identity", size = 1.0) +
  theme_stata()+
  scale_color_brewer(palette= "Dark2")+
  labs(title = "Transactions by Type",
       subtitle = "Daily Basis",
       x = "Date", y = "No. of Transactions")
tot_mon = ggplot(typeOfTrans_monthly, aes(x = month, y = reorder(count,txn_description), color= txn_description, fill = txn_description))+
  geom_bar(stat = "identity", position =  "dodge") +
  theme_stata()+
  scale_color_brewer(palette= "Dark2")+
  scale_fill_brewer(palette= "Dark2")


spendings = subset(anz, txn_description !="PAY/SALARY")  
spending_avg_monthly = aggregate(amount~age+gender, spendings, mean)
spending_avg_daily = aggregate(amount~age+gender+date)