#Setting the working directory
setwd("C:/Users/muska/Downloads/R - Retail Case study (1)/R case study 1 (Retail)")

#Importing the data set
cust <- read.csv("C:/Users/muska/Downloads/R - Retail Case study (1)/R case study 1 (Retail)/Customer.csv")
prod <- read.csv("C:/Users/muska/Downloads/R - Retail Case study (1)/R case study 1 (Retail)/prod_cat_info.csv")
trans <- read.csv("C:/Users/muska/Downloads/R - Retail Case study (1)/R case study 1 (Retail)/Transactions.csv")

#Importing Libraries
library(dplyr)
library(ggplot2)
library(data.table)
library(plotrix)
library(stringr)
require(stringr)

#Data Exploration
str(cust)
str(prod)
str(trans)

#Checking Null values
colSums(is.na(cust))
colSums(is.na(prod))
colSums(is.na(trans))

#1. Merge the datasets:
#a. Use the base merge()

trans_cust <- merge(x=trans, y=cust, by.x='cust_id', by.y='customer_Id', all.x=FALSE)
Customer_Final <- merge(x=trans_cust, y=prod, by.x=c('prod_cat_code','prod_subcat_code'), by.y=c('prod_cat_code','prod_sub_cat_code'), all.x=FALSE)
View(Customer_Final)

#b. using dplyr() func
trans_cust1<-dplyr::left_join(trans,cust,by=c("cust_id" = "customer_Id"))
Customer_Final1<-dplyr::left_join(trans_cust1,prod,by=c('prod_cat_code'='prod_cat_code','prod_subcat_code'='prod_sub_cat_code'))

#Converting dates into proper format
Customer_Final$DOB<- as.Date(Customer_Final$DOB, format='%d-%m-%Y')
Customer_Final$tran_date<-str_replace_all(Customer_Final$tran_date,'/','-')
Customer_Final$tran_date<-as.Date(Customer_Final$tran_date, format='%d-%m-%Y')

#2. Prepare summary report:
#a. Get column names and corresponding data types
View(data.frame(sapply(Customer_Final,class)))
#b. Top/botoom 10 observations
head(Customer_Final,10)
tail(Customer_Final,10)
#c. "Five Number summary"
summary(Customer_Final)
#d. Frequency table for all categorical columns
Store_type<-Customer_Final %>% group_by(Store_type)%>% count(Store_type)
Gender<-Customer_Final %>% group_by(Gender)%>% count(Gender)
ProdCat<-Customer_Final %>% group_by(prod_cat)%>% count(prod_cat)
ProdSubcat<-Customer_Final %>% group_by(prod_subcat)%>% count(prod_subcat)

#3. Create histograms for all continous and categorical variables
ggplot(Store_type) + geom_bar(aes(x=Store_type, y=n,fill=factor(n)),stat="identity")
ggplot(Gender) + geom_bar(aes(x=Gender, y=n,fill=factor(n)),stat="identity")
ggplot(ProdCat) + geom_bar(aes(x=prod_cat, y=n,fill=factor(n)),stat="identity")
ggplot(ProdSubcat) + geom_bar(aes(x=prod_subcat, y=n,fill=factor(n)),stat="identity")

hist(Customer_Final$Rate, breaks=40, col="violet", xlab="Rate", main="")
hist(Customer_Final$Qty, breaks=5, col="red", xlab="Qty", main="")
hist(Customer_Final$Tax, breaks=20, col="blue", xlab="Tax", main="")
hist(Customer_Final$total_amt, breaks=10, col="yellow", xlab="Total amount", main="")
hist(Customer_Final$city_code, breaks=10, col="orange", xlab="City code", main="")

#4. Calculate the following information using the merged dataset :
#a. Time period of the available transaction data
max(Customer_Final$tran_date)-min(Customer_Final$tran_date)

#b. Count of transactions where the total amount of transaction was negative
length(which(Customer_Final$total_amt<0))

#5. Analyze which product categories are more popular among females vs male customers.
Gender_Prod<-Customer_Final %>% group_by(Gender,prod_cat)%>% count(Gender)

#6. Which City code has the maximum customers and what was the percentage of customers from that city?
Citycode<-Customer_Final %>% group_by(city_code)%>% count(city_code)%>%arrange(desc(n))
#City code with max customers:
Topcity<-head(Citycode,1)
#Percentage:
Topcity$n/(sum(Citycode$n))*100

#7. Which store type sells the maximum products by value and by quantity?
#Maximum product by quantity
head(Customer_Final %>% group_by(Store_type)%>% count(Store_type)%>%arrange(desc(n)),1)
#Maximum product by value
head(Customer_Final %>% group_by(Store_type)%>% summarise(Amount=sum(total_amt))%>%arrange(desc(Amount)),1)

#8. What was the total amount earned from the "Electronics" and "Clothing" categories from Flagship Stores?
Customer_Final %>% filter(Customer_Final$Store_type=="Flagship store" & (Customer_Final$prod_cat=="Electronics"|Customer_Final$prod_cat=="Clothing"))%>% group_by(prod_cat)%>%summarise(Amount=sum(total_amt))

#9. What was the total amount earned from "Male" customers under the "Electronics" category?
Customer_Final %>% filter(Customer_Final$prod_cat=="Electronics"& Customer_Final$Gender=="M")%>% group_by(prod_cat)%>%summarise(Amount=sum(total_amt))

#10. How many customers have more than 10 unique transactions, after removing all transactions which have any negative amounts?
Positive_trans<-Customer_Final[Customer_Final$Qty>0,]
Cust_trans<-Positive_trans%>% group_by(cust_id)%>% count(cust_id)
Cust_trans[Cust_trans$n>10,]

#11. For all customers aged between 25 - 35, find out:
#a. What was the total amount spent for "Electronics" and "Books" product categories?
Customer_Final$Age <- as.integer(Sys.Date() - Customer_Final$DOB)/365
Cust_age<-Customer_Final[(Customer_Final$Age>25) & (Customer_Final$Age<35),]
Cust_age%>% filter(Cust_age$prod_cat=="Electronics"| Cust_age$prod_cat=="Books")%>% group_by(prod_cat)%>%summarise(Amount=sum(total_amt))

#b. What was the total amount spent by these customers between 1st Jan, 2014 to 1st Mar, 2014?
Cust_age%>% filter(Cust_age$tran_date>"2014-01-01"& Cust_age$tran_date<"2014-03-01")%>%summarise(Amount=sum(total_amt))



