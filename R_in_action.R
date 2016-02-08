## Chapter 4th 基础数据管理

setwd("E:\\liuxianren\\Rdata")


#4.1 创建leadership数据框

manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <- c("US","US","UK","UK","UK")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2)
q5 <- c(5,5,2,NA,1)
leadership <-
        data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors = FALSE)


#4.2 创建新变量

mydata <- data.frame(x1 = c(2,2,6,4), x2 = c(3,4,2,8))

#方法一：
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2) / 2

#方法二：(推荐写法)

mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2) / 2)

str(mydata)

#4.3 变量的重编码：
leadership$age [leadership$age == 99] <- NA


leadership <- within(leadership, {
        agecat <- NA
        agecat [age > 75] <- "Elder"
        agecat [age >= 55 & age <= 75] <- "Middle Aged"
        agecat [age < 55] <- "Yong"
})

head(leadership,10)

#4.4 变量重命名

fix(leadership)

install.packages('reshape')

library(reshape)
leadership <- rename(leadership, c(data = 'testDat'))

names(leadership)

#4.5.2 在分析中排除缺失值

newdata <- na.omit(leadership)
leadership

#4.6 日期

####日期格式化
strDates <- c("01/05/1965","08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")

##系统日期
today <- Sys.Date()
date()

format(today, format = "%Y-%m-%d")

#计算时间间隔
dob <- as.Date("1956-10-12")
difftime(today,dob, units = 'weeks')
# 'arg'应当是“auto”, “secs”, “mins”, “hours”, “days”, “weeks”其中的一个


#4.8 数据排序

newdata <- leadership [order(leadership$age),]

newdata

#4.10 数据集的筛选

#4.10.1

leadership[, c(6:10)]

#4.10.3

attach(leadership)
newdata <- leadership[which(gender == 'M' & age > 30),]
detach

str(leadership)


#4.10.4 subset()函数
newdata <- subset(leadership,gender == "M" & age > 25, select = gender:q4)
newdata

#4.11.5 随机抽样

mysample <- leadership[sample(1:nrow(leadership), 3, replace=FALSE),]

#4.11 使用SQL
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg", row.names=TRUE)




##############################################################################

#Chapter 5 高级数据管理

#统计函数

#数据的标准化
newdata <- scale(mydata)



#5.5 正态分布函数
 x <- pretty (c(-3,3),30)
 y <- dnorm(x)
 plot(x,y, type="l", xlab="NormalDeviate", ylab="Density", yaxs="i")
 

###############################################################################
 ## Chapter 6 基本图形
 
 #主要内容
 # 1. 条形图、箱线图、和点图
 # 2. 饼图和扇形图
 # 3. 直方图和核密度图
 
 
 
 #6.1.1 条形图
 
 install.packages('vcd')
 library(vcd)
 
 counts <- table(Arthritis$Improved)
 counts
 
 barplot(counts,
         main="简单条形图",
         xlab = "Improvement",
         ylab = "Frequency"
         )
 
 barplot(counts,
         main="水平条形图",
         ylab = "Improvement",
         xlab = "Frequency",
         horiz= TRUE
 )
 
 #针对因子型的变量，可以直接使用plot来描述
 
 plot(Arthritis$Improved)

 #查看性别间的改善情况
 
 head(Arthritis,20)
 sex_imp <- with(Arthritis, table(Improved,Sex))
 barplot(sex_imp)

 
 #6.1.2 堆砌/分组条形图
 
 library(vcd)
 
 
 attach(Arthritis)
 counts <- table(Improved, Treatment)
 counts
 detach(Arthritis)
 
 ##上式等价于以下表示方式
 counts <- with(Arthritis, table(Improved, Treatment))
 

 barplot(counts,
         main="分组条形图",
         xlab="Improvement",
         ylab="Frequency",
         col= c("red","yellow","green"),
         legend = rownames(counts),
         beside=TRUE
         )
 
 abline(c(15,15),rownames(counts))
 

