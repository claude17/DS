g <-"My First List"
h <-c(25,26,18,39)
j <-matrix(1:10, nrow=5)
k <-c("one","two","three")
mylist <-list(title=g, ages=h, j, k)
mylist[[2]]

item <-list("mango", "apple")
append(item,"orange", after=1)

item <-list("mango", "apple")
Newitem<-item[-1]
Newitem

var1 = readline(prompt = "Enter the value: ")
var2 = readline(prompt = "Enter the number: ")

var2=as.integer(var2)
print(var1)
print(var2)

x = scan()
print(x)

s = scan(what = " ")
print(s)


mydata <-data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
mydata <-edit(mydata)
mydata
write.csv(mydata, file='C:/Users/student/Documents/newfile.csv', row.names=FALSE)

newdata <-read.csv(file="C:/Users/student/Downloads/iris.csv", header = TRUE, sep = ",")
newdata[5:10,1:3]
newdata$variety
subset(newdata, variety=="Virginica")
subset(newdata,petal.length>=6)
install.packages("dplyr")
library(dplyr)

filter(newdata, sepal.width>3)

stats <-data.frame(player=c('A', 'B', 'C', 'D', 'A', 'A'),
                   runs=c(100, 200, 408, 19, 56, 100),
                   wickets=c(17, 20, NA, 5, 2, 17))

distinct(stats)
distinct(stats, player, .keep_all = TRUE)

stats <-data.frame(player=c('A', 'B', 'C', 'D'),
                   runs=c(100, 200, 408, 19),
                   wickets=c(17, 20, NA, 5))

arrange(stats, desc(runs))

rename(stats, runs_scored=runs)

select(stats, player, wickets)

mutate(stats, avg=runs/4)

transmute(stats, avg=runs/4)

stats <-data.frame(player=c('A', 'B', 'C', 'D'),
                   runs=c(100, 200, 408, 19),
                   wickets=c(17, 20, 7, 5))

summarize(stats, sum(runs), mean(runs))

newdata

head(newdata)

summary(newdata)

newdata$variety <-factor(newdata$variety,
                         level= c("Setosa", "Versicolor", "Virginica"),labels=c(1,2,3))
newdata1 <- newdata
newdata1


minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#data <-c("sepal.length","sepal.length","petal.length","petal.width")
normNewdata <- as.data.frame(lapply(newdata[c("sepal.length","sepal.width","sepal.length","petal.width")], minMax))
normNewdata
str(newdata)

s <-newdata[c("sepal.length","sepal.width","petal.length","petal.width")]
sapply(s, sd)




