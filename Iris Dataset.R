library(dplyr)
library(e1071)
data()
data("iris")
iris


#####################################################################################
#1) Using while loop to calculate 2 means one for [sepal length] other for [petal length]
sepal_len_sum=0
petal_len_sum=0

i=1
while (i <= 150) {
  sepal_len_sum <- sepal_len_sum + (iris$Sepal.Length[i])
  petal_len_sum <- petal_len_sum + (iris$Petal.Length[i])
  i<- i+1
  
}
sepal_len_mean <- sepal_len_sum / 150
petal_len_mean <- petal_len_sum/ 150

sepal_len_mean
petal_len_mean

#all of the above equal these built in functions 
#print(mean(iris$Sepal.Length)) 
#print(mean(iris$Petal.Length))

#####################################################################################
#2) plot [sepal length] of the dataset using dot plot where you must use the following:
#  - group data on [species] - Change the symbol to make it '+ symbol' - Make title 'Iris Data'


dotchart(iris$Sepal.Length, groups=iris$Species, labels = row.names(iris$Species),
         cex =0.9, main = "Iris Data", xlab= "Sepal Length",pch = "+")

#####################################################################################
#3) plot [sepal Width] of the dataset using histogram and then using 
#density line in the same plot where you must use the following:
#  - make the line of density smooth by 2.5
#- Make sure that histogram is for density not frequency

hist(iris$Sepal.Width, prob = TRUE, col="blue",,main = "Histogram for Sepal width", xlab= "Sepal width")
lines(density(iris$Sepal.Width,adjust=2.5), col="red", lwd=5)

#####################################################################################
#4) Using Data wrangling pipe operator get the rows of the dataset 
#where the [species] is 'setosa' or [sepal length] greater than 6.5
#then sort by sepal length.

iris_wrangling_1 <- iris %>%
  filter(iris$Species == "setosa" | iris$Sepal.Length > 6.5) %>%
  arrange(Sepal.Length)

iris_wrangling_1

#####################################################################################
#5) Using Data wrangling pipe operator group the data on [species] 
#then summarize [petal length] by the mean and sd.

iris_wrangling_2 <- iris %>%
  group_by(iris$Species) %>%
  summarise(petal_len_mean=mean(Petal.Length),petal_len_SD=sd(Petal.Length))

iris_wrangling_2
#####################################################################################
#6) Build the linear regression model which will be used to get the dependent value [petal width],
#based on [petal length]that affect Then find the correlation between them
#Finally make prediction using only the equation when petal length equal 6.

#linear Regression
fit<-lm(iris$Petal.Width ~ iris$Petal.Length)
fit
#correlation between petal width and petal length
correlation_petal_len_wid <- cor(iris$Petal.Length,iris$Petal.Width) 
correlation_petal_len_wid 

#prediction when petal length equal 6.
predict_petal_width <- fit$coefficients[1] + fit$coefficients[2]*6
predict_petal_width
#####################################################################################
#7) Bonus: chose a classifier of your choice and implement it on your data set

trainData<-rbind(iris[1:40,],iris[51:90,])
trainData<-rbind(trainData,iris[101:140,])
trainData
count(trainData)#row 120 row  80% training data

testData<-rbind(iris[41:50,],iris[91:100,])
testData<-rbind(trainData,iris[141:150,])
testData
count(testData)#row 30 row  20% test data

#(Species ~ . ) == (Species ~ sepal.length+sepal.width+petal.length+petal.width)
classifiy_Species<-naiveBayes(Species ~ ., data =trainData)
SpeciesPrediction <-predict(classifiy_Species, newdata = testData)
table(SpeciesPrediction)
