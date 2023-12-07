Martyna Baran, Zuzanna Jarlaczynska, Dario Napolitano, Maria Leon, Laura []DATABASE INFO

author: TAWFIK ELMETWALLY

title: Car information dataset

publisher: Kaggle

url: https://www.kaggle.com/datasets/tawfikelmetwally/automobile-dataset
cars=read.csv("Downloads/Automobile.csv") 
head(cars)
str(cars)
As we can see, our dataset contains 9 columns and 398 rows. Each column represents one feature and each row stands for one pattern. 
sapply(cars, class)
In our dataset we have diverse types of features: 2 factor, 3 numeric and 4 numeric columns. Columns represent the following features:
name -  unique identifier for each automobile
mpg -  fuel efficiency measured in miles per gallon
cylinders - number of cylinders in the engine
displacement - engine displacement, indicating its size or capacity
horsepower - power output of the engine
weight - weight of the automobile
acceleration - Capability to increase speed, measured in seconds
model_year - year of manufacture for the automobile model
origin - country or region of origin for each automobile
summary(cars)
From the obtained tabel we can get the basic informations about our dataset. Firstly, the most frequent car model is ford pinto. Furthermore, the medium quantity of miles per gallon is 23.51 which is also quite close to the median. The number of cylinders veries from 3 to 8 while the displacement hasitates from 68 to 455. The mean horsepower for cars in our dataset is 104.5. The heaviest car weights over 3 times more than the lightest one. Also the fastest one is 3 times as fast as the slowest one. All the models come from years 1970-1982. Finally, the vast majority of them origin from the USA. 
sum(is.na(cars))

cars=cars[rowSums(is.na(cars))<=0,]
sum(is.na(cars))

cars$brand <- sapply(strsplit(as.character(cars$name), " "), function(x) x[1])
table(cars$brand)
Another thing we need to notice is that some names had spelling mistake like "toyouta" instead of "toyota" and were wrongly interpretet as another brand. We need to fix that.

cars$brand <- gsub("toyouta", "toyota", cars$brand)
cars$brand <- gsub("vokswagen", "volkswagen", cars$brand)
cars$brand <- gsub("vw", "volkswagen", cars$brand)
cars$brand <- gsub("chevroelt", "chevrolet", cars$brand)
cars$brand <- gsub("chevy", "chevrolet", cars$brand)
cars$brand <- gsub("maxda", "mazda", cars$brand)
cars$brand <- gsub("mercedes-benz", "mercedes", cars$brand)

table(cars$brand)

hist(cars$cylinders, 
	xlab="cylinders", 
	ylab="number", main = "Cylinders number")
On the obtained histogram we can see that the vast majority of cars has 4 cylinders - over 200. Another equally popular numbers are 6 and 8. The remaining values are very rare, which may indicate noisy data. 
barplot(table(cars$horsepower), 
        xlab="horsepower", 
        ylab="number", main = "Horsepower plot")
Analysing the horsepower plot we may conclude that the range of values is in this case quite wide. The most popular numbers are 150 and 90,  but each of them characterises only about 20 patterns. Considering the size of our dataset, it is less than 5%, so not much. Most of the classified cars have between 70 to 100HP, but there is a group of stronger engines about 160HP. 
temp<-density(table(cars$acceleration)) 
plot(temp, type="n", 
     main="Acceleration density plot") 
polygon(temp, col="lightgray", 
        border="gray")
The vast of our automobiles have acceleration in range 0 to 5, values under 20 are less common but still encountered. Other values occur so rarely, that they may be the outliers and we shouldn't consider them useful. 
boxplot(cars$weight, main="Cars weight")
From the plot we can see that the weight of classified automobiles vary from 2000 to 3500 kilograms, the mean value is about 2750. Furthermore, when it comes to 'weight' feature, our dataset doesn't include definite outliers. 
library(knitr)
library(dplyr)

grouped_data <- cars %>% group_by(brand)

agg_func <- list(
  avg_mpg = ~mean(mpg),
  max_mpg = ~max(mpg),
  min_mpg = ~min(mpg),
  origin = ~first(origin)
)

data <- grouped_data %>% summarize(
  avg_mpg = mean(mpg),
  max_mpg = max(mpg),
  min_mpg = min(mpg),
  origin = first(origin)
)

data <- data %>%
  arrange(desc(avg_mpg))

kable(data)

library(knitr)
library(dplyr)

grouped_data <- cars %>% group_by(brand)

agg_func <- list(
  avg_acceleration = ~mean(acceleration),
  max_acceleration = ~max(acceleration),
  min_acceleration = ~min(acceleration),
  origin = ~first(origin)
)

data <- grouped_data %>% summarize(
  avg_acceleration = mean(acceleration),
  max_acceleration = max(acceleration),
  min_acceleration = min(acceleration),
  origin = first(origin)
)

data <- data %>%
  arrange(desc(avg_acceleration))

kable(data)

library(ggplot2)

country_counts <- table(cars$origin)
pie_chart <- ggplot(data = as.data.frame(country_counts), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Origin Distribution") +
  theme(legend.position = "bottom")
print(pie_chart)
The presented chart shows us the pattern distribution based on the place of origin. We can see that the vast majority of samples comes from United Stated. The remaining 30% of our dataset is equally divided into cars from Europe and Japan.
scatter_plot <- ggplot(cars, aes(x = origin, y = mpg)) +
  geom_point(size = 5, alpha = 0.5) +
  labs(title = "Average MPG by Origin", x = "Origin", y = "Average MPG") +
  theme_minimal()

print(scatter_plot)
# slot(table(cars$model_year, cars$), 
#         xlab="Year", 
#         ylab="number", main = "Model year plot")

# bar_plot <- ggplot(cars, aes(x = origin, y = mpg)) +
#   geom_bar(stat = "summary", fun = "mean", fill = "blue") +
#   labs(title = "Average MPG by Origin", x = "Origin", y = "Average MPG") +
#   theme_minimal()

# print(bar_plot)
This scatter plot represents the relationship between number of miles per gallon and place of origin. As we can see, the density of the plot is the highest for different values for each place. For Europe this values oscilates around 25, for Japan about 32 and for USA about 20. Taking this into account, we may conclude that american cars are more efficient when it comes to fuel usage. However, we need to remember that a great number of automobiles in our dataset originate form USA and among them the fuel usage is quite wide spread. 

barplot(table(cars$model_year), 
        xlab="Year", 
        ylab="number", main = "Model year plot")


ggplot(cars, aes(x=model_year, y = displacement)) + 
    geom_point(aes(color=brand)) +
    labs( title = "Relationship between Year and Displacement",
        x = "Year of Production",
        y= "Displacement",
        color="Brand")

ggplot(cars,aes(x=model_year,y=displacement))+
    geom_line(stat="summary", fun="mean", aes(group =1),color="blue")+
    geom_point(stat = "summary", fun = "mean", color = "red", size =3)+
labs(title = "Average Displacement by Year",
    x="Year of Production",
    y="Average Displacement")

library(tidyverse)
arrange(cars, desc(mpg)) %>% slice(1:10) 
Above we can see the top 10 most efficient cars, basing on how many miles each car makes per gallon of petrol. We may conclude that the most efficient brand is volkswagen. 
cars %>% group_by(origin) %>% summarize(avg_mpg=mean(mpg)) 
# The most efficient cars are the japanese brands


