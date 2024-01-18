
new_object <- c(20.5, 21, 19.8, 20)
temperatures_in_june <- c(20.5, 21, 19.8, 20)

print(new_object[1])
ages_of_students <- c(30, 32, 28, 30, 29, 22, 45)
ages_of_students2 <- append(ages_of_students,22)
print(ages_of_students2)
ess_data <- read.csv("ess_2020.csv")
dim(ess_data)
str(ess_data)
x <- 5
myage_vector <- c(21,2000,"Enis")
print(myage_vector) 
square <- function(x) {
  x^2
}
square(10)
greet <- function(name = "John") {
  print(paste("Hello", name))
}
greet("Enis")
install.packages("ggplot2")
library(ggplot2)
ggplot(ess_data, aes(x = cntry, y =happy,fill=cntry)) +
  geom_bar(stat = "summary", fun = "mean")
install.packages("dplyr")
library(dplyr)
ess_data %>% group_by(cntry) %>% summarize(avg_happy_index_men = mean(happy[gndr == 1]), avg_happy_index_women = mean(happy[gndr == 2]))


