#--------Block 3-----------#
#Run the code below. You don't need to worry about it now. Just skip to the first question.

names <- c()
for(i in 1:10){
  name <- paste('participant', i)
  names[i] <- name
}
before_coffee = seq(from = 5, to = 50, by = 5) + rnorm(n = 10, mean = 1, sd = 2)
coffee_sim <- data.frame( "Participants" = c(names),
                          "before_coffee" = before_coffee,
                          "after_coffee" = before_coffee + rnorm(n = 10, mean = 20, sd = 1)
)


#1) Imagine you are a researcher and you want to measured the speed 
#of a pearson running before and after a cup of coffee. Here's your data set. Take a look at it
#compare the differences in both within subject (row to row), 
#and within conditions (column to column):
coffee_sim


#2) Calculate the descriptive statistics that we learned in our previous exercises

mean(coffee_sim$before_coffee)
mean(coffee_sim$after_coffee)
sd(coffee_sim$before_coffee)
sd(coffee_sim$after_coffee)

#3) Using only the descriptive statistics from question 2, 
#do you think that there will be a significance difference
#in speed before and after people take coffe? Justify your answer.


#4) Calculate the difference in scores between conditions:
dif <- mean(coffee_sim$before_coffee - coffee_sim$after_coffee)

#5) Calculate the standard error of the mean of the differences
se_mean <- sd(coffee_sim$before_coffee - coffee_sim$after_coffee)/sqrt(length(coffee_sim$before_coffee))

#6) Now divide the difference you found between samples with the S.E.
dif/se_mean

#7) Now compute a paired samples t-test using r's native function. Compare the t-statistic
#with the result from question 6
t.test(coffee_sim$before_coffee,
       coffee_sim$after_coffee,
       paired = TRUE)[["statistic"]][["t"]]


#8) Now repeat the procedure, but considering that samples 1 and 2 were collected from 
#different groups of people.
squared_residuals1 <- sum((coffee_sim$before_coffee- mean(coffee_sim$before_coffee))^2)
squared_residuals2 <- sum((coffee_sim$after_coffee- mean(coffee_sim$after_coffee))^2)
degrees_freedom <- (length(coffee_sim$before_coffee)+length(coffee_sim$after_coffee))-2

pooled_var <- (squared_residuals1 + squared_residuals2)/degrees_freedom

nominator <- mean(coffee_sim$before_coffee)-mean(coffee_sim$after_coffee)
denominator <- sqrt((pooled_var/length(coffee_sim$before_coffee))+
                      (pooled_var/length(coffee_sim$after_coffee)))

nominator/denominator

#Or take the shortcut:
t.test(coffee_sim$before_coffee, 
       coffee_sim$after_coffee, 
       paired = FALSE)[["statistic"]][["t"]]

#9) Compare the t-statistics that you got from exercises 7 and 8. Aside from the formula that
#we used to calculate them, why do you think that they are so different? Hint: look at the 

#Answer: in a paired sample t-test we are looking at the S.E.
##of the differences in scores, whereas in a paired sample, we are looking at a wheited average
#of variances between two samples. In plain words, in a paired samples t-test we are 
#care only about the differences between samples 1 and 2, whereas in an independent 
#samples t-test we take into consideration the variation in scores between groups.