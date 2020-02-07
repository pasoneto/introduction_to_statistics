#----------Block 1-----------#
#Understanding standard errors

#Run the code below. You do not need to worry about it now. 
#Just run it and skip to the first question
sim <- function(samples, cases, mn, stdev){
  #Creating an empty matrix
  df <- matrix(nrow = cases, ncol = samples)
  #Fills the  matrix with samples
  for(i in 1:samples){
    df[,i] <- (rnorm(n = cases, sd = stdev, mean = mn))
  }
  df <- data.frame(df)
  #naming each column of the sample with a different name
  for(j in 1:samples){
    name <- paste("sample", j, sep = "")
    names(df)[j] <- name
  }
  return(df)
}


#1) In the present script, we make use of a fictitious data set with 100 samples (10 observations
#each. #The 10 samples have been randomly drawned from a population with mean 10 and sd 2:

simulation <- sim(100, 10, 10, 2) #samples, observations, mean, sd


#1) In R, if you want to know a statistic of any given sample from our simulation,
#just type simulation$(name of the sample). Here I show you how to get the M and SD of sample 1

mean(simulation$sample1)
sd(simulation$sample1)

#Repeat this process for different samples from our simulation. Compare the statistics you get.
#Do you see significant differences between different samples from our population? Why do you
#Think that's tha case?


#2) Imagine we are interested in finding the mean of every sample within our simulation.
#doing this by hand might be a little tedious, so we do it as follows:

#Create an empty vector:
vector_of_sample_means <- c()

#Calculate the mean of each sample from our simulation and
#store the result within the empty vector above.
for(i in 1:length(simulation)){
  vector_of_sample_means[i] <- mean(simulation[,i])
}
 

#3) Now imagine that you are interested in finding out if the means of our samples vary too much. For that purpose, compute means and standard deviations
#For that, you would calculate the SD from our previously defined "vector_of_sample_means", 

sd(vector_of_sample_means)

#4)#What does the sd of sample means tell us? Why do you think this value is smaller
#than the actual standard deviation of our population of origin?


#5) Compute the formula for the standard error: sd/sqrt(N), 
#where N represents the number of observations in each sample:
2/sqrt(10)


#6) Compare it with the standard deviation you found in question 3. 
#Are they close? Why?

#7) Do you understand what the standard error of the mean tells us?

#6) In real life examples, we often have access to only one or two samples, and differently from
#this script, we do not know the mean and standard deviations of the populations originating these
#samples. In these cases, instead of using the exact standard deviation from the population (which we know 
#to be 2), we estimate it from our sample and divide it by the sqrt of our N.

#Estimate standard errors from each one of our 5 samples
sd(simulation$sample1)/sqrt(length(simulation$sample1))
sd(simulation$sample2)/sqrt(length(simulation$sample2))
sd(simulation$sample3)/sqrt(length(simulation$sample3))
sd(simulation$sample4)/sqrt(length(simulation$sample4))
sd(simulation$sample5)/sqrt(length(simulation$sample5))

#7) Compare the results between each sample, and then compare them with the response you gave on 
#questions 3 and 5. The result for all of these calculations should be somewhat close,
#And what it tells us is that the standard deviation of sample means (i.e. the 
#standard error of the mean) can be estimated from our sample, and we do not need to know
#the exact measure of dispersion from the underlying distribution to compute a pretty good
#SE statistic.





#----------BLOCK 2------------#
#Standard error and statistical tests


#For the following exercises, we will make use of these two randomly generated samples
#from a population with mean 10 and sd 2. Each sample has 10 observations.
simulation2 <- sim(2, 100, 10, 2)

#1) Researchers are often interested in evaluating if there are differences in means between
#two samples. Imagine you are a researcher and you want to evaluate if there is a difference in 
#means between samples 1 and 2 from our simulation.

mean(simulation2$sample1) - mean(simulation2$sample2)

#2) As a researcher, would you confidently say that there is a difference 
#between samples 1 and 2?


#3) In order to further evaluate this question, researchers often analyze how much variation there
#is between the scores of each group. In this case, we could create a new vector containing
#only the difference in scores:

differences <- c(simulation2$sample1-simulation2$sample2)

#And we could also calculate the standard deviation and SEs of these differences:
sd(differences)
sd(differences)/sqrt(length(differences))

#4) As previously explained, both SD and SEs are measures of dispersions. Analyze both statistics
#And compare them with the mean difference we found between samples 1 and 2. Do these statistics
#tell you anything else about the difference in means that we found from our samples? 


#5) Remember that large S.E.s indicate that if we randomly draw many samples from a given population
#these samples will have varying means. Would you trust more the difference that you found between
#samples 1 and two if the S.E. of the differences was small or large? Justify your answer.


#6) In order for you to get a better understanding of this, calculate the ratio between the 
#mean difference in you found between samples 1 and 2 and the SE of the differences:
mean_difference <- mean(simulation2$sample1-simulation2$sample2)
se_mean <- sd(differences)/sqrt(length(differences))
mean_difference/se_mean

#7) Keep in mind this value that you just found. As a researcher, you would be more certain
#that there is a true difference between samples 1 and 2 as this value increases. Do you
#Agree with that? Explain your thoughts.

#8) Dividing the mean_difference with the se_mean gives us what we call the paired-samples 
#t statistic. In this case, we calculated the t statistic considering that each
#observation from samples 1 and 2 came from the same person. In other words, 
#simulation2$sample1[1] and simulation2$sample2[1] came from the same person. 
#A real world example would be: 10 different peolple took two 2 tests each, 
#one in the morning and one in the afternoon.

#9) Now you might ask: what if samples 1 and 2 came from different people? How would we calculate
#the t-statistic? The answer is: pretty much the same way. The only difference is that, instead of 
#dividing the mean difference from each sample with the SE of the mean, we would have to estimate
#the common variance between the two samples. 

#Calculating pooled variances, which is the sum of squared residuals divided by 
#degrees of freedom.
sq_resid1 = sum((simulation2$sample1-mean(simulation2$sample1))^2)
sq_resid2 = sum((simulation2$sample2-mean(simulation2$sample2))^2)
degrees_freedom = (length(simulation2$sample1) + length(simulation2$sample2))-2
pooled_variance = (sq_resid1 + sq_resid2)/degrees_freedom

denominator = sqrt(((pooled_variance/length(simulation2$sample1)) + 
                    (pooled_variance)/length(simulation2$sample2))
                   )
nominator = mean(simulation2$sample1) - mean(simulation2$sample2)

t_statistic = nominator/denominator
t.test(simulation2$sample1,
       simulation2$sample2,
       paired = FALSE)[["statistic"]][["t"]]

