#----------BLOCK 2------------#
#Standard error and statistical tests

#For the following exercises, make use of these two randomly generated samples
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

