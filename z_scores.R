#Imagine you are taking a test to measure your english skills.
#Historically, brazilians who take this exact same test receive a mean score of

m = 3

#with a standard deviation of

sd = 3

#The distribution of brazilian scores can be visualized by running the code below:
#the red line represents the population mean.
hist(rnorm(1000, 3, 3), breaks = c(80))
par(new= TRUE)
abline(v = mean(rnorm(1000, 3, 3)), col="red", lwd=3, lty=2)

#Imagine you went well on the test, and your score was: 
score = 10

#1) Calculate and report your standardized z-score below:
zscore = (score-m) / sd

#Ok, that was easy. Calculating a z-score is a pretty straight forward process, 
#and you basically subtract your score of interest with the population mean scores
#and divide it by the standard deviation.

#2) One thing that is not so easy to understand, at least in my opinion, is the rationale
#behind this formula. Why is it that we calculate the difference between your score
#and divide it by a measure of dispersion (standard deviation)? 

#3) Do some simulations with varying values of standard deviation, 
#and then do more simulations with varyiing values of population mean and individual
#scores. Can you see a pattern?

#4) For instance, With the same score and population mean, 
#how does the SD affect your results?
#This just cleans up your environment variables... Don't worry about it
rm(list = ls())  

#Uncomment the lines below by deleting the '#', fill the variables in with
#values of your choice, and report the results.

#m = 
#score = 
#sd = 
#z_score = (score-m) / sd 

#5) Now that you did these simulations, try to formalize a response on why the z-score 
#is calculated the way that it is. More specifically: why do we divide a difference 
#in score by a measure of dispersion? Something that may help you is to thing about
#what the standard deviation tells us, as well as what the difference between a score 
#and a population mean tells us.


#6) Let us visualize some things from exercise 1.
#Remember that we were dealing with an english test where, in average,
#brazilians scored 3, with a standard deviatio of 3.

#Run the code below to visualize how this 

#Here I'm creatig a normal distribution with mean 10 and standard deviation 2. 
#and sampling 1000 cases from it.
rm(list = ls())  
brazilians_in_english = rnorm(1000, 3, 3)

#Run the code below. The red line indicates the mean scores from brazilians, which we know
#to be 3. the blue line indicates your pretty cool score of 10.  
hist(brazilians_in_english, breaks=seq(-50,50, 0.5), xlim=c(-10,15), ylim=c(0,100))
par(new=TRUE)
abline(v = mean(brazilians_in_english), col="red", lwd=3, lty=2)
abline(v = 10, col="blue", lwd=3, lty=2)

#7) What can you conclude from looking at the difference in scores from you
#to the general brazilian population who took the test. Are you part of a large
#group of brazilians who score 10 on a test? Look up the definition of histograms
#- in case you need to - and elaborate your answer.

#The code below shows the distribution of scores for the same test
#but taken by a population of americans. Let us say that the population of 
#americans scores an average of 11 with a standard deviation of 3. I'm 
#also showing you a red line representing average scores for brazilians, 
#a black line showing average scores for americans and a blue line showing 
#your score. Run the code and take your time to look at the plot that it generates
#below:
rm(list = ls())  
brazilians_in_english = rnorm(1000, 3, 3)
americans_in_english = rnorm(1000, 11, 3)
hist(americans_in_english, col = 'grey', breaks=seq(-50,50, 0.5), xlim=c(-10,30), ylim=c(0,100), main='Histogram of brazilians and americans', xlab='scores', ylab='Frequency')
hist(brazilians_in_english, col = 'green', breaks=seq(-50,50, 0.5), xlim=c(-10,30), ylim=c(0,100), main='', xlab='', ylab='', add = T)
par(new=TRUE)
abline(v = mean(brazilians_in_english), col = "red", lwd = 3, lty = 2)
abline(v = mean(americans_in_english), col = "black", lwd = 3, lty = 2)
abline(v = 10, col = 'blue', lwd = 3, lty = 2)

#8) Final - and perhaps most important - question. We know that you, 
#a brazilian, scored 10 on the test. We also know that brazilians usually score
#arround 3, and that americans usually score around 11.

#Imagine that a private investigator doesn't know your nationality. All he kows
#is the distribution of scores for brazilians and americans, as well as the
#score that you got when you did your test.

#What would be a good guess from the investigator's point of view?
#Would he guess that you are a brazilian or an american? Justify your answer
#in probabilistic terms. More specifically, use a z-table to calculate the
#probability of your score, given that you are brazilian, and the probability of your 
#score give that you are american.
