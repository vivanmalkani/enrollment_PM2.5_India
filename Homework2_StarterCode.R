
# Starter Code for Homework #2 in ESS 368
# Below are examples of how to run basic panel regression and plot output. The data are from Burke, Hsiang, Miguel 2015 paper we will read in a few weeks. Data are at the country-year level, and given growth in GDP per capita and average temperature in each country-year. 

# The stuff at the bottom shows you how to construct and plot confidence intervals. This is not required for Homework 2, but will be required for the final project so check it out if you have time, and/or refer back to it later.

# clear workspace if you wanna
rm(list=ls())

# load packages.  need to have installed them first, e.g. using install.packages("blah")
library(tidyverse)
library(lfe)

#change the below to whatever your working directory is
setwd('/Documents/Dropbox/classes/ESS268/2019/assignments/homework2/')
dta <- read.csv("Homework2_data.csv")


# run a panel regression of GDP growth on temperature, with country and year fixed effects. 'iso' is the variable denoting countries, 'year' obviously denotes year
# i'm doing this using the felm() command from lfe(), which is currently the best way to run fixed effects models in R so long as you don't care about the estimates on the fixed effects themselves. With large datasets, it's roughly a bazillion times faster than manually making the FE variables and running the full regression in lm(). 
mod <- felm(growth ~ tmean | iso + year, data=dta)
    # check out ?felm for more on syntax.  basically, the first part is like lm(), then after the '|' you put the FE that you want
summary(mod)  # in full sample, there is a positive but not statistically significant relationship between growth and temperature
# it's easy to access the coefficients in this model, e.g. 
cf <- summary(mod)$coefficients
coef(mod)

# here's how'd you run the identical regression using LM.
mod <- lm(growth ~ tmean + as.factor(iso) + as.factor(year), data=dta)
summary(mod)  #same answer but now you get all the parameter estimates on the FE as well. 


# now lets say you wanted to include country-specific time trends as well as year FE. here's what you'd do:
mod <- felm(growth ~ tmean + as.factor(iso):year | iso + year, data=dta)  
  #you probably got a warning message about the matrix being rank-deficient. okay to ignore for now. R is probably complaining because some subset of the variables are collinear. for an extended discussion see here: https://cran.r-project.org/web/packages/lfe/vignettes/identification.pdf

#lets say, as in Burke et al 2015, you think the temperature/growth relationship might be not be linear. Run the next most obvious model:  a quadratic!  i.e.  y = T + T^2.   two ways to do this, equivalent:
summary(felm(growth ~ poly(tmean,2,raw=T) | iso + year, data=dta)) # use poly() function to generate polynomials
dta$tmean2 = dta$tmean^2
summary(mod <- felm(growth ~ tmean + tmean2 | iso + year, data=dta)) # manually generate squared term and include. 
  # now things are getting interesting!  including a squared term gives us statistically significant positive linear term, and negative squared term.  What does this plotted relationship actually look like?

#plotting regression results
range(dta$tmean)  #our temperature variables go from roughly 0 to 30C, which I will assume is the plotting window we want
x = 0:30
mod <- felm(growth ~ tmean | iso + year, data=dta) # first lets do a linear model
yy = x*mod$coefficients[1]  # here i'm generating the predicted values for y. 
#just multiplying our x's (here, 0 through 30) by the linear coefficient we just estimated, which you can access through mod$coefficients. i..e y = b*x
plot(x,yy,type="l",las=1,ylab="growth rate",xlab="temperature (C)")
    # okay, from the plot it's hard to tell what's going on, and there's no uncertainty here.  will worry about that below
# how about our quadratic model?
mod <- felm(growth ~ tmean + tmean2 | iso + year, data=dta)
yy = x*mod$coefficients[1] + x^2*mod$coefficients[2]  #now, y = b1*x + b2*x^2. so pull out both coefficients, generated predicted y variable.  plot:
plot(x,yy,type="l",las=1,ylab="growth rate",xlab="temperature (C)")  #curvy!
  # maybe you're interested in where the peak of the curve is?  calculate derivative, set equal to zero:  
  #   dy/dx = b1 + 2*b2*x = 0
  #   x = -b1/(2*b2)   in code this is:
  -mod$coefficients[1]/(2*mod$coefficients[2])  # or ~16.2 degrees C

# NOTICE SOMETHING VERY IMPORTANT HERE:  we have no idea what the intecept is here. R is setting it equal to zero given how we defined x and yy, but that's arbitrary.  In these FE models, there is no "one" intercept; recall that each country is getting its own.  But every country gets this same response function we just estimated.  The way we have it plotted now, the line basically shows how much more or less growth you get compared to a year spent at 0C.  The way it's drawn, you could easily ask a question like:  "Compared to a year at 0C, how much higher is economic growth if you had a year at 5C".  And then answer from the plot is something like 3.5%.  
# In papers, we often re-center these response functions at some arbitrary value in our distribution of X's.  E.g. we plot them relative to the effect at 20C (or some temperature at the middle of the distribution).  
#Once we've done this, the interpretation of the plot would then be:  "the effect of temperature on the growth rate, relative to a year spent at 20C". And here's how'd you plot that: 
yy1 <- yy - yy[x=20]  #for the predicted y's you just made above, you just subtract off the value of y where x==20. This works because x and yy are the same length by design, and we defined x to take on integer values between 0 and 30. 
plot(x,yy1,type="l",las=1,ylab="growth rate",xlab="temperature (C)")  # now it crosses y=0 at x=20
abline(h=0)
    # a plot like this is fine for Homework 2.


# We have ignored uncertainty up until now. But our estimates have confidence intervals. That is, had we drawn a slightly different sample of countries or years from some "superpopulation" of country-years, we might have gotten a different estimate.  
# If we had a simple linear model with no FE, there are nice analytic ways to do this.  Example, running a regression with no fixed effects
mod1 <- lm(growth ~ tmean, data=dta)
yy <- predict(mod1,data.frame(tmean=x),interval = "predict", level=0.95)  #this is now a matrix with three columns, giving the point estimate and the upper and lower bounds of the 95% confidence interval
plot(x,yy[,1],type="l",las=1,xlab="temperature",ylab="growth rate",ylim=c(-0.2,0.2))
lines(x,yy[,2],lty=2)  #lower bound
lines(x,yy[,3],lty=2) # upper boudn

# But it's a lot trickier when we're estimating non-linear models like the one we just did using felm().  The best way to get confidence intervals in this context is by bootstrapping.  Bootstrapping is basically just repeatedly resampling (with replacement) from your data.  That is, we have 6584 observations in our data.  From this original dataset I am going to create a new dataset also with 6584 observations, in which some observations from the old dataset will show up multiple times and some won't show up at all.  Toy example of how this works:
a <- 1:10
b <- sample(a,size=length(a),replace=T)
print(b)  # the new vector b has same length as a, but some numbers repeated and some left out. 
# so the idea is to do this a bunch of times (say, 100 or 1000).  Each time you draw a new sample from your original data, re-estimate your regression on the new data, and save the coefficients. then we use these coefficients to construct a confidence interval. 

# example with 100 bootstraps
coef <- matrix(nrow=100,ncol=2)  #make a matrix to hold the coefficients you will generate.  here i have a temperature and temp squared term, so need to hold 2 coefficients for each bootstrap
ll = dim(dta)[1]  #the number of observations we have in the original dataset: 6584
for (i in 1:100)  {
  samp <- sample(1:ll,size=ll,replace=T)  #this draws a sample of length 6584 from the numbers 1:6584, where again we will have some repeats and some no-shows. We are going to use this to index our data.
  newdata = dta[samp,]
      # to convince yourself this is the same size as old dataset: dim(dta), dim(newdata)
  mod <- felm(growth ~ tmean + tmean2 | iso + year, data=newdata)  #estimate our regression y = b1*T + b2*T^2
  coef[i,] <- coef(mod)  #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
  print(i)  #print this out so you can watch progress :)
}
# Okay so now your coef matrix has 100 bootstrapped estimates for your two coefficients. A simple way to visualize is to make a line out of each one of these and plot each!  But recall, we don't know what the intercept is, so for each we are going to use our re-centering trick before plotting.  Let's recenter again at X=20C. 

plot(1,xlim=c(0,30),ylim=c(-0.15,0.05),las=1,xlab="temperature",ylab="growth rate")  #i'm starting by initialing an empty plotting window.  then i'm looping over my bootstrap estimates, making our predicted y's for each bootstrap, and plotting the line.
for (i in 1:100) {
  yy <- x*coef[i,1] + x^2*coef[i,2]  
  yy <- yy - yy[x=20]
  lines(x,yy,lwd=0.5)
}
#and you could add your point estimate using the original dataset
    mod <- felm(growth ~ tmean + tmean2 | iso + year, data=dta)
    yy = x*mod$coefficients[1] + x^2*mod$coefficients[2]  
    yy = yy - yy[x=20]
    lines(x,yy,col="red",lwd=2)
# add a zero line:
    abline(h=0,col="blue")
# interpretation?  lines seem to cross zero everywhere, so you might be tempted to say, uhh, doesn't look significant anywhere.  but recall this is ALL our bootstrapped estimates, and typically we can get away with just plotting the 95% confidence interval or sometimes even the 90% confidence interval if reviewers are being friendly/ checked out. 
# What we'd have now is basically the 100% confidence interval (or, almost: typically you'd want to bootstrap 1000 times or more; we just did 100 for simplicity). So we don't know what more "conventional" confidence intervals look like yet. 
# So how do we convert these lines to a 90 or 95% confidence intervals?  The basic idea is, for all your X values, draw an imaginary vertical line at that X value. In our plot it will cross 100 lines, each with corresponding y value. If you ranked those 100 y values from 1:100, and took the 5th and 95th values, that would be the 90% confidence interval at that X value. To get the 95% confidence interval, you'd have to take the 2.5th and 97.5th values, which don't actually exist (had we run 1000 bootstraps, they would!  it'd be the 25th value and the 975th value in your 1000 ranked values).
# Okay to implement this, basically we're going to create the predicted y's as above and save all the predicted y's, and then calculate the values we want for each value of X. 
boots <- matrix(nrow=100,ncol=length(x))  #making a matrix to hold our estimates. nrow= number of bootstraps, ncol = number of X values we want to compute
for (i in 1:100) {
  yy <- x*coef[i,1] + x^2*coef[i,2]   # as before
  yy <- yy - yy[x=20]  # as before
  boots[i,] <- yy  # save these to the boots matrix
}
# now we have 100 predictions for y at each value of X; we want to find the 5th and 95th value at each value of X. here's the easy way:
confint <- apply(boots,2,function(x) quantile(x,probs=c(0.05,0.5,0.95))) 
    # we are using apply(), which applies a function over a matrix or an array. first argument is matrix we want to apply function over, second argument is the dimension of the matrix you want to apply over where row=1 and col=2, and the third thing is a function, e.g. mean(). here' the function i'm using is just the quantile() function, where I'm telling it to take the values at the 5th, 50th, and 95th percentiles of the distribution. Had we wanted 95% CI we could have said probs=c(0.025,0.5,0.95)
# This returns a matrix with 30 columns correspondign to our x values, and three rows corresponding to the percentile values we wanted to compute at each X value.  Now we can plot!
plot(1,xlim=c(0,30),ylim=c(-0.15,0.05),las=1,xlab="temperature",ylab="growth rate")  #initialize plotting window again
lines(x,confint[2,])  #median estimate across bootstraps
lines(x,confint[1,],lty=2)  #lower 5th percentile
lines(x,confint[3,],lty=2)  #upper 95th percentile

# here's a way to do it shaded, using polygon function:
plot(1,xlim=c(0,30),ylim=c(-0.15,0.05),las=1,xlab="temperature",ylab="growth rate")  #initialize plotting window again
polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col="grey80",border = NA)
lines(x,confint[2,])  #median estimate across bootstraps

