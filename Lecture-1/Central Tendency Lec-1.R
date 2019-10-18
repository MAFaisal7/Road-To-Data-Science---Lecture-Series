
############### LECTURE 1: MEASURE OF CENTRAL TENDENCY ############################################

library(MASS)
df <- Cars93
View(df)

#View the mean as a measure of central tendency (viewing mean horsepower of USA built cars)
Horsepower.USA <- Cars93$Horsepower[Cars93$Origin == "USA"]
                      mean(Horsepower.USA)
            
Horsepower.NonUSA <- Cars93$Horsepower[Cars93$Origin == "non-USA"]
mean(Horsepower.NonUSA)

#Another method of writing code
with(Cars93, mean(Horsepower[Origin == "USA" & Cylinders ==4]))

# To create histograms side by side
library(ggplot2)

ggplot(Cars93, aes(x=Horsepower)) +
  geom_histogram(color="black", fill="white",binwidth = 10)+
  facet_wrap(~Origin)

#GEOMETRIC MEAN
#In this example, the geometric mean is the fifth root of the product of five numbers. 
#Is it always the nth root of the product of n numbers? Yep.

invest <- c(1.10,1.15,1.10,1.20,1.05)
gm.invest <- prod(invest)^(1/(length(invest)))
gm.invest

#MODE 
install.packages("modeest")
library(modeest)
scores <- c(1,2,2,2,3,4,4,4,5,6)
mfv(scores)
#VARIATION
#Think of variation as a kind of average of how
#much each number in a group of numbers differs from the group mean.


#VARIENCE IS THE DISTANCE OF ONE DATA POINT FORM THE MEAN X-Xbar
#For sample variance
heights <- c(50, 47, 52, 46, 45)
var(heights)

#For popullation varience
var.p = function(x){var(x)*(length(x)-1)/length(x)}
var.p(heights)

#STANDARD DEVIATION
#The standard deviation of a population is the square root of the population variance
  #STANDARD DEVIATION OF SAMPLE of POPULATION
sd(heights)

with(Cars93, var(Horsepower[Origin == "USA"]))

#to know how many members are presenet the count of cars
with(Cars93, length(Horsepower[Origin == "USA"]))

#z-sCORE: The idea is to take a set ofscores and use its mean as a zero point, and its standard deviation as a unit of
#measure. Then you make comparisons: 
#When you calculate the z-score for every score in the set, the mean of the z-scoresis 0, and the standard deviation of 
#the z-scores is 1.

Horsepower.USA.Eight <- Cars93$Horsepower[Cars93$Origin == "USA" & Cars93$Cylinders == 8]
Horsepower.USA.Eight

scale(Horsepower.USA.Eight)
attr( ,"scaled:scale")                
                                      
                                      
                                      
                                      
                                      
                                      
                                    
                                    