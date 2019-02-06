#Question 1: data "SEEDLING_SURVIVAL.csv"
read.csv("SEEDLING_SURVIVAL.csv")
seedling_survival<-read.csv("SEEDLING_SURVIVAL.csv")
plot(jitter(seedling_survival$survival)~seedling_survival$HEIGHT)
curve(plogis(0+2*x),col="red",add=T)
plot(jitter(seedling_survival$survival)~seedling_survival$LIGHT)
curve(plogis(0+2*x),col="red",add=T)
height<-glm(seedling_survival$survival~seedling_survival$HEIGHT,family = "binomial")
height
-0.06271/4
0.14071/4
light<-glm(seedling_survival$survival~seedling_survival$LIGHT,family = "binomial")
light
2.66195/4
-0.06553/4
#Based on my work above my assessment would be that 
##height is a stronger or more consistent predictor of seedling survival.

#Question 2: Data "seeds.csv"
read.csv("seeds.csv")
Seeds<-read.csv('seeds.csv')
#a) a plot of raw data with the best-fit regression line (use curve) overlaid on the plot
#(1) plotting proportional data: use number of successes divided by the total number trials as 
#the y-axis variable
#successes=recruits and trials = seeds?
plot(jitter(Seeds$seedlings),Seeds$recruits/Seeds$seeds,xlab='Pre-existing Seedlings in plot',
     ylab='Seed Success')

curve(plogis(2.5-1.5*x,col='green',add=T))
##### I am confused about where the values for the curve function come from

#(2) a proportional glm in R, you must first creat a proportional variable, 
##with one column representing successes, and one column representing failures:
response<-cbind(Seeds$recruits,Seeds$seeds-Seeds$recruits)
head(response)      
#fit a glm to get estimates of our parameters
SeedParameters<-glm(response~Seeds$seedlings,family = binomial)

#b: point estimates for slope and intercept parameters, including a verbal description of the 
#baseline and effect size for these parameters
coef(SeedParameters)
# (Intercept) Seeds$seedlings 
#-2.035570       -1.213717
#baseline effect:
plogis(-2.035570)
#0.1155186 
#11.55186% baseline seedling survival

#c: Confidence intervals for slope and intercept
confint(SeedParameters)
#                     2.5% and 97.5%
#intercept       -2.121808   -1.9511837
#Seeds$seedlings -1.592362   -0.8977661
#Seedlings already present in the study plot have a significant negative effect on seed success
#recruitment is low or there could be an allelopathic effect of the existing plants

#Question 3
skeeter<-read.csv("mosquito_data.csv")
head(skeeter)
#a. Plot the data. (note: plotting the proportion of eggs is necessary, since the number of 
#eggs varies)
plot((skeeter$Emergent_adults/skeeter$Egg_Count)~skeeter$Detritus,xlim=c(0,5),xlab="Detritus",
     ylab = "Skeeters emerged/# of eggs")
#polynomial data
curve(plogis(1.44-0.19*x-(0.21*x^2)+(0.04*x^3)),col='navy',add=T)

#Ricker
curve(plogis(10*x*exp(-2*x)),col='orange',add=T)

#c. How are the biological implications of the polynomial model different from the Ricker model?
#d. use dbinom to calculate the likelihood of both models (with given parameter values)
#Hint: use log=T as an argument in dbinom before taking sum of dbinom output
#e. According to dbinom, the likelihood of the data is higher for which model?
#for dbinom
#function(x,size, prob,log=FALSE)
#x is successes, size is our number of trials, probability is 
#"if we didnt put the sum, it would give us a 1000 numbers because there are 1000 trials

Evaluation_Poly<--sum(dbinom(x=skeeter$Emergent_adults,
                             size=skeeter$Egg_Count,
                             prob=plogis(1.44-0.19*skeeter$Detritus-
                                           (0.21*skeeter$Detritus^2)+(0.04*skeeter$Detritus^3)),
                             log=TRUE))
Evaluation_Poly
#1415.63

Ricker_Evaluation<--sum(dbinom(x=skeeter$Emergent_adults,size = skeeter$Egg_Count,
                               prob=plogis(10*skeeter$Detritus*exp(-2*skeeter$Detritus)),log = TRUE))
Ricker_Evaluation
#1385.847

#Question 4
Plant_Survival
plantsurvival<-Plant_Survival
plot(plantsurvival$Plant~plantsurvival$Survival)
mod_plantsurvival<-lm(plantsurvival$Plant~plantsurvival$Survival)
coef(mod_plantsurvival)
#0.80    0.50
curve(0.80+0.50*x,add=T)
confint(mod_plantsurvival)
#0.89   0.59
plantsurvival$Survival
sigma(plantsurvival)
0.65

slope=0.07
intercept=0.59
standev=0.65

plantsurvival=seq(from=65,to=1500)
sample=c(3:10000)
Vector_pow<-rep(NA,times=length(sample))

#Notes: for each different vlaue of this sample size we want to be able to store a value
##y = generated response variable, drawing if from the normal distribution that we already know 
###the bounds of our predictor variable can also be estimated by what we already know
#survival ranges from 0-1
for(j in 1:length(sample)){y=norm(n=sample[j],mean(intercept+slope*seq(from=65,to=1500,length=sample[j])),sd=standev)m1<-lm(y~seq(from=65,to=1500,length=sample[j]))power_vector[j]=coef(m1)[2]}
#plot
plot(Vector_pow~sample)
abline(h=0.05,col="purple",lwd=1)
#It levels out around 2000 samples

#Binary data power analysis
plantsurvival
head(survival)
#I made up this survival data because I couldn't figure out how to get published data (I am now working on learning)
findcoef<-glm(plantsurvival$Survival~plantsurvival$Plant,family=binomial)
findcoef
confint(findcoef)
###0.2     0.40
dim(plantsurvival)
##15     3

slopeP=0.2
interceptP=0.40
samplesizeP=15

PlantsP=seq(from=1,to=29,length=15)
samplesize=seq(from=20,to=8000)
estimatedslope=rep(NA,times=length(samplesize))
for(j in 1:length(samplesize[j])){y=rbinom(n=samplesize[j],prob=plogis(interceptP+slopeP*seq(from=1,to=12,length=samplesize[j])),size=1)m1<-glm(y~seq(from=1,to=12,length=samplesize[j])family="binomial")estimatedslope[j]=coef(m1)[2]}
plot(estimatedslope~samplesize)
abline(h=0.4,col="green",lwd=1)
MSE1=mean((0.4-estimatedslope)^2)
MSE1

for(j in 1:length(samplesize)){y=rbinom(n=samplesize[j],prob=plogis(interceptP+slopeP*seq(from=1,to=12,length=samplesize[j])),size=1)
m1<-glm(y~seq(from=1,to=12,length=samplesize[j]),family="binomial")estimatedslope[j]=summary(m1)$coefficients[2,4]}
#the estimated slope value is the p value
#this in particular is not very acurate as our assurance of achieving a better p value is low
#this data also is confusing so thats probably why 