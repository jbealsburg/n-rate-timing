### Vignette to demonstrate how to estimate the agronomically
### optimum nitrogen fertilizer rate for maximizing yield.
### The example includes Kernza grain yield response to 6 N
### fertilizer rates tested at three locations in 2012.
### Packages will be loaded prior to using functions that 
### requires those packages, so you know which packages are 
### needed for certain functions.

setwd("/Users/junge037/Documents/Projects/Misc/MiscR/CurveFitting") #Set your own for where you save the data.
setwd("D:/My Drive/Mac/Projects/Misc/MiscR/CurveFitting") #Set your own for where you save the data.
setwd("/Volumes/GoogleDrive/My Drive/Mac/Projects/Misc/MiscR/CurveFitting")
dat<-read.csv('Ndat.csv')
head(dat) # We have 6 columns showing Kernza seed yield (seedyld) in
#response to 6 nitrogen fertilizer rates (Nfertnew) at 3 locations (Lam, Was, & Mor).
#There are 4 reps of each treatment per location. There is also a 'trt' column that is 
#a code identifier for the N fertilizer treatments. This should be a factor.

str(dat) #check to see what variables are factors

#Make some variables factors. Keep the originals, but make a new
#column for each new factored variable and label it with an "f"
dat$frep<-factor(dat$rep)
dat$ftrt<-factor(dat$trt)

str(dat) #Looks like our new variables are in the data frame as factors

#plot the data in ggplot to see what we should expect for best fitting function
#First, summarize the data by averaging points within a rep for each treatment/location combo
#the summarySE() function in the Rmisc is awesome for this. I use it all the time

library(Rmisc)
sumdat<-summarySE(data=dat, measurevar="seedyld", groupvars=c("Nfertnew", "location"), na.rm=T)

#Think of ggplot as a set of layers. The fist bits of information, which are filled in withing the 
#initial ggplot arguement, include the dataset, and then the aesthetics, or aes(). In here, you tell ggplot
#what the x and y axes should represent. And if you want to apply any seperator, such as line color, point shape, line type, you can do that here.
library(ggplot2)
#Starting with the most basic plot.
ggplot(sumdat, aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point() #only adding points, but we will make each location a unique color as directed above.
#Now we'll add error bars to the points from another column of information in that same dataset.
ggplot(sumdat, aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point()+
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se), width=0.2) #note you can use the standard error,
#standard deviation, or 95% confidence interval for an errorbar as the summarySE function provides all 3.

#Now let's plot the data in other ways
#First, treat N fertilizer as a categorical variable and draw some bar graphs, one for each location
#Again, we use the summarySE() function to summarize the data into the groups we're interested in.
sumdat2<-summarySE(dat, measurevar = "seedyld", groupvars=c("ftrt", "location"), na.rm=T)

ggplot(sumdat2, aes(x=ftrt, y=seedyld, fill=location))+
  geom_bar(position=position_dodge(), #puts the bars next to each other
           stat="identity") +
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se),
                width=0, #This changes how wide the horizontal part of the errorbar is.
                position=position_dodge(.9)) #0.9 always centers the error bars correctly.

#Let's make a figure with three panels, one for each location. Use the facet_grid() or facet_wrap() commands. Both have interesting functions.
ggplot(sumdat2, aes(x=ftrt, y=seedyld))+
  facet_wrap(~location)+ #use one or the other, facet_wrap or facet_grid. Read the internet about pros and cons to both.
  #facet_grid(~location, col=1)+
  geom_bar(position=position_dodge(), #puts the bars next to each other
           stat="identity") +
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se),
                width=0, #This changes how wide the horizontal part of the errorbar is.
                position=position_dodge(.9)) #0.9 always centers the error bars correctly.

#There are so many ways to customize your plots. 
ggplot(sumdat, aes(x=Nfertnew, y=seedyld, color=location))+
  geom_line()+ #order doesn't usually matter, but sometimes. geom_line simply connects the points
  geom_point()+
  stat_smooth(linetype=2,method = lm, formula = y ~ x + I(x^2), size = 1, se=FALSE)+ #I also changed the linetype and size in here. Note that it's applied to all the levels of your factor. Putting it in aes() would give you a unique linetype or size for each factor level (location in this case)
  geom_errorbar(alpha = 0.5, width=0.9, aes(ymin=seedyld-se, ymax=seedyld+se))+ #alpha makes things more transparent
  xlab(expression("N fertilizer rate " ~ ("kg N" ~ ha^{-1})))+ #use the expression() function to exponentiate characters in x axis label.
  ylab("Growth index")+
  scale_x_continuous(limits=c(0,200), breaks=c(0,50,100,150,200))+ #Control x axis 
  ylim(c(0,1300))+
  scale_color_manual(name="Location", values=c(1:3), labels=c("Lamberton", "Morris", "Waseca"))+ #This is important. This gives you control of the aes parameter that controls how your data are separated. If you adjust the "color" option in the aes, then it's scale_color_manual. If you adjust the fill option, like we did for the bar graph, then it's scale_fill_manual. Note that you can relable your levels and retitle the legend here.
  theme( #This is where you can control many of the plot details. 
    legend.key = element_rect(colour = NA, fill = NA),
    legend.position=c(.75,.25),# the numbers are % of x and y axis. So c(0.1,0.1) would place the legend in the bottom left hand corner
    legend.text=element_text(size=12),
    legend.title=element_blank(), #turn this on and off with comment to see how it works
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12, color="red"),
    axis.title.x=element_text(size=20),
    axis.title.y = element_text(size=12))

#Let's go back to N fertilizer as a continuous variable. We'll work with sumdat
#ggplot can fit functions to the data without having to create the linear model outside ggplot
library(nlme)
library(ggplot2)
ggplot(sumdat, aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point()+
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se), width=0.2)+
  stat_smooth(method = lm, formula = y ~ x + I(x^2), se=FALSE)

#or a linear line, and this time include the se 
ggplot(sumdat, aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point()+
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se), width=0.2)+
  stat_smooth(method = lm, formula = y ~ x, se=F) #see the difference in syntax here?

#It looks like a curved function is going to fit best. However, we'll fit a linear function as well just
#to test it out.

#First, we'll fit equations for each location, treating them independently.
#Then, we'll treat location as a random effect and find the best fitting equation to model
#yield responses to N across all three locations.

#1. Fitting equation for locations independently, with rep as random effect
library(nlme)
mod1<-lme(seedyld~Nfertnew*location, random=~1|rep, data=dat)# linear model
mod2<-lme(seedyld~(Nfertnew+I(Nfertnew^2))*location, random=~1|rep, data=dat) #quadratic model. Note the weird notation for the quadratic term
anova(mod1) 
anova(mod2)

summary(mod1)
summary(mod2)
#There's no N rate X location interaction for either model, so I wouldn't treat each 
#location independently, but for demonstration we'll do it anyway.

#Subset the locations
Lamdat<-droplevels(subset(dat, location=="Lam")) #always droplevels() when subsetting, sometimes the old levels end up in the new dataset.
Wasdat<-droplevels(subset(dat, location=="Was"))
Mordat<-droplevels(subset(dat, location=="Mor"))

#Manually create two models, a linear and quadratic, for each location; I'll just do lamberton
Lammod1<-lme(seedyld~Nfertnew, random=~1|rep, data=Lamdat)
Lammod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|rep, data=Lamdat)

#Compare using liklihood ratio test
anova(Lammod1, Lammod2) # Note that REML (restricted maximum liklihood) methods are not meaningful, 
#so switch to ML (maximum liklihood)

Lammod1.ml<-update(Lammod1, method="ML")
Lammod2.ml<-update(Lammod2, method="ML")
anova(Lammod1.ml, Lammod2.ml)
#Lammod2.ml, the quadratic model, is far superior. AIC is much lower, and L.ratio is significant based on alpha=0.05

#Now the tough part, identifying the AONR, or the x value of this curve where y value peaks.
#The way to do this is to set the derivitive of the function to 0 (no slope, or flat), and solve for x
#Let's say our quadratic model is y = alpha + beta*x + gamma*x^2 (where alpha is y-intercept and beta and gamma are other coefficients)
#setting the derivitive of this function to 0 would look like: 0 = beta + 2*gamma*x
#then x = (1/(2*gamma))-beta. Let's call this x value ANOR. So we'll rearrange the equation to solve for beta.
#beta = -2*gamma*AONR. Now substitute this for beta in our original quadratic equation so that the AONR is in it
#y = alpha - ((2*gamma*AONR)*x) + gamma*x^2   This is our new equation with AONR as a coefficient. R can solve for this
#coefficient and provide an estimate of variance for that coefficient.

#Use the nls() function to fit your data to the new equation above.
#first, define the function
qmmod<-function(x, alpha, AONR, gamma){
  alpha - ((2*gamma*AONR)*x)+(gamma*x^2)
}

#Use nls. The tricky part is that you need to provide an estimate of what the coefficients will be to help
#R know where to start searching. 
Lammod.q<-nls(seedyld ~ qmmod(Nfertnew, alpha, AONR, gamma), data=Lamdat, 
    start = list(alpha = 700, AONR = 60, gamma = -0.01), #by looking at the figure, we can see that the y intercept (or alpha) is about 800, the peak of the curve might be around 60.
    control = list(maxiter=200))                        #The third coefficient, gamma, is trickier to guess. I use trial and error, but now I know it's usually around -0.01

summary(Lammod.q) #It works; R gives us an estimate of 77.3 kg N per ha for maximizing grain yields. Let's plot

#However, this does not have our random effects in it. You can use nlme() to fit a random effects model with
#custom functions
ranLammod.q<-nlme(seedyld ~ qmmod(Nfertnew, alpha, AONR, gamma),
     data=Lamdat,
     fixed=alpha+AONR+gamma~1,
     random=alpha~1|rep,
     method="ML",
     control=nlmeControl(pnlsTol = 0.1, verbose=T),
     start=c(alpha=700, gamma=-0.01, AONR=60),
     na.action=na.omit)
summary(ranLammod.q) #Very similar results for fixed effects coefficients, and we see that the variation explained
#by our random effect is very small.

#create a fake dataset to fit your new function to. Make sure the column names are exactly the same as your original data.
#You need all the columns too, including random effect.
plotdat<-data.frame(expand.grid(Nfertnew=c(0:200), rep=NA, location=NA,  seedyld=NA))
plotdat$seedyld<-predict(ranLammod.q, plotdat, level=0) #use your model to predict seed yield at all N fert levels
#between 0 and 200, and add those predictions to your new fake data frame.

#what we're really interested in is the seed yield estimate at the predicted AONR
lamaonr<-ranLammod.q$coefficient$fixed[2] #This pulls out the second fixed effect coefficient from the model, or our AONR
AONRpt<-data.frame("AONR"=lamaonr,
                   "AONRse"=summary(ranLammod.q)$tTable[2,2], #This extracts the Std. Error of AONR
                   "seedyld"= predict(ranLammod.q, data.frame(Nfertnew=lamaonr), level=0))

#Make the same plot as before, but we'll add the new line in the fake data set with predictions
ggplot(sumdat, aes(x=Nfertnew, y=seedyld))+
  geom_point(aes(color=location))+
  geom_errorbar(aes(ymin=seedyld-se, ymax=seedyld+se, color=location), width=0.2)+
  geom_line(data=plotdat, aes(color=location))+ 
  geom_point(data=AONRpt, aes(x=AONR, y=seedyld), shape=4, size=8, alpha=.5)+ # the X indicates the ANOR and estimated yield
  geom_segment(data=AONRpt, aes(x=AONR-AONRse, y=seedyld, xend=AONR+AONRse, yend=seedyld), 
               alpha=.5, linetype=1, size=2, color="black") #The line indicates the AONR range based on standard error

library(pgirmess) #need this for next function
#Here's a function to do this easily, but now I'm including location as random effect, with rep nested in each location
qmmod<-function(x, alpha, AONR, gamma){
  alpha - ((2*gamma*AONR)*x)+(gamma*x^2)
}

auto.aonr<-function(dataset){ 
  m1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=dataset, na.action=na.omit, method="ML")
  m2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=dataset, na.action=na.omit, method="ML")
  if(selMod(list(m1,m2))[1,3]>selMod(list(m1,m2))[2,3]){ #if model 2 has a lower AICc, the make the nls model and print
    grdat<-dataset[,c("location","plot","rep","Nfertnew","seedyld")]
    grdat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=grdat,
                       labels=list(x='Nitrogen rate', y="Grain Yield"),
                       units=list(x="(kg2/ha)", y="(kg2/ha)"))
    
    m3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, AONR, gamma),
             data=grdat,
             fixed=alpha+AONR+gamma~1,
             random=alpha~1|location/rep,
             method="ML",
             control=nlmeControl(pnlsTol = 0.01, verbose=T, maxIter=1000),
             start=c(alpha=800, gamma=-0.01, AONR=100),
             na.action=na.omit)
    
    AONRtab<-data.frame("AONR"=m3$coefficient$fixed[2],
                       "AONRse"=summary(m3)$tTable[2,2], #This extracts the Std. Error of AONR
                       "seedyld"= predict(m3, data.frame(Nfertnew=m3$coefficient$fixed[2]), level=0),
                       "Model Type"="Quadratic")
    print(AONRtab)
  }else{ #if the linear model is significantly better, just print those results
    AONRtab<-data.frame("AONR"=NA,
                        "AONRse"=NA,
                        "seedyld"= NA,
                        "Model Type"="Linear",
                        "Equation"=print("Y = ", m1$coefficients[1], " + X", m1$coefficients[2]))
  }
}

auto.aonr(dat) #Run the function, here we see that the AONR is 62.89 kg N ha and predicted seed yield is 958 kg ha
#when using data from all locations and treating location as a random effect.
#You could use this function for other datasets as long as they have the same variables and those variables are named the same



#####That's great, but what if yields plateau and don't drop off with high N rates?
#What if a quadratic function isn't optimal?

odat<-read.table("Otherdat.txt", header=T)
head(odat); str(odat)
odat$fyear<-as.factor(odat$year);odat$fplot<-as.factor(odat$plot)
odat$fNtrt<-as.factor(odat$Ntrt); odat$frep<-as.factor(odat$rep)

#Subset the data for simplicity
Aus09<-droplevels(subset(odat, odat$year=="2009"&odat$site=="Aus"))
plot(Aus09$dm ~ Aus09$Ntrt)
#The square root quadratic plateau model has a nice plateau
#The square root quadratic model is y = alpha + beta*x + gamma*x^0.5 (where alpha is y-intercept and beta and gamma are other coefficients)
#setting the derivitive of this function to 0 would look like: 0=beta+(gamma/(2*sqrt(x)))
# then solve for x, which is the point where y plateaus: x = ((-gamma)/(2*beta))^2
#Rename the x value here as AONR; then rearrange equation to solve for beta
#beta = -gamma/(2*sqrt(AONR)). Now substitute this for beta in our original quadratic equation so that the AONR is in it
#y = alpha - (-gamma/(2*sqrt(AONR))*x) + gamma*x^2   
#Simplify the equation: y = alpha+(0.5*gamma/AONR^0.5)*x + gamma*x^2
#This is our new equation with AONR as a coefficient. R can solve for this
#coefficient and provide an estimate of variance for that coefficient.

#Use the nls() function to fit your data to the new equation above.
sqrtmod<-nls(dm ~ alpha - (((.5*gamma)/sqrt(AONR))*Ntrt)+(gamma*Ntrt^.5), data=Aus09, 
                start = list(alpha = 4, AONR = 120, gamma = 140),
                control = list(maxiter=200))
#Use nls. The tricky part is that you need to provide an estimate of what the coefficients will be to help
#R know where to start searching. 
#This isn't too tricky the way that the equation is arranged now. alpha is the y intercept, so look at
#the plot to figure out about where that should be. You should also be able to give a good guess at what
#the AONR should be. Here I guessed 120
coef(sqrtmod)
#Or you can make a function and call that in nls()
sqrtfun<-function(x, alpha, AONR, gamma){
  alpha - (((.5*gamma)/sqrt(AONR))*x)+(gamma*x^.5)
}

#Use nls. The tricky part is that you need to provide an estimate of what the coefficients will be to help
#R know where to start searching. 
sqrtmod2<-nls(dm ~ sqrtfun(Ntrt, alpha, AONR, gamma), data=Aus09, 
              start = list(alpha = 4, AONR = 100, gamma = 100), #by looking at the figure, we can see that the y intercept (or alpha) is about 800, the peak of the curve might be around 60.
              control = list(maxiter=200))                        #The third coefficient, gamma, is trickier to guess. I use trial and error, but now I know it's usually around -0.01
coef(sqrtmod2)
