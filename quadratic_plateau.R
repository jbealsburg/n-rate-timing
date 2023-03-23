# https://agronomy4future.org/?p=17203 



# This is the full code to generate the above graph. You can simply copy and paste this code in your R console to obtain the graph. 

# to uplopad data
library (readr)

github<- "https://raw.githubusercontent.com/agronomy4future/raw_data_practice/main/sulphur%20application.csv"

dataA<-data.frame(read_csv(url(github)))

# to find reasonable initial values for parameters
fit.lm<-lm(yield ~ poly(sulphur,2, raw=TRUE),data=dataA)
a_parameter<-fit.lm$coefficients[1]
b_parameter<-fit.lm$coefficients[2]
c_parameter<-fit.lm$coefficients[3]
x_mean<-mean(dataA$sulphur)

# to define quadratic  plateau function
# a = intercept
# b = slope
# c = quadratic term (curvy bit)
# jp = join point = break point = critical concentration

qp <- function(x, a, b, jp) {
  c <- -0.5 * b / jp
  if_else(condition = x < jp,
          true  = a + (b * x) + (c * x * x),
          false = a + (b * jp) + (c * jp * jp))
}

# to find the best fit parameters
library(dplyr, warn.conflicts = FALSE)

model <- nls(formula=yield ~ qp(sulphur, a, b, jp),
             data=dataA,
             start=list(a=a_parameter, b=b_parameter, jp=x_mean))

summary(model)

# to generate a graph
library(ggplot2)
library(nlraa)
library(minpack.lm)

dataA %>% 
  ggplot(aes(sulphur, yield)) +
  geom_point(size=4, alpha = 0.5) +
  geom_line(stat="smooth",
            method="nls",
            formula=y~SSquadp3xs(x,a,b,jp),
            se=FALSE,
            color="Dark red") +
  geom_vline(xintercept=25.0656, linetype="solid", color="grey") +
  annotate("text", label=paste("sulphur=","25.1","kg/ha"),
           x=25.2, y=1000, angle=90, hjust=0, vjust=1.5, alpha=0.5)+
  labs(x="Sulphur application (kg/ha)", y="Yield (kg/ha)") +
  theme_grey(base_size=15, base_family="serif")+
  theme(legend.position="none",
        axis.line=element_line(linewidth=0.5, colour="black")) 
