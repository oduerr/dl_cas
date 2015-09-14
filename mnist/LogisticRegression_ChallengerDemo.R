library(manipulate)
library(ggplot2)
library(ggvis)
library(alr3) 
attach(challeng)
Failure = ifelse(Fail > 0, 1, 0) # We only consider failures
challenger = data.frame(Temp, Failure)
qplot(Temp, Fail)
qplot(Temp, Failure)

erg<-glm(Failure~Temp,family=binomial(logit), data = challenger)
a<-erg$coefficients[1]
b<-erg$coefficients[2]

x<-seq(30,100,0.1)

manipulate(
  {
    fit = data.frame(x = x, y = exp(a+b*x)/(1+exp(a+b*x)))
    ggplot(data = challenger) + 
      geom_point(aes(x = Temp, y=Failure), alpha=0.9) + 
      geom_line(data = fit, aes(x = x, y = y), col='blue')
  },
  a = slider(0,20,a,step=0.1),
  b = slider(-1,2,b,step=0.02)
)



# Not working yet
# fit = data.frame(x = x, y = exp(a+b*x)/(1+exp(a+b*x)))
# g = ggvis(challenger, x = ~Temp, y=~Failure)  
# layer_points(g, opacity := 0.5) 

