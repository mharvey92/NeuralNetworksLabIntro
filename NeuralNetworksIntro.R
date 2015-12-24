# This code is part of a lab that is taken from Louis Aslett's website

library(nnet)

cheese<-read.csv("cheese.csv")

#data summary
names(cheese)
summary(cheese)
head(cheese)

#fit simple neural net
fitnn1= nnet(taste ~ Acetic+H2S+Lactic, cheese, size=0, skip=TRUE, linout=TRUE)
summary(fitnn1)

#compare to linear model
lm(taste ~ Acetic+H2S+Lactic, cheese)

#computing Akaike Information Criterion given that sample size is small (n=30)
SSE=sum(fitnn1$residuals^2)
AIC=2*5+30*log(SSE/30)
AIC

#computing Schwartz Bayesian Information Criterion
SBIC=5*log(30)+30*log(SSE/30)
SBIC

#create second model without variable "Acetic"
fitnn2=nnet(taste~H2S+Lactic, cheese, size=0, skip=TRUE, linout=TRUE)
summary(fitnn2)

#compare fitnn1 to fitnn2
SSE2=sum(fitnn2$residuals^2)
AIC2=2*5+30*log(SSE2/30)
AIC2<AIC

#adding hidden layer w/ single node
fitnn3=nnet(taste ~ Acetic +H2S+Lactic, cheese, size=1, linout=TRUE)

#normalize data for robustness
cheesescaled=scale(cheese)
fitnn3=nnet(taste ~ Acetic +H2S+Lactic, cheesescaled, size=1, linout=TRUE)
summary(fitnn3)

#plot nnet model 3
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


plot.nnet(fitnn3,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,circle.col='brown')
