

## ########################################################
########################################################
########################################################
#                  --------------                      #
#        -------* SENSITIVITY ANALYSIS *-------------- #
#                 ---------------                      #
########################################################
########################################################
########################################################

library("sensitivity")
X<- M[,c("mu_a", "mu_cm", "mu_cs", "RRm", "gamma", "delta", "alpha",  "beta",   "RRp")] #,    "wam",    "was",    "wcm",    "wcs")]
y<- M[,"DALY"]
letters<-c(expression(paste(mu,'a')), expression(paste(mu,'cm')), expression(paste(mu,'cs')),
          expression(RRm), expression(gamma), expression(delta), expression(alpha), expression(beta),
          expression(RRp))
names(X)<- letters
         # , expression(wam), expression(was), expression(wcm), expression(wcs))

## Partial Rank Correlation Coeficient
library(epiR)
dat<- data.frame(cbind(X, Y = y))
P<- epi.prcc(dat, sided.test = 2)
cols <- c("black","red")
pos <- (P[,1]) >= 0
barplot((P[,1]), col = cols[pos + 1], border = cols[pos + 1], xlim=c(-1,1), horiz=T,
        names=letters, main="DALYs", las=2, xlab="PRCC")


