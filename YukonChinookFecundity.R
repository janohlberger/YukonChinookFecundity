##=======================================================================##
## Source: Ohlberger et al. 2020 (https://doi.org/10.1139/cjfas-2020-0012) 
## Regressions of female length vs. reproductive output in Chinook salmon
## Samples taken in the Yukon River at the Eagle site during 2008-2010
## All data collected by Lara Horstmann, University of Alaska Fairbanks

##=======================================================================##
pkgs<-c("minpack.lm")
if(length(setdiff(pkgs,rownames(installed.packages())))>0) { install.packages(setdiff(pkgs,rownames(installed.packages())),dependencies=T) }
invisible(lapply(pkgs,library,character.only=T))

source("try-many-inits-function.R") ## function to try many initial values
FEC<-function(L,a,b) { a*L^b } ## non-linear power function 
data<-read.csv(file="YukonChinookFecundity.csv")[,-1] ## read data file

## regression of egg number vs fish length 
x<-data$Length;log_x<-log(x)
y<-data$Fecundity;log_y<-log(y)
mod<-try_many_inits(data.frame(x=x,y=y)) ## try fits using nlsLM()
coeffs1<-summary(mod)$coefficients
sig1<-sigma(mod)

## regression of egg mass versus fish length
x<-data$Length;log_x<-log(x)
y<-data$OvaryMass;log_y<-log(y)
mod<-try_many_inits(data.frame(x=x,y=y)) 
coeffs2<-summary(mod)$coefficients
sig2<-sigma(mod)

##=======================================================================##