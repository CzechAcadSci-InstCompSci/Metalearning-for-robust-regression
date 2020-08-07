### first dataset starting the analysis
### reading the data sets; this may be useful for a variety of other tasks than only metalearning
### the datasets were dilligently found so that the regression task is meaningful
### the dataset do not contain any missing values
### some of the datasets are publicly available 
### the user may use another database of training datasets
library(robustbase)
### aircraft
  y<-as.vector(aircraft$Y)
  x<-matrix(c(aircraft$X1, aircraft$X2, aircraft$X3, aircraft$X4), ncol=4, nrow=23) 
### ammonia
  ammonia<-read.table("W:/OMIB/pestova/RobustRegression/ammonia.txt")
  y<-as.vector(ammonia$V5) #B: the percentage of unprocessed ammonia
  x<-matrix(c(ammonia$V2, ammonia$V3, ammonia$V4), ncol=3, nrow=21)
### auto-mpg
  auto.mpg<-read.table("W:/OMIB/pestova/RobustRegression/auto-mpg.data",na.strings="?")  
  y<-as.vector(auto.mpg$V1) #mpg variable
  x<-matrix(c(auto.mpg$V3, auto.mpg$V4, auto.mpg$V5, auto.mpg$V6), ncol=4, nrow=398);
  xx=cbind(x,y); nc=dim(x)[2]; pom=na.omit(xx); x=pom[,1:nc]; y=pom[,nc+1]; colnames(x)=c(1,2,3,4);
cirrhosis<-read.table("W:/OMIB/pestova/RobustRegression/cirrhosis.txt")
  y<-as.vector(cirrhosis$V7) #B: the death rate from cirrhosis
  x<-matrix(c(cirrhosis$V3, cirrhosis$V4, cirrhosis$V5, cirrhosis$V6),ncol=4, nrow=46) 
### coleman
  y<-as.vector(coleman$Y)
  x<-matrix(c(coleman$salaryP, coleman$fatherWc, coleman$sstatus, coleman$teacherSc, coleman$motherLev), ncol=5, nrow=20) 
### delivery
  y<-as.vector(delivery$delTime)
  x<-matrix(c(delivery$n.prod, delivery$distance), ncol=2, nrow=25) 
### education
  y<-as.vector(education$Y)
  x<-matrix(c(education$X1, education$X2, education$X3), ncol=3, nrow=50) 
electricity<-read.table("W:/OMIB/pestova/RobustRegression/electricity.txt")
  y<-as.vector(electricity$V6) #B: the output of electricity in millions of kilowatts annum
  x<-matrix(c(electricity$V3, electricity$V4, electricity$V5), ncol=3, nrow=16) 
employment<-read.table("W:/OMIB/pestova/RobustRegression/employment.txt")
  y<-as.vector(employment$V9) #B: the number of people employed
  x<-matrix(c(employment$V3, employment$V4, employment$V5,employment$V6, employment$V7, employment$V8), ncol=6, nrow=16) 
houseprices<-read.table("W:/OMIB/pestova/RobustRegression/house-prices.txt")
  y<-as.vector(houseprices$V14) #B: the selling price
  x<-matrix(c(houseprices$V3, houseprices$V5, houseprices$V6, houseprices$V8, houseprices$V10), ncol=5, nrow=28) 
housing<-read.table("W:/OMIB/pestova/RobustRegression/housing.data",na.strings="?")
  y<-as.vector(housing$V1) #CRIM: per capita crime rate by town 
  x<-matrix(c(housing$V6,housing$V7, housing$V8, housing$V13, housing$V14), ncol=5, nrow=506) 
imports<-read.table("W:/OMIB/pestova/RobustRegression/imports.txt")
  y<-as.vector(imports$V7) #B: the levels of imports
  x<-matrix(c(imports$V4, imports$V5, imports$V6), ncol=3, nrow=18) 
### kootenay
  y<-as.vector(kootenay$Newgate)
  x<-matrix(c(kootenay$Libby), ncol=1, nrow=13) 
livestock<-read.table("W:/OMIB/pestova/RobustRegression/livestock.txt")
  y<-as.vector(livestock$V7) #B: expenses in thousands of dollars
  x<-matrix(c(livestock$V3, livestock$V4, livestock$V5, livestock$V6), ncol=4, nrow=19) 
machine<-read.table("W:/OMIB/pestova/RobustRegression/machine.data",sep=",")
  y<-as.vector(machine$V9) #PRP: published relative performance (integer)
  x<-matrix(c(machine$V3, machine$V4, machine$V5, machine$V6,machine$V7, machine$V8), ncol=6, nrow=209) 
murders<-read.table("W:/OMIB/pestova/RobustRegression/murders.txt")
  y<-as.vector(murders$V5) #B: the number of murders per 1,000,000 inhabitants per annum
  x<-matrix(c(murders$V2, murders$V3, murders$V4), ncol=3, nrow=20) 
### NOx emissions
  y<-as.vector(NOxEmissions$LNOx)
  x<-matrix(c(NOxEmissions$julday, NOxEmissions$LNOxEm, NOxEmissions$sqrtWS), ncol=3, nrow=8088) 
octane<-read.table("W:/OMIB/pestova/RobustRegression/octane.txt")
  y<-as.vector(octane$V7) #B: the octane rating
  x<-matrix(c(octane$V3, octane$V4, octane$V5, octane$V6), ncol=4, nrow=82) 
pasture<-read.table("W:/OMIB/pestova/RobustRegression/pasture.txt")
  y<-as.vector(pasture$V5) #B: the rental price per acre for this variety of grass
  x<-matrix(c(pasture$V2, pasture$V3, pasture$V4), ncol=3, nrow=67) 
### pension
  y<-as.vector(log(pension$Reserves))
  x<-matrix(c(log(pension$Income)), ncol=1, nrow=18) 
petrol<-read.table("W:/OMIB/pestova/RobustRegression/petrol.txt")
  y<-as.vector(petrol$V7) #B: the consumption of petrol
  x<-matrix(c(petrol$V3, petrol$V4, petrol$V5, petrol$V6), ncol=4, nrow=48) 
### stars
  y<-as.vector(starsCYG$log.Te)
  x<-matrix(c(starsCYG$log.light), ncol=1, nrow=47) 
### wood
  y<-as.vector(wood$y)
  x<-matrix(c(wood$x1,wood$x2,wood$x3,wood$x4,wood$x5), ncol=5, nrow=20) 
### data set contains matrix x and vector y
### rows of x are observations
xx=cbind(x,y); nc=dim(x)[2];
pom=na.omit(xx); x=pom[,1:nc]; y=pom[,nc+1];

my=lm(y~x)
var(my$res)