####### we work with a single dataset; compare various regression estimators, 
### auxiliary file: only autovalidation, instead of cross validation

runauto=function()
{n=length(y);
library(MASS);
  library(robustbase);
mira=function(b,y) ### quality fit measure comparing y with vector b
  {out=sum((y-b) * (y-b)); 
  list(out=out)
}#mira
### least squares
  my1=lm(y~x);
  out1=mira(my1$fitted,y)$out;
### Huber M-estimator 
  #library(robustreg);
  #my2=robustRegH(y~x); #no experience
  my2=rlm(y~x, psi=psi.huber);
  out2=mira(my2$fitted,y)$out;
### Hampel redescending M-estimator  
  my3=rlm(y~x, psi=psi.hampel);
  out3=mira(my3$fitted,y)$out;
### LTS with default setting
  #my=ltsReg(y~x); # this computes RLS, final h can be small or large 
### LTS with fixed h=0.5
  a=0.5;
  pom=ltsReg(y~x, alpha=a);
  h=pom$quan; #rounded     
  w=rep(0,n);
  for (i in 1:h)
    {j=pom$best[i];
    w[j]=1;}
  my4=lm(y~x, weights=w);
  out4=mira(my4$fitted,y)$out;
### LTS with fixed h=0.75
  a=0.75;
  pom=ltsReg(y~x, alpha=a);
  h=pom$quan; #rounded     
  w=rep(0,n);
  for (i in 1:h)
    {j=pom$best[i];
    w[j]=1;}
  my5=lm(y~x, weights=w);
  out5=mira(my5$fitted,y)$out;
results=c(out1,out2,out3,out4,out5);
print(results); 
rank(results)
}#runauto
################################################################
### plot only for one regressor
#plot(x,y, pch=19);
#points(x, my1$coef[1]+my1$coef[2]*x, col="red");
#points(x, my2$coef[1]+my2$coef[2]*x, col="cyan");
#points(x, my3$coef[1]+my3$coef[2]*x, col="darkmagenta");
#points(x, my4$coef[1]+my4$coef[2]*x, col="green");
#points(x, my5$coef[1]+my5$coef[2]*x, col="navy");

