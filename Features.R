######################################## Computing all features for a single dataset
fea=function()
{n=length(y);
library(moments); library(lmtest);
f1=n;
f2=NCOL(x); #p
f3=f2/f1;
### regression
  my1=lm(y~x);
  a=0.5;
  pom=ltsReg(y~x, alpha=a);
  h=pom$quan; #rounded     
  w=rep(0,n);
  for (i in 1:h)
    {j=pom$best[i];
    w[j]=1;}
  my4=lm(y~x, weights=w);
###
f4= shapiro.test(my1$resid)$p.value;
f5=skewness(my1$resid);
f6=kurtosis(my1$resid);
f7=summary(my1)$r.squared;
### reweighted LTS; approach of Rousseuw and Leroy
#pom=ltsReg(y~x); this keeps a fixed h
res=my4$resid;
sd=sd(res);
f8=sum(abs(res/sd) > 2.5)/n;#percentage of outliers
f9=bptest(my1)$p.value; #Breusch Pagan test
pom=adjOutlyingness(x);
f10=sum(pom$nonOut); #ratio, manually
features=c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10);
print(features);  
}#fea