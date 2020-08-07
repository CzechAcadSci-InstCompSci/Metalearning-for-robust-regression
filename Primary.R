### PRIMARY LEARNING for a given x,y; one regressor vs. several: modify leavek(); 
### robust prediction error measure in included here
library(MASS); library(robustbase); #library(rlm);

volej=function()### main
{n=length(y);
myout=leave1(x,y,n)$outcv; 
  #print("in volej, myout"); print(myout);
### MSE
  main1=apply(myout,2,mean);
### TMSE, 0.9
  h=floor(0.9*n);
  short=apply(myout,2,sort);
  short=short[1:h,]; 
  main2=apply(short,2,mean);
### TMSE, 0.5
  h=floor(0.5*n);
  short=apply(myout,2,sort);
  short=short[1:h,]; 
  main3=apply(short,2,mean);
print("final results"); 
print("MSE"); print(main1); print(rank(main1));
print("TMSE 0.9"); print(main2); print(rank(main2));
print("MSE 0.5"); print(main3); print(rank(main3));
}#volej

leave1=function(x,y,n) #leave-k-out
{outcv=matrix(0, nrow=n, ncol=5);
for (my in 1:n) #all indexes sequentially considered
  {#print("leave1, hello");
   xx=x[-my,]; yy=y[-my]; 
   novex=x[my,]; novey=y[my];
   outcv[my,]=predikuj(xx,yy,novex,novey)$outvec; 
   #print("leave1, end"); print(c(my,outcv[my,]));
  }#for
list(outcv=outcv)
}#leavek

mira=function(b,y) ### quality fit measure comparing y with vector b
{out=(y-b)^2;
#print("mira");
list(out=out)
}#mira

predikuj=function(a,b,novex,novey)
{#print("predikuj, hello");
outvec=rep(-1,5); #init.
xd=data.frame(a); #usek castych chyb
newx=matrix(novex, nrow=1);
### OLS
  my1=lm(b~., xd);
  #print("xd"); print(xd); print("b"); print(b); print("my1"); print(my1); print("xnew"); print(novex);
  pom=predict(my1, newdata=data.frame(newx)); 
  #print("in predikuj, pom"); print(pom);   
  outvec[1]=mira(pom, novey)$out;
  #print("outvec1"); print(outvec);
### Huber M-estimator 
  my2=rlm(b~., data=xd, psi=psi.huber);
  pom=predict(my2, newdata=data.frame(newx));
  outvec[2]=mira(pom,novey)$out;
### Hampel redescending M-estimator  
  my3=rlm(b~., data=xd, psi=psi.hampel);
  pom=predict(my3, newdata=data.frame(newx));
  outvec[3]=mira(pom,novey)$out;
### LTS with fixed h=0.5
  init=ltsReg(b~a, alpha=0.5);
  h=init$quan; #rounded     
  w=rep(0,length(b));
  for (k in 1:h)
    {j=init$best[k];
    w[j]=1;}
  my4=lm(b~., data=xd, weights=w);
  pom=predict(my4, newdata=data.frame(newx));
  outvec[4]=mira(pom,novey)$out;
### LTS with fixed h=0.75
  init=ltsReg(b~a, alpha=0.75);
  h=init$quan; #rounded     
  w=rep(0,length(b));
  for (k in 1:h)
    {j=init$best[k];
    w[j]=1;}
  my5=lm(b~., data=xd, weights=w);
  pom=predict(my5, newdata=data.frame(newx));
  outvec[5]=mira(pom,novey)$out;
### navic, vybrat jen nektere metody:
#out=rep(-1,3); out[1]=outvec[1]; out[2]=min(outvec[2], outvec[3]); out[3]=min(outvec[4], outvec[5]); 
#print("predikuj, end, outvec"); print(outvec);
list(outvec=outvec)
}#predikuj

### correct prediction given x,y:
#xd=data.frame(x);
#my=lm(y~., xd);
#xnew=matrix(x[10,], nrow=1);
#xnewd=data.frame(xnew);
#out=predict(my, newdata=xnewd);