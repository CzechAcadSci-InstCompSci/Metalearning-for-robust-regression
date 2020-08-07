### secondary learning of the metalearning tasks
library(e1071); library(MASS); library(class);

### various choices of the response
#y=c(1,5,1, 1,5,4, 2,5,2, 1,1,4, 3,1,2, 1,1,4, 1,1,2); #a
#y=c(1,3,1, 1,3,3, 2,3,2, 1,1,3, 2,1,2, 1,1,3, 1,1,2); #b
#y=c(1,2,1, 1,2,2, 1,2,1, 1,1,2, 1,1,1, 1,1,2, 1,1,2); #c
#y=c(5,5,2, 1,3,3, 5,2,2, 1,5,5, 4,3,3, 1,3,4, 5,2,2); #d
#y=c(3,3,2, 1,2,2, 3,2,2, 1,3,3, 3,2,2, 1,2,3, 3,2,2); #e
#y=c(2,2,1, 1,1,1, 2,1,1, 1,2,2, 2,1,1, 1,1,2, 2,1,1); #f
#y=c(1,4,5, 2,2,2, 5,5,2, 1,4,5, 5,5,1, 3,5,4, 5,4,4); #g
#y=c(1,3,3, 2,2,2, 3,3,2, 1,3,3, 3,3,1, 2,3,3, 3,3,3); #h
y=c(1,2,2, 1,1,1, 2,2,1, 1,2,2, 2,2,1, 1,2,2, 2,2,2); #i

### the user needs to have the values of the features in the matrix 'a'
a=matrix(-1, nrow=21, ncol=10);
a[1,]=c(23 , 4, 0.17, 0.69, 0.21, 3.02, 0.88, 0.04, 0.07, 0); #aircraft
a[2,]=c(21 , 3 , 0.14 , 0.82 , -0.19, 3.11 , 0.91 , 0 , 0.18, 0); #ammonia
a[3,]=c(392 , 4 , 0.01, 0, 0.71, 4.05, 0.71, 0.03, 0, 0.02); #autompg
a[4,]=c(46, 4, 0.09, 0.11, -0.21, 2.07, 0.81, 0, 0.61, 0);
a[5,]=c(20, 5, 0.25, 0.15, 0.51, 5.09, 0.91, 0.05, 0.33, 0);
a[6,]=c(25, 2, 0.08, 0.27, 0.03, 3.07, 0.96, 0.04, 0.00, 0.08);
a[7,]=c(50, 3, 0.06, 0.93, 0.26, 2.71, 0.59, 0.02, 0.00, 0);
a[8,]=c(16, 3, 0.19, 0.22, 0.78, 3.84, 0.92, 0.06, 0.13, 0);
a[9,]=c(16, 6, 0.38, 0.48, 0.42, 2.44, 1.00, 0, 0.87, 0);
a[10,]=c(28, 5, 0.18, 0.26, 0.20, 2.22, 0.93, 0.04, 0.47, 0.07);
a[11,]=c(18, 3, 0.17, 0.38, 0.27, 2.12, 0.97, 0.11, 0.03, 0.06);
a[12,]=c(13, 1, 0.08, 0.88, 0.04, 2.27, 0.00, 0.08, 0.29, 0);
a[13,]=c(19, 4, 0.21, 0.77, 0.42, 2.76, 0.94, 0.05, 0.54, 0);
a[14,]=c(209, 6, 0.03, 0, 1.50, 14.0, 0.86, 0.03, 0, 0.03);
a[15,]=c(20, 3, 0.15, 0.14, 0.68, 3.18, 0.82, 0, 0.34, 0);
a[16,]=c(82, 4, 0.05, 0.72, 0.12, 2.54, 0.91, 0.01, 0.58, 0.06);
a[17,]=c(67, 3, 0.04, 0.44, 0.45, 3.90, 0.86, 0.04, 0.01, 0.06);
a[18,]=c(18, 1, 0.06, 0.94, 0.004, 2.23, 0.84, 0, 0.41, 0);
a[19,]=c(48, 4, 0.08, 0.02, 1.06, 5.30, 0.68, 0.06, 0.16, 0.02);
a[20,]=c(47, 1, 0.02, 0, -1.36, 4.23, 0.04, 0.09, 0, 0);
a[21,]=c(20, 5, 0.25, 0.64, 0.51, 2.69, 0.81, 0.05, 0.16, 0);
pp=dim(a)[2];
#male=a[,1];

### various classifiers are applied to the classification task
### including several well known classifiers
mylda=function(loc)
{### autovalidation
 # my=lda(y~loc);
 # sm=predict(my,data.frame(loc));
 # w=sum(sm$class==y); 
### leave-one-out
#print("LDA");
q=lda(y~loc, CV=TRUE);
print(y); print(loc);
#print("after q"); print(q);
w=sum(q$class==y); 
print(q$class);
print("in LDA"); print(w);
list(out=w);
}#mylda

mysvm=function(loc)
{#my=svm(y~loc, kernel="linear", type="C"); #"C" for classif.
 #sm=predict(my,data.frame(loc));
 #w=sum(sm==y); 
### leave-one-out
  q=svm(y~loc, kernel="linear", type="C", cross=1); #to nic nedava
n=length(y);
out=rep(-1,n);
for (i in 1:n)
  {podx=loc[-i,]; pody=y[-i];
   my=svm(pody~podx, kernel="linear", type="C");
   b=loc[1:(n-1),]; b[1,]=loc[i,]; #kvuli dimenzi p-1
   sm=predict(my, data.frame(b));
   out[i]=sm[1];
  }
w=sum(out==y); 
#print(c("in SVM", w));  
list(out=w);
}#mysvm

### k-nearest neighbors
myknn=function(loc)
  {pom=knn.cv(a,cl=y,k=5); 
  w=sum(pom==y);
  list(out=w)
}#myknn
#library(class);
### autovalidation
#  w=knn(a, a, cl=y, k=5); sum((w==y)*1); 
### leave-one-out
#  w=knn.cv(a,cl=y,k=5); sum((w==y)*1);

### data standardization
#a=scale(a); #mean 0, variance 1
#a=apply(a,2,rank); #ranks in each column

### secondary learning for one variable
secone=function()
{ldamax=svmmax=q1=q2=-1;
for (i in 1:10)
{index=c(i,i);
  male=a[,index];
  q1=mysvm(male)$out; if (q1>svmmax) {svmmax=q1;} 
  q2=mylda(male)$out; if (q2>ldamax) {ldamax=q2; print("best index"); print(index);} 
  print(c(i,q1,q2));
 }#for
print("SVM max"); print(svmmax);
print("LDA max"); print(ldamax);
}#secone

### secondary learning for two or more variables
sec=function(konst)
{svmmax=-1; ldamax=-1; knnmax=-1; q1=q2=q3=-1;
for (i in 1:30)
  {if (konst > 1) {index=sample(pp,konst);
     } else {index=specific()$out;}
   male=a[,index];
q1=mysvm(male)$out; if (q1>svmmax) {svmmax=q1;} 
#q2=mylda(male)$out; if (q2>ldamax) {ldamax=q2;} #important output for interpretation #print("best index"); print(index);
q3=myknn(male)$out; if (q3>knnmax) {knnmax=q3;}
print(i); print(c(q1,q2,index)); if (q1==14) print("hello");
  }#for
print("SVM max"); print(svmmax/21);
print("LDA max"); print(ldamax/21);
print("knn max"); print(knnmax/21);
}#sec