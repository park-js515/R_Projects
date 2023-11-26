#!2022/03/15 수정본!#
# 반드시 ramos_fdr 함수와 사용할 것Functions for flex and fixed methods

#Functions for flex and fixed methods

#libraries needed
library(abind)
library(locfdr)
library(RConics)


# #data simulator
# 
# #Basic data simulator; used in initial simulations but later modified 9see rdata2 to rdata5
# #n is the sample size
# #m0 is the mean for the first null component
# #m1 is the mean for the 2nd null component
# #v1 is the variance for the first null component
# #v2 is the variance for the 2nd null component
# #p is the mixing probability for the two null components
# #altp is the proportion of samples coming from the alternative distribution
# 
# rdata<-function(n,m0=0.02,m1=-0.02,v0=0.02,v1=0.03,p=0.6,altp=0.05){
# M<-matrix(numeric(2*n),ncol=2);
# for(i in 1:n){
# 		 M[i,1]<-rbinom(1,1,p);
# 		 }
# for(i in 1:n){
# 		if(M[i,1]==1){M[i,2]<-rnorm(1,mean=m0,sd=sqrt(v0));}
# 		if(M[i,1]==0){M[i,2]<-rnorm(1,mean=m1,sd=sqrt(v1));}
# 		}
# nullcl<-numeric(n);
# M<-cbind(M,nullcl);
# if(altp>0){
# altn<-round(n*altp);
# A<-matrix(numeric(2*altn),ncol=2);
# for(j in 1:altn){
# 		 A[j,1]<-rbinom(1,1,0.5);
# 		 }
# for(j in 1:altn){
# 		if(A[j,1]==1){A[j,2]<-rt(1,20)+2;}
# 		if(A[j,1]==0){A[j,2]<-rt(1,20)-2;}
# 		}
# altcl<-numeric(altn)+1;
# A<-cbind(A,altcl);
# simdata<-rbind(M,A);
# }
# if(altp==0){
# simdata<-M;
# }
# return(simdata);
# }
# 
# 
# #data simulator v2. Alternatives are from truncated t
# #n is the sample size
# #m0 is the mean for the first null component
# #m1 is the mean for the 2nd null component
# #v1 is the variance for the first null component
# #v2 is the variance for the 2nd null component
# #p is the mixing probability for the two null components
# #altp is the proportion of samples coming from the alternative distribution
# 
# rdata2<-function(n,m0=0.02,m1=-0.02,v0=0.02,v1=0.03,p=0.6,altp=0.05){
# M<-matrix(numeric(2*n),ncol=2);
# for(i in 1:n){
# 		 M[i,1]<-rbinom(1,1,p);
# 		 }
# for(i in 1:n){
# 		if(M[i,1]==1){M[i,2]<-rnorm(1,mean=m0,sd=sqrt(v0));}
# 		if(M[i,1]==0){M[i,2]<-rnorm(1,mean=m1,sd=sqrt(v1));}
# 		}
# nullcl<-numeric(n);
# M<-cbind(M,nullcl);
# if(altp>0){
# altn<-round(n*altp);
# A<-matrix(numeric(2*altn),ncol=2);
# for(j in 1:altn){
# 		 A[j,1]<-rbinom(1,1,0.5);
# 		 }
# for(j in 1:altn){
# 		if(A[j,1]==1){A[j,2]<-abs(rt(1,20))+0.75;}
# 		if(A[j,1]==0){A[j,2]<--abs(rt(1,20))-0.75;}
# 		}
# altcl<-numeric(altn)+1;
# A<-cbind(A,altcl);
# simdata<-rbind(M,A);
# }
# if(altp==0){
# simdata<-M;
# }
# return(simdata);
# }
# 
# #data simulator v3. adjustable truncation
# #n is the sample size
# #m0 is the mean for the first null component
# #m1 is the mean for the 2nd null component
# #v1 is the variance for the first null component
# #v2 is the variance for the 2nd null component
# #p is the mixing probability for the two null components
# #altp is the proportion of samples coming from the alternative distribution
# #tv is the truncation limit (all numbers in (-tv,tv) are truncated out)
# 
# rdata3<-function(n,m0=0.02,m1=-0.02,v0=0.02,v1=0.03,p=0.6,altp=0.05,tv=0.4){
# M<-matrix(numeric(2*n),ncol=2);
# for(i in 1:n){
# 		 M[i,1]<-rbinom(1,1,p);
# 		 }
# for(i in 1:n){
# 		if(M[i,1]==1){M[i,2]<-rnorm(1,mean=m0,sd=sqrt(v0));}
# 		if(M[i,1]==0){M[i,2]<-rnorm(1,mean=m1,sd=sqrt(v1));}
# 		}
# nullcl<-numeric(n);
# M<-cbind(M,nullcl);
# if(altp>0){
# altn<-round(n*altp);
# A<-matrix(numeric(2*altn),ncol=2);
# for(j in 1:altn){
# 		 A[j,1]<-rbinom(1,1,0.5);
# 		 }
# for(j in 1:altn){
# 		if(A[j,1]==1){A[j,2]<-abs(rt(1,20))+tv;}
# 		if(A[j,1]==0){A[j,2]<--abs(rt(1,20))-tv;}
# 		}
# altcl<-numeric(altn)+1;
# A<-cbind(A,altcl);
# simdata<-rbind(M,A);
# }
# if(altp==0){
# simdata<-M;
# }
# return(simdata);
# }
# 
# #data simulator v4. with shifting and adjustable truncation
# #n is the sample size
# #m0 is the mean for the first null component
# #m1 is the mean for the 2nd null component
# #v1 is the variance for the first null component
# #v2 is the variance for the 2nd null component
# #p is the mixing probability for the two null components
# #altp is the proportion of samples coming from the alternative distribution
# #cf is the truncation limit (all numbers in (-cf,cf) are truncated out)
# #tv is the shifting. a t distribution is shifted to each side by +-tv
# 
# rdata4<-function(n,m0=0.02,m1=-0.02,v0=0.02,v1=0.03,p=0.6,altp=0.05,tv=1.5,cf=0.5){
#   ## null
#   M<-matrix(numeric(2*n),ncol=2);
#   for(i in 1:n){
# 		M[i,1]<-rbinom(1,1,p);
# 	}
#   for(i in 1:n){
# 		if(M[i,1]==1){M[i,2]<-rnorm(1,mean=m0,sd=sqrt(v0));}
# 		if(M[i,1]==0){M[i,2]<-rnorm(1,mean=m1,sd=sqrt(v1));}
# 	}
#   nullcl<-numeric(n);
#   M<-cbind(M,nullcl);
#   
#   ## alter
#   if(altp>0){
#     altn<-round(n*altp);
#     A<-matrix(numeric(2*altn),ncol=2);
#   
#     for(j in 1:altn){
#   		A[j,1]<-rbinom(1,1,0.5);
#     }
#     for(j in 1:altn){
# 	    if(A[j,1]==1){
#   		  samp1<-abs(rt(1,20)+tv);
#   	  	while(samp1<cf){samp1<-abs(rt(1,20)+tv);}
#   		  A[j,2]<-samp1
#       }
# 		  if(A[j,1]==0){
# 		    samp2<--abs(rt(1,20)-tv);
# 		    while(samp2>-cf){samp2<--abs(rt(1,20)-tv);}
# 		    A[j,2]<-samp2;
# 	  	}
#     }
#     altcl<-numeric(altn)+1;
#     A<-cbind(A,altcl);
#     simdata<-rbind(M,A);
#   }
#   if(altp==0){
#   simdata<-M;
#   }
#   return(simdata);
# }
# 
# #gamma alternative
# #data simulator v3. adjustable truncation
# #n is the sample size
# #m0 is the mean for the first null component
# #m1 is the mean for the 2nd null component
# #v1 is the variance for the first null component
# #v2 is the variance for the 2nd null component
# #p is the mixing probability for the two null components
# #altp is the proportion of samples coming from the alternative distribution
# #tv is the shift parameter for the gamma alternative
# #shape is the shape parameter for the gamma alternative
# #scale is the scale parameter for the gamma alternative
# 
# rdata5<-function(n,m0=0.02,m1=-0.02,v0=0.02,v1=0.03,p=0.6,altp=0.05,tv=0.4,shape=3,scale=1){
# M<-matrix(numeric(2*n),ncol=2);
# for(i in 1:n){
# 		 M[i,1]<-rbinom(1,1,p);
# 		 }
# for(i in 1:n){
# 		if(M[i,1]==1){M[i,2]<-rnorm(1,mean=m0,sd=sqrt(v0));}
# 		if(M[i,1]==0){M[i,2]<-rnorm(1,mean=m1,sd=sqrt(v1));}
# 		}
# nullcl<-numeric(n);
# M<-cbind(M,nullcl);
# if(altp>0){
# altn<-round(n*altp);
# A<-matrix(numeric(2*altn),ncol=2);
# for(j in 1:altn){
# 		 A[j,1]<-rbinom(1,1,0.5);
# 		 }
# for(j in 1:altn){
# 		if(A[j,1]==1){A[j,2]<-abs(rgamma(1,shape=shape,scale=scale))+tv;}
# 		if(A[j,1]==0){A[j,2]<--abs(rgamma(1,shape=shape,scale=scale))-tv;}
# 		}
# altcl<-numeric(altn)+1;
# A<-cbind(A,altcl);
# simdata<-rbind(M,A);
# }
# if(altp==0){
# simdata<-M;
# }
# return(simdata);
# }


#EM setup function
#eps is initial mixing parameter
#mu1 is initial value for mean of 1st component
#mu2 is initial value for mean of 2nd component
#sigma1 is initial value for sd of 1st component
#sigma2 is initial value for sd of 2nd component
#a,b,x,y are data values

eprob<-function(eps,mu1, mu2,sigma1,sigma2,a,b,x,y)
{
  return((eps*(pnorm(y,mu1,sigma1)-pnorm(x,mu1,sigma1))+(1-eps)*(pnorm(y,mu2,sigma2)-pnorm(x,mu2,sigma2)))/
    (eps*(pnorm(b,mu1,sigma1)-pnorm(a,mu1,sigma1))+(1-eps)*(pnorm(b,mu2,sigma2)-pnorm(a,mu2,sigma2))))
}

em_ramos <- function(bb2, mu1, mu2, sigma1, sigma2, phi, a, b, iter)
{
gridsigma=seq(0.001,4,by=0.005)
for(i in 2:iter)
{
gamma1=phi*dnorm(bb2,mu1,sigma1)/(pnorm(b,mu1,sigma1)-pnorm(a,mu1,sigma1))
gamma2=(1-phi)*dnorm(bb2,mu2,sigma2)/(pnorm(b,mu2,sigma2)-pnorm(a,mu2,sigma2))
gamma=gamma1/(gamma1+gamma2)

sumgamma=sum(gamma)
sumgamma2=sum(1-gamma)
obj_sigma1= -0.5*log(gridsigma^2)*sumgamma-0.5*sum(gamma*(bb2-mu1)^2)/
            gridsigma^2-sumgamma*log(pnorm(b,mu1,gridsigma)-pnorm(a,mu1,gridsigma))
obj_sigma2= -0.5*log(gridsigma^2)*sumgamma2-0.5*sum((1-gamma)*(bb2-mu2)^2)/
            gridsigma^2-sumgamma2*log(pnorm(b,mu2,gridsigma)-pnorm(a,mu2,gridsigma))
sigma1n= gridsigma[which.max(obj_sigma1)]
sigma2n= gridsigma[which.max(obj_sigma2)]
mu1n = sum(gamma *(bb2))/sum(gamma)
mu2n = sum(gamma2 *(bb2))/sum(gamma2)
phin=mean(gamma)

sigma1 = sigma1n
sigma2 = sigma2n
mu1 = mu1n
mu2 = mu2n
phi= phin
}

aaa = c(mu1, mu2, sigma1, sigma2, phi)

return(aaa)
}

#binning function
#bb is data
#nb is number of bins


countftn <-function(bb, nb) { ## bb = data, nb=number of bins
bin = (max(bb)-min(bb))/nb
cut=0
cut[1] = min(bb)
cut[2] = min(bb) + bin
col = nb 
aa = 0
aa[1] =  sum( cut[1]<= bb &   bb <=cut[2]) 
for(i in 2:col) {
 cut[i+1] = cut[i] + bin
 aa[i] = sum(cut[i] < bb & bb<= cut[i+1]) }
bb = cbind(cut[1:nb] + .5*bin, aa)
return(bb)
}

#goodness of fit algorithm
#bb is the sample data
#R is the prespecified truncation. Data in [-R,R] are considered to come purely from the null distribution
# bb<-simdata[,2];R<-c[length(1)]
NEM<-function(bb,R){

kk=density(bb)
hatmu= kk$x[which.max(kk$y)]

hattheta=0
logprior=0
logsum=0
loglik=0
a=0
b=0
acount=0
bcount=0
cc=0

 
 mid=0
 nb=100                       #need to be even number)
 bin = max((max(bb)-hatmu), (hatmu-min(bb)))/(nb*.5)

 cut=0
 for(i in 1:(nb*.5)) 
 cut[i] = hatmu - bin*(nb*.5-i)

 for(i in (nb*.5+1):nb)
 cut[i] = hatmu+bin*(i-nb*.5)

 count=0
 count[1] = sum(bb<=cut[1])
 count[nb] = sum(bb>cut[nb-1])
 for(i in 2:(nb-1))     count[i] = sum(cut[i-1] < bb & bb<= cut[i])

 pchi=0
 R_value=0
 indexnum = 15:50


R = R

hattheta=0
logprior=0
logsum=0
loglik=0
cc=0

a=hatmu-R
b=hatmu+R
bb2=bb[a<bb & bb<b]


mu1=0
mu2=.2
sigma1=1
sigma2=0.3
phi=0.5
iter=40

   est =  em_ramos(bb2, mu1, mu2, sigma1, sigma2, phi, a, b, iter)

   mu1 = est[1] 
   mu2 = est[2]
   sigma1 = est[3]
   sigma2 = est[4]
   phi = est[5]

out1<-c(mu1,sigma1^2);
out2<-c(mu2,sigma2^2);
out3<-c(phi,R);
out<-rbind(out1,out2,out3);
return(out);
}

#old em

convem<-function(oldemres){
newres<-numeric(7);
newres[3]<-oldemres[1];
newres[4]<-oldemres[3]^2;
newres[5]<-oldemres[2];
newres[6]<-oldemres[4]^2;
newres[7]<-oldemres[5];
return(newres);
}

#Base estimator
#bb is the sample data

CEM<-function(bb){

kk=density(bb)
hatmu= kk$x[which.max(kk$y)]

hattheta=0
logprior=0
logsum=0
loglik=0
a=0
b=0
acount=0
bcount=0
cc=0

 
 mid=0
 nb=100                    #need to be even number)
 bin = max((max(bb)-hatmu), (hatmu-min(bb)))/(nb*.5)

 cut=0
 for(i in 1:(nb*.5)) 
 cut[i] = hatmu - bin*(nb*.5-i)

 for(i in (nb*.5+1):nb)
 cut[i] = hatmu+bin*(i-nb*.5)

 count=0
 count[1] = sum(bb<=cut[1])
 count[nb] = sum(bb>cut[nb-1])
 for(i in 2:(nb-1))     count[i] = sum(cut[i-1] < bb & bb<= cut[i])

 pchi=0
 R_value=0
 indexnum = 5:50

for(k in 1:length(indexnum))
{

acut = cut[(nb*.5-indexnum[k]+1) : (nb*.5+indexnum[k])]
acount = count[(nb*.5-indexnum[k]+1) : (nb*.5+indexnum[k])]
a = min(acut)-bin
b = max(acut)

bb2=bb[a<=bb & bb<=b]
gridsigma=seq(0.001,4,by=0.01)

mu1=0
mu2=.2
sigma1=1
sigma2=0.3
phi=0.5
iter=40

   est =  em_ramos(bb2, mu1, mu2, sigma1, sigma2, phi, a, b, iter)

   mu1 = est[1] 
   mu2 = est[2]
   sigma1 = est[3]
   sigma2 = est[4]
   phi = est[5]

   expect=0
   expect[1] = eprob(phi, mu1, mu2, sigma1, sigma2, a, b, a, acut[1] )
   for (l in 1: (indexnum[k]*2-1)) {
       expect[(l+1)] = eprob(phi, mu1, mu2, sigma1, sigma2, a, b, acut[l], acut[(l+1)] ) 
     }
   expect = sum(acount)*expect

  chisqr = sum( (acount - expect)^2/expect )
  
#  print(chisqr)
#  print(indexnum[k])
  pchi[k] = 1- pchisq(chisqr, (length(expect)-1))
  R_value[k] = abs(min(acut)-bin) +  hatmu
}

R = R_value[which.max(pchi)]

hattheta=0
logprior=0
logsum=0
loglik=0
cc=0

a=hatmu-R
b=hatmu+R
bb2=bb[a<bb & bb<b]


mu1=0
mu2=.2
sigma1=1
sigma2=0.3
phi=0.5
iter=40

   est =  em_ramos(bb2, mu1, mu2, sigma1, sigma2, phi, a, b, iter)

   mu1 = est[1] 
   mu2 = est[2]
   sigma1 = est[3]
   sigma2 = est[4]
   phi = est[5]
out<-convem(est);
out[1]<-R;
return(out);
}


#truncation function
#d is data
#C is the truncation value. All values outside of [-C,C] are truncated out


trn<-function(d,C){
  w<-c()
  if(length(C)==1){
    w<-d[(d> -C) & (d<C)]
  } else {
    w<-d[(d> C[1]) & (d<C[2])]
  }
  return(w);
}


#Empirical mixture function
#d is the data

fx<-function(d, aax, den){
n<-length(d);
f<-numeric(n);
for(i in 1:n){
f[i]<-lookup(d[i], aax, den);
}
return(f);
}

#Mixture of normal likelihood
#d is the data
#m0 is mean of first component
#m1 is mean of 2nd component
#v0 is the variance of first component
#v1 is the variance of 2nd component
#R is the zero assumption constant

lik<-function(d,m0,m1,v0,v1,p0,R){
n<-length(d);
f<-numeric(n);
for(i in 1:n){
f[i]<-p0*dnorm(d[i],mean=m0,sd=sqrt(v0))+(1-p0)*dnorm(d[i],mean=m1,sd=sqrt(v1));
}
return(f);
}

#Mixture of log likelihood
#d is the data
#m0 is mean of first component
#m1 is mean of 2nd component
#v0 is the variance of first component
#v1 is the variance of 2nd component
#R is the zero assumption constant

loglik<-function(z,m0,m1,v0,v1,p0,b1){
S<-0;
for(i in 1:length(z)){
S<-S+log(p0*dtruncnorm(z[i],a=-b1,b=b1,mean=m0,sd=sqrt(v0))+(1-p0)*dtruncnorm(z[i],a=-b1,b=b1,mean=m1,sd=sqrt(v1)));
}
return(S);
}


#Estimator of proportion of nulls
#w is the data
#EMres is the result from the EM algorithm

pi0<-function(w,EMres, aax, remanres){
p<-(preman(y=w, aax, remanres)-preman(y=(-1)*w, aax, remanres))/(pnormalmix(x=w,res=EMres)-pnormalmix(x=-1*w,res=EMres));
p1<-min(1,p);
return(p1);
}


#Criteria computer with null proportion
#dfx is result from fx
#dlik is result from lik
#pay is result from pi10

crit2<-function(dfx,dlik,pay){
n<-length(dfx);
S<-0;
for(i in 1:n){
S<-S+log(dlik[i]/dfx[i]);
}
S<-S*((length(dlik))/pay);
return(S);
}

#Criteria computer with null proportion
#dfx is result from fx
#dlik is result from lik
#pay is result from pi10

crit<-function(dfx,dlik,pay){
n<-length(dfx);
S<-0;
for(i in 1:n){
S<-S+log(dlik[i]/dfx[i]);
}
return(S);
}

#Binning function
#d is data
#c is initial bin
#inc is data per bin
#end is ending bin

cpick<-function(d,c=0.25,inc=100,end=1.5){
nd<-trn(d,c);
cpk<-numeric(10000);
cpk[1]<-c;
i<-2;
	while(c<end){
	dif<-0;
	x<-length(nd);
		while(dif<inc){
		c<-c+0.001;
		nd<-trn(d,c);
		x1<-length(nd);
		dif<-x1-x;
		}
	cpk[i]<-c;
	i<-i+1;
	}
cpk<-setdiff(cpk,0);
cpk<-cpk[-length(cpk)];
return(cpk);
}

#Cutting function
#n is data
#s is initial value
#diff is constant interval

cut<-function(s,diff,n){
c<-numeric(n);
t<-0;
for(i in 1:n){
c[i]<-s-t;
t<-t+diff;
}
return(c);
}

#EM Estimator wrapping simn
#d is data
#s is initial value
#diff is constant interval
#n is number of intervals

simn2<-function(d,s,diff,n, aax, den){
c<-cut(s,diff,n);
x<-simn(d,c, aax, den, remanres);
return(x);
}

#Marginal distribution generator
#data is sample data
#DC is truncation value
#bre and df are arguments for locfdr
#trunc is truncation indicator

# degen original

# dengen<-function(data,DC=6,bre=150,df=45,trunc=0){
# if(trunc==1){
# data<-trn(data,DC);
# }
# lfdr<-locfdr(data, bre=bre, df=df, type=0, plot = 0); # type = 0 ?
# aa<-lfdr$mat;
# aa<-data.frame(aa);
# if(length(DC)==1){
#   den<-aa$f /sum(aa$f) /(2*DC/(bre-1));
# } else{
#   den<-aa$f /sum(aa$f) /((-DC[1]+DC[2])/(bre-1));
# }
# remanres<-reman(aa$x,den);
# # plot(aa$x, den,type="l");
# out<-cbind(aa$x,den,remanres);
# return(out);
# }

# 2022-03-13 modified
dengen <- function(data, DC = 6, bre = 150, df = 45, trunc = 0, type = 0){
  if (trunc == 1){
    data <- trn(data, DC);
    }
  
  lfdr <- locfdr(data, bre = bre, df = df, type = type, plot = 0); # type = 0 ?
  aa <- lfdr$mat;
  aa <- data.frame(aa);  
  d_min <- min(data)
  d_max <- max(data)
  
  if (length(DC) == 1){
    if (is.infinite(DC)){
      den <- aa$f /sum(aa$f) /((-d_min + d_max)/(bre - 1))
      }
    else {
      den <- aa$f /sum(aa$f) /(2*DC/(bre - 1))
      }

    } 
  else {
    if (sum(is.infinite(DC)) >= 1){
      if (sum(is.infinite(DC)) == 2){
        den <- aa$f /sum(aa$f) /((-d_min + d_max)/(bre - 1))
        }
      else if (is.infinite(DC[1]) & !is.infinite(DC[1])){
        den <- aa$f /sum(aa$f) /((-d_min + DC[2])/(bre - 1))
        }
      else {
        den <- aa$f /sum(aa$f) /((-DC[1] + d_max)/(bre - 1))
        }
      }
    else {
      den <- aa$f /sum(aa$f) /((-DC[1] + DC[2])/(bre - 1));
      }
    }
  
  remanres <- reman(aa$x, den);
  # plot(aa$x, den,type="l");
  out <- cbind(aa$x, den, remanres);
  return(out);
}

#Marginal constructor
#x and den are outputs from dengen

reman<-function(x,den){
len<-x[2]-x[1]; # interval? 
n<-length(x);
area<-numeric(n);
for(i in 1:n){
area[i]<-len*den[i];
}
return(area);
}

#Marginal function
#y is the sample data input
#x is aax from dengen
#remanres1 is result from reman

preman<-function(y, aax, remanres){
w<- aax[1];
S<-0;
i<-1;
while(w<y){
S<-S + remanres[i];
w<-aax[i+1];
i<-i+1;
}
return(S);
}

#Function for computing proportion of null
#w is the sample data
#simnres is result from simn function


pi02<-function(w,simnres, aax, remanres){
p<-(preman(y=w, aax, remanres)-preman(y=(-1)*w, aax, remanres))/(pnormalmix2(x=w,res=simnres)-pnormalmix2(x=-1*w,res=simnres));
p1<-min(1,p);
return(p1);
}

#Local fdr computer
#d is sample data
#simnres is result from simn function
mylocfdr<-function(d,simnres, aax, den, remanres){
f0<-lik2(d,simnres[3],simnres[5],simnres[4],simnres[6],simnres[7]);
p<-pi02(simnres[1],simnres, aax, remanres);
pvec<-p*(numeric(length(d))+1);
f<-fx(d, aax, den);
# print(p);
out<-p*f0/f;
out2<-cbind(d,out,pvec);
return(out2);
}

mylocfdr2<-function(d,simnres, aax, den, remanres){
  f0<-lik2(d,simnres[3],simnres[5],simnres[4],simnres[6],simnres[7]);
  p<-pi02(simnres[1],simnres, aax, remanres);
  pvec<-p*(numeric(length(d))+1);
  f<-fx(d, aax, den);
  # print(p);
  out<-p*f0/f;
  out2<-cbind(d,out,pvec,f);
  return(out2);
}
#Likelihood computer
#d is sample data
#m0 is mean of first component
#m1 is mean of 2nd component
#v0 is the variance of first component
#v1 is the variance of 2nd component
#p0 is proportion of 1st component

lik2<-function(d,m0,m1,v0,v1,p0){
n<-length(d);
f<-numeric(n);
for(i in 1:n){
f[i]<-p0*dnorm(d[i],m0,sqrt(v0))+(1-p0)*dnorm(d[i],m1,sqrt(v1));
}
return(f);
}

#Density for normal mixture
#x is data input
#res is result from simn function

dnormalmix2<-function(x,res){
f<-res[7]*dnorm(x,res[3],sqrt(res[4]))+(1-res[7])*dnorm(x,res[5],sqrt(res[6]));
return(f);
}

#Cummulative distribution for normal mixture
#x is data input
#res is result from simn function

pnormalmix2<-function(x,res){
f<-res[7]*pnorm(x,res[3],sqrt(res[4]))+(1-res[7])*pnorm(x,res[5],sqrt(res[6]));
return(f);
}


#pvalue adjuster
#sample data
#result from simn function

pv<-function(d,simnres){
n<-length(d);
pval<-numeric(n);
for(i in 1:n){
pval[i]<-1-pnormalmix2(abs(d[i]),simnres);
}
pvaladj<-p.adjust(pval,"BH");
out<-cbind(d,pvaladj);
return(out);
}

#Significant genes counter
#p is the vector of local fdrs
#a is the alpha threshold

sigcount<-function(p,a=0.05){
n<-length(p);
S<-0;
for(i in 1:n){
if(p[i]<=a){S<-S+1;}
}
return(S);
}

#Estimator of empirical fdr
#datafromsim is sample (simulated) data (includes indicator if null or alternative)
#datawpval is the fdr values computed for the data

fdrest<-function(datafromsim,datawpval,a=0.2){
  n<-length(datafromsim[,1]);
  decision<-numeric(n);
  qq<-quantile(datafromsim[,2],c(0.45,0.55))
  fdr_ind<-which((datafromsim[,2]<qq[1] | datafromsim[,2]>qq[2])==1)
  
  for(i in fdr_ind){
    if(datawpval[i,2]<a){decision[i]<-1;}
  }
  
  w<-datafromsim[,3]-decision;
  rej<-sum(decision)
  fd<-sum(w==-1)
  tp<-rej-fd
  tpr<-tp/sum(datafromsim[,3]==1)
  if(rej==0) {fdr<-0 } else {fdr<-fd/rej}
  fpr<-fd/sum(datafromsim[,3]==0);
  spe<-1-fpr;
  
  out<-c(fdr,tpr,spe,fd,rej);
  names(out)<-c("FDR","TPR","TNR","FD","Reject")
  return(out);
}


fdrest2<-function(datafromsim,datawpval,a=0.2){
  n<-length(datafromsim[,1]);
  decision<-numeric(n);
  qq<-quantile(datafromsim[,2],c(0.15,0.85))
  fdr_ind<-which((datafromsim[,2]<qq[1] | datafromsim[,2]>qq[2])==1)
    
  for(i in fdr_ind){
    if(datawpval[i,2]<a){decision[i]<-1;}
  }
  
  w<-datafromsim[,3]-decision;
  rej<-sum(decision)
  fd<-sum(w==-1)
  tp<-rej-fd
  tpr<-tp/sum(datafromsim[,3]==1)
  if(rej==0) {fdr<-0 } else {fdr<-fd/rej}
  fpr<-fd/sum(datafromsim[,3]==0);
  spe<-1-fpr;
  
  out<-c(fdr,tpr,spe,fd,rej);
  names(out)<-c("FDR","TPR","TNR","FD","Reject")
  return(out);
}

#lookup function
#x is sample data
#look is input check values
#assign is output values

lookup<-function(x,look,assign){
n<-length(look);
i<-1;
while(x>look[i]&i<n){
i<-i+1;
}
m<-assign[i];
return(m)
}



#normal mixture 
#x is data
#res is result from simn

pnormalmix<-function(x,res){
f<-res[3,1]*pnorm(x,res[1,1],sqrt(res[1,2]))+(1-res[3,1])*pnorm(x,res[2,1],sqrt(res[2,2]));
return(f);
}

#EM result repackager
#d is data
#c is zero assumption constant

simn<-function(d,c, aax, den, remanres){
	out<-numeric(length(c));
	M0<-numeric(length(c));
	V0<-numeric(length(c));
	M1<-numeric(length(c));
	V1<-numeric(length(c));
	P0<-numeric(length(c));
	I<-numeric(length(c));
	for(i in 1:length(c)){
	res<-NEM(d,R=c[i]);
	x<-trn(d,c[i]);
	datalik<-lik(x,res[1,1],res[2,1],res[1,2],res[2,2],res[3,1]);
	datafx<-fx(x, aax, den);
	pi0dat<-pi0(w=c[i],EMres=res, aax, remanres);
	out[i]<-crit(dfx=datafx,dlik=datalik,pay=pi0dat);
	M0[i]<-res[1,1];
	V0[i]<-res[1,2];
	M1[i]<-res[2,1];
	V1[i]<-res[2,2];
	P0[i]<-res[3,1];
	I[i]<-res[3,2];
	# print(out[i]);
	}
	outfin<-cbind(c,out,M0,V0,M1,V1,P0,I);
	return(outfin);
}


#function to identify initial c
#c is initial value
#data is sample data
#nul is percentage of data to be used


csolver<-function(c,data=simdata[,2],nul=0.80){
out<-length(trn(simdata[,2],c))-length(data)*nul;
return(out);
}

#The simn function used for final EM estimation (others were prototypes)
#d is the data
#c is the zero assumption  constant

simnstep1<-function(d,c, aax, den){
	initialcut<-c[1];
	c<-c[-1];
	size<-length(c);
	out<-numeric(size);
	M0<-numeric(size);
	V0<-numeric(size);
	M1<-numeric(size);
	V1<-numeric(size);
	P0<-numeric(size);
	I<-numeric(size);
	for(i in 1:size){
	res<-NEM(d,R=c[i]);
	x<-d[d<=-initialcut|d>=initialcut];
	x<-x[x>=-c[i]&x<=c[i]];
	datalik<-lik(x,res[1,1],res[2,1],res[1,2],res[2,2],res[3,1]);
	datafx<-fx(x, aax, den);
	out[i]<-crit(dfx=datafx,dlik=datalik,pay=0);
	M0[i]<-res[1,1];
	V0[i]<-res[1,2];
	M1[i]<-res[2,1];
	V1[i]<-res[2,2];
	P0[i]<-res[3,1];
	I[i]<-res[3,2];
	# print(out[i]);
	}
	outfin<-cbind(c,out,M0,V0,M1,V1,P0,I);
	return(outfin);
}

#finds best result
#simn2res is result from simn function

bestfind<-function(simn2res){
n<-length(simn2res[,2]);
cand<-simn2res[,2];
for(i in 1:n){
if(max(cand)==cand[i]){choice<-i;}
}
bestchoice<-simn2res[choice, ];
return(bestchoice);
}

#quartic function
#x is the independent variable
#c is the vector of coefficients

p4func<-function(x,c){
out<-c[5]+c[4]*x+c[3]*x^2+c[2]*x^3+c[1]*x^4;
return(out);
}

#quandratic function
#x is the independent variable
#c is the vector of coefficients

p2func<-function(x,c){
out<-as.numeric(c[3])+as.numeric(c[2])*x+as.numeric(c[1])*x^2;
return(out);
}

#cubic function
#x is the independent variable
#c is the vector of coefficients

p3func<-function(x,c){
out<-as.numeric(c[4])+as.numeric(c[3])*x+as.numeric(c[2])*x^2+as.numeric(c[1])*x^3;
return(out);
}

fdrest_vec<-Vectorize(fdrest,c("a"))
fdrest2_vec<-Vectorize(fdrest2,c("a"))



