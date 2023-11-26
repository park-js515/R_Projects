require("kedd")
require("splines")
require("graphics")
require("truncnorm")
require("VennDiagram")
require("xlsx")
# options("scipen"=10)

kernel_func<-function(Zval,bw=NULL,r=1024){
  set.seed(208907)
  
  ### 1. Band-width selection & Kernel density cutoff
  if (is.null(bw)) {bw<-density(Zval)$bw*1.1} ## band width
  range_x<-c(min(Zval)-4*bw,max(Zval)+4*bw)
  range_y<-seq(range_x[1L], range_x[2L],length.out=r)
  
  ### 2. Find cut-point 
  ### 2.1 Kernel density
  Zval_kde<-dkde(Zval, deriv.order = 0,h=bw,y=range_y)
  kde_value<-Zval_kde$eval.points
  kde_est<-Zval_kde$est.fx
  
  ### 2.2 The 1-derivative of Kernel density
  Zval_dkde<-dkde(Zval, deriv.order = 1,h=bw,y=range_y)
  kde_est2<-Zval_dkde$est.fx
  
  ### 3. result
  result<-cbind(kde_value,kde_est,kde_est2)
  return(result)
}


Truncated<- function(data=Zval,iter=100,tol=10^(-7),fig=F,
                     low1,up1,low2,up2,low3,up3,low4,up4){
  
  ### 1. Truncated data
  Zval_t<-data[((data>low1) & (data<up1)) | ((data>low2) & (data<up2)) |
                 ((data>low3) & (data<up3)) | ((data>low4) & (data<up4))];
  n_Zval_t<-length(Zval_t)
  mu<-mean(Zval_t); se<-sd(Zval_t)
  para<-matrix(c(mu,se),ncol=2)
  
  ### 2. parameter estimation
  for(i in 1:iter){
    
    ### 2.1 prior
    mu_break<-mu
    se_break<-se
    
    ### 2.2 Mu
    f<-function (mu) {
      C<-(pnorm(up1,mu,se)-pnorm(low1,mu,se))+(pnorm(up2,mu,se)-pnorm(low2,mu,se))+
        (pnorm(up3,mu,se)-pnorm(low3,mu,se))+(pnorm(up4,mu,se)-pnorm(low4,mu,se))
      C_prime1<-(1/se)*(dnorm((low1-mu)/se)-dnorm((up1-mu)/se)+
                          dnorm((low2-mu)/se)-dnorm((up2-mu)/se)+
                          dnorm((low3-mu)/se)-dnorm((up3-mu)/se)+
                          dnorm((low4-mu)/se)-dnorm((up4-mu)/se))
      return(abs((-n_Zval_t)*(C_prime1/C) + (1/se^2)*(sum(Zval_t)-n_Zval_t*mu)))
    }
    # curve(f,from=-4,to=4,ylim=c(-1000,1000)) ## mu plot
    mu<-optimize(f,c(-3,3))$minimum
    
    ### 2.2 SE
    g<-function(se){
      C<-(pnorm(up1,mu,se)-pnorm(low1,mu,se))+(pnorm(up2,mu,se)-pnorm(low2,mu,se))+
        (pnorm(up3,mu,se)-pnorm(low3,mu,se))+(pnorm(up4,mu,se)-pnorm(low4,mu,se))
      C_prime2<-(1/se)*{((low1-mu)/se)*dnorm((low1-mu)/se)-((up1-mu)/se)*dnorm((up1-mu)/se)+
          ((low2-mu)/se)*dnorm((low2-mu)/se)-((up2-mu)/se)*dnorm((up2-mu)/se)+
          ((low3-mu)/se)*dnorm((low3-mu)/se)-((up3-mu)/se)*dnorm((up3-mu)/se)+
          ((low4-mu)/se)*dnorm((low4-mu)/se)-((up4-mu)/se)*dnorm((up4-mu)/se)}
      
      return( (-n_Zval_t)*(C_prime2/C) - (n_Zval_t)/se +sum((Zval_t-mu)^2)/(se^3))
    }
    
    # curve(g,from=0.01,to=3,ylim=c(-1500,1000),xlim=c(0,2)) ## SE plot
    xxx<-seq(se/5,3,0.0001);
    se<-uniroot(g,c(se/5,xxx[which.min(g(xxx))]) )$root
    
    ### 2.3 Stop-limit
    para<-rbind(para,c(mu,se))
    if( (abs(mu-mu_break)<tol) & (abs(se-se_break)<tol)  ){break;}
  }
  
  ### 3. Truncated histogram
  if(fig==T){hist(Zval_t,nclass=150)}
  
  ### 4. result table
  rownames(para)<-c(1:(i+1));colnames(para)<-c("Mu", "SE")
  result_list<-list(Zval_t,mu,se,para,n_Zval_t)
  names(result_list)<-c("Zval_t","Mu0","Sigma0","iter","n_Zval_t")
  return(result_list)
}  

Find_cutoff<-function(Zval,k=NULL,kk=NULL,kernel_r,method=1,pct=c(0.05,0.95)){
  set.seed(208907)
  
  ### 1 Zero assumption area (Initial)
  init_low1<-quantile(Zval,pct)[1]
  init_up2<-quantile(Zval,pct)[2]
  
  ### 2. Find cutoff
  ### 2.1 Kernel
  # kernel_ind<-(kernel_r[,1]>init_low1 & kernel_r[,1]<init_up2)
  kde_value<-kernel_r[,1]
  kde_est<-kernel_r[,2]
  kde_est2<-kernel_r[,3]
  
  ### 2.2 Peak area
  ### 2.2.1 peak criteria
  if(method==1){
    sigma_init<-max(em_ramos(Zval[Zval>init_low1 & Zval<init_up2], mu1=0, mu2=0, sigma1=1.1,
                       sigma2=0.2, phi=0.6, a=init_low1, b=init_up2, iter=100)[3:4])
  }else if(method==2){
    sigma_init<-sd(Zval[Zval>init_low1 & Zval<init_up2])
  }else if(method==3){
    sigma_init<-min(1,max(em_ramos(Zval[Zval>init_low1 & Zval<init_up2], mu1=0, mu2=0, sigma1=1.1,
                             sigma2=0.2, phi=0.6, a=init_low1, b=init_up2, iter=100)[3:4]))
  }
  density_cutoff<- 1/((sigma_init^2)*sqrt(2*pi*exp(1)))
  
  ### 2.2.2 peak condition
  upper_i<-cumsum(rle(kde_est2>density_cutoff)[[1]])
  lower_i<-cumsum(rle(-density_cutoff>kde_est2)[[1]])+1
  upper_p<-upper_i[seq(1,length(upper_i)-1,by=2)]
  lower_p<-lower_i[seq(2,length(lower_i),by=2)]
  
  cut_i<-cumsum(rle(sort(c(upper_p,lower_p)) %in% lower_p)[[1]])
  cut_l<-sort(c(upper_p,lower_p))[cut_i[seq(1,length(cut_i),by=2)]]
  cut_u<-sort(c(upper_p,lower_p))[cut_i[seq(1,length(cut_i),by=2)]+1]
  cut_m<-cbind(rbind(kde_value[cut_l],kde_value[cut_u]),c(10,10))
  cut_c<-tail(which(cut_m[1,]<kde_value[which.max(kde_est)]),1)
  
  n_peak<-dim(cut_m)[2]-1
  
  if(!is.na(n_peak[1])){
    if(n_peak!=1){
      up11<-c(cut_m[1,(cut_c-1)],-10)[1]; low21<-c(cut_m[2,(cut_c-1)],-10)[1]
      up12<-cut_m[1,cut_c]; low22<-cut_m[2,cut_c]
      up13<-cut_m[1,(cut_c+1)]; low23<-cut_m[2,(cut_c+1)]
    } else{
      up11<-(-10);low21<-(-10)
      up12<-cut_m[1,cut_c]; low22<-cut_m[2,cut_c];
      up13<-10;low23<-10
    }
  }else {
    up11<-(-10);low21<-(-10)
    up12<-0;low22<-0;
    up13<-10;low23<-10
    cat('There are no truncated areas. \n')
  }
  
  ### 2.2.3 Find up1 / low2
  if(init_low1 > up11 & init_low1<low21 ){
    up11<-init_low1
  } else if(init_low1>low21 & init_low1<up12){
    up11<-init_low1; low21<-init_low1
  } else if(init_low1>up12 & init_low1<low22){
    up11<-init_low1; low21<-init_low1; up12<-init_low1
  }
  
  if(init_up2 < low23 & init_up2 > up13 ){
    low23<-init_up2
  } else if(init_up2 < up13 & init_up2> low22 ){
    low23<-init_up2; up13<-init_up2;
  } else if(init_up2 < low22 & init_up2> up12 ){
    low23<-init_up2; up13<-init_up2; low22<-init_up2;
  }
  
  # cat(init_low1,up11,low21,up12,low22,up13,low23,init_up2)
  
  ### 2.3 Parameter Estimation (Initial)
  init_par<-Truncated(data=Zval, low1=init_low1, up1=up11, low2=low21, up2=up12,
                      low3=low22, up3=up13, low4=low23, up4=init_up2)
  mu_init<-init_par[[2]];se_init<-init_par[[3]]
  cutoff_m<-round(matrix(c(init_low1,up11,low21,up12,low22,up13,low23,init_up2,mu_init,se_init,sigma_init,
                     0,0,0,0,0,0,0,0,0,0,density_cutoff),nrow=2,ncol=11,byrow=T),5)
  
  
  ### 2.4 Find low1 / up2
  if(is.null(kk)){
    low1<-mu_init-k*se_init; up2<-mu_init+k*se_init
  } else{
    low1<-mu_init-kk; up2<-mu_init+kk
  }
  
  if(low1 > up11 & low1<low21 ){
    up11<-low1
  } else if(low1>low21 & low1<up12){
    up11<-low1; low21<-low1
  } else if(low1>up12 & low1<low22){
    up11<-low1; low21<-low1; up12<-low1
  }
  
  if(up2 < low23 & up2 > up13 ){
    low23<-up2
  } else if(up2 < up13 & up2> low22 ){
    low23<-up2; up13<-up2;
  } else if(up2 < low22 & up2> up12 ){
    low23<-up2; up13<-up2; low22<-up2;
  }
  # cat(low1,up11,low21,up12,low22,up13,low23,up2)
  
  ### 2.5 Result matrix
  cutoff_m[2,c(1:10)]<-round(t(c(low1,up11,low21,up12,low22,up13,low23,up2,0,0)),5)
  
  rownames(cutoff_m)<-c("Initial_point","Truncated_point")
  colnames(cutoff_m)<-c("c(low)","a_L(low)","a_L(up)","a(low)","a(up)",
                        "a_R(low)","a_R(up)","c(up)","Mu0","Sigma0","Density cutoff")
  cutoff_m
  return(cutoff_m)
}


fdr_func<-function(Zval,cutoff){
  
  c<-cutoff[2,1:8]
  mu_t<-cutoff[2,9]; se_t<-cutoff[2,10]
  
  ### 1. pi0 Estimation
  ### 1.1 Zero assumption
  ind_pi0<-((Zval>c[1])&(Zval<c[8]))
  Zval_pi0<-Zval[ind_pi0]
  
  f0_pi0<-numeric(sum(ind_pi0))
  f_pi0<-numeric(sum(ind_pi0))
  f_2_pi0<-numeric(sum(ind_pi0))
  
  for(i in c(1: sum(ind_pi0))){
    f0_pi0[i]<-dnorm(Zval_pi0[i],mu_t,se_t)
    f_pi0[i]<-lookup(Zval_pi0[i],aax,den);
    f_2_pi0[i]<-lookup(Zval_pi0[i],aax_kernel,den_kernel)
  }
  
  
  p00<-{preman(y=c[8])-preman(y=c[7])+preman(y=c[6])-preman(y=c[5])+
      preman(y=c[4])-preman(y=c[3])+preman(y=c[2])-preman(y=c[1])}/{
        pnorm(c[8],mu_t,se_t)-pnorm(c[7],mu_t,se_t)+pnorm(c[6],mu_t,se_t)-pnorm(c[5],mu_t,se_t)+
          pnorm(c[4],mu_t,se_t)-pnorm(c[3],mu_t,se_t)+pnorm(c[2],mu_t,se_t)-pnorm(c[1],mu_t,se_t)}
  
  ### 1.2 pi0 Estimate
  p0_1<-min(1,p00)  ## poisson area
  p0_2<-min(1,f_pi0/f0_pi0)  ## poisson point
  p0_3<-min(1,f_2_pi0/f0_pi0)  ## kernel point
  
  
  ### 2. Estimation of f, f0 
  f0<-numeric(n)
  f<-numeric(n)
  f_2<-numeric(n)
  
  for(i in 1: n){
    f0[i]<-dnorm(Zval[i],mu_t,se_t)
    f[i]<-lookup(Zval[i],aax,den);
    f_2[i]<-lookup(Zval[i],aax_kernel,den_kernel );
  }
  
  ### 3. Supplementary
  sup_mat<-matrix(0,ncol=7,nrow=n)
  rownames(sup_mat)<-seq(1:n)
  colnames(sup_mat)<-c("P_f","K_f","fdr(P_pi0*P_f)","fdr(K_pi0*P_f)",
                       "fdr(A_pi0*K_f)","fdr(P_pi0*K_f)","fdr(K_pi0 *K_f)")
  
  sup_mat[,1]<-f  ## poisson f
  sup_mat[,2]<-f_2  ## kernel f
  sup_mat[,3]<-p0_2*f0/f  ## fdr(poisson pi0 *poisson f)
  sup_mat[,4]<-p0_3*f0/f  ## fdr(kernel pi0 *poisson f)
  sup_mat[,5]<-p0_1*f0/f_2  ## fdr(Area pi0 *kernel f)
  sup_mat[,6]<-p0_2*f0/f_2  ## fdr(poisson pi0 *kernel f)
  sup_mat[,7]<-p0_3*f0/f_2  ## fdr(kernel pi0 *kernel f)

  
    
  ### 4 result
  result<-list()
  result[[1]]<-c(p0_1,p0_2,p0_3) ## pi0 (Poisson Area/Poisson/ Kernel)
  result[[2]]<-p0_1*f0/f ## fdr (Poisson area pi0 * Possion f)
  result[[3]]<-sup_mat
  names(result)<-c("pi0","fdr","sup_mat")
  
  return(result)
}
