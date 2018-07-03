#### Codigo modificado de M. Conroy. 2014. ANÁLISIS ESTADÍSTICA DE POBLACIONES SILVESTRES, USANDO DATOS DE OCUPACIÓN 
####(PRESENCIA - AUSENCIA) Y CAPTURA-RECAPTURA 15 - 26 septiembre 2014. Universidad Nacional de Rio Cuarto. 
https://sites.google.com/site/cursoriocuarto/home ####

##Definición de número de réplicas


rm(list=ls())
#install unmarked if not installed
#install.packages("unmarked")
#load library
require(unmarked)
#set here the directory where your data files are located
data_dir<-"C:/Ranalysis/Unmarked/samplesize"
setwd(data_dir)


sample_approx<-function(psi,p,nsites,nreps)
{
k<-nreps
s<-nsites
pstar<-1-(1-p)^k
d1<-(s*pstar)+psi*(1-pstar)*k*p*(1-pstar)
d2<-s*pstar*(pstar*(1-p)-k*p*(1-pstar))
var_psi<-psi*(1-psi)/s +psi*(1-pstar)/(d1*d2)
return(sqrt(var_psi)/psi)
}   


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.2
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.2
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
for (nreps in 2:9)
{

cv<-sample_approx(psi,p,nsites,nreps)
cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
sim_expts1<-rbind(sim_expts1,cv)

}
 }   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))

}


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.3
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.3
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 




#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.4
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.4
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.5
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.5
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.6
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.6
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.7
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.7
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.8
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.8
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


#PLOTS
#1- fixed number of sites and detection prob, vary p and n reps, PSI 0.9
detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.9
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
} 


  

#2-fixed detection prob, vary p and n sites
detection<-0.3
psi<-0.2
sim_expts2<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (nsites in c(10,25,50,75,100))
{
for (nreps in 2:5)
{
cv<-sample_approx(psi,p,nsites,nreps)
cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
sim_expts2<-rbind(sim_expts2,cv)
}
 }   
sim_expts2


n_levels<-c(100,75,50,25,10)
lab<-paste("psi=",psi," p=",detection,"n sites=",n_levels[1],"(solid)","n sites=",n_levels[2],"-",n_levels[4],"(dashed)")



lims<-range(sim_expts2$cv)
with(sim_expts2,plot(nreps[nsites==n_levels[1]],cv[nsites==n_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:5)
for (n in n_levels)
{
with(sim_expts2,matlines(nreps[nsites==n],cv[nsites==n],lty=2))

}
savePlot(filename="cv_vs_reps.by.nsites.jpg",type="jpg")  




###Test###

detect<-c(.1,0.2,0.3,0.4,.5)
nsites<-40
psi<-0.2
sim_expts1<-data.frame(nsites=as.numeric,psi=as.numeric,p=as.numeric,nreps=as.numeric,cv=as.numeric)
for (p in detect)
{
  for (nreps in 2:9)
  {
    
    cv<-sample_approx(psi,p,nsites,nreps)
    cv<-data.frame(nsites,psi,p=p,nreps=nreps,cv=cv)
    sim_expts1<-rbind(sim_expts1,cv)
    
  }
}   
sim_expts1

p_levels<-detect
lab<-paste("psi=",psi," n sites=",nsites,"p=",p_levels[1],"(solid)","p=",p_levels[2],"-",p_levels[5],"(dashed)")
lims<-range(sim_expts1$cv)
with(sim_expts1,plot(nreps[p==p_levels[1]],cv[p==p_levels[1]],type="l",main=lab,ylab="CV",xlab="Number of reps",xaxt="n",ylim=lims))
axis(1, at = 1:9)
for (px in p_levels)
{
  with(sim_expts1,matlines(nreps[p==px],cv[p==px],lty=2))
  
}
savePlot(filename="cv_vs_reps.by.p.jpg",type="jpg")  

