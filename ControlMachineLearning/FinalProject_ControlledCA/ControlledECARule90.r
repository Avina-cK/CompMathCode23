# Uncomment the 4 lines below to download and install library: 'binaryLogic' 
# url <- "https://cran.r-project.org/src/contrib/Archive/binaryLogic/binaryLogic_0.3.9.tar.gz"
# pkgFile <- "binaryLogic_0.3.9.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Example
N = 6; T = 3;   # N     = length of grid, T     = number of time steps
u0 = c(0,1);
u1 = c(1,0);
u2 = c(1,0);
u = matrix(c(u0,u1,u2),nrow=3, byrow=TRUE)
rule_no = 90;   # rule number [0 to 256]
Rand = FALSE    # Rand  = initial condition is random or one single live cell in the middle 
initial_state =  c(0,1,1,1,0,0);  # a specific initial condition or NULL

library(binaryLogic)
n<-as.binary(rule_no, n=8)
R0<-rep(0, N)
GOL<-matrix(rep(0,N), ncol = N)
if(Rand==TRUE){GOL[1,]<-sample(c(1,0),N, replace=TRUE)}
if(Rand==FALSE){
  if (is.null(initial_state)){
    GOL[1,floor(N/2)]<-1
  } else{GOL[1,]<-initial_state}
}
t<-1
rot <- function(x) t(apply(x, 2, rev))
while(t<(T+2)){
  GOL<-rbind(GOL, c(R0))
  if(t<T+1){
    i=1;
    if(u[t,1]==0){
      #0-11
      if((GOL[t, i] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[4]}
        
      #0-10
      if((GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[6]}
      
      #0-01
      if((GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[7]}
      
      #0-00
      if((GOL[t, i] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[8]}
    }
    if(u[t,1]==1){
      #1-11
      if((GOL[t, i] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[1]}
      
      #1-10
      if((GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
          GOL[t+1,i]<-n[2]}
        
      #1-01
      if((GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
          GOL[t+1,i]<-n[3]}
        
      #1-00
      if((GOL[t,i] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[4]}
      }
      
      
    i=N;
    if(u[t,2]==0){
      #11-0
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 1)){
        GOL[t+1,i]<-n[2]}
      #10-0
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0)){
        GOL[t+1,i]<-n[4]}
      #01-0
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1)){
        GOL[t+1,i]<-n[6]}
      #00-0
      if((GOL[t,i-1] == 0) && (GOL[t, i] == 0)){
        GOL[t+1,i]<-n[8]}
    }
    if(u[t,2]==1){
      #11-1
      if((GOL[t,i-1] == 1) && (GOL[t, i] == 1)){
        GOL[t+1,i]<-n[1]}
      #101
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0)){
        GOL[t+1,i]<-n[3]}
      #011
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1)){
        GOL[t+1,i]<-n[5]}
      #001
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 0)){
        GOL[t+1,i]<-n[7]}
    }      
    for(i in 2:(N-1)){
      #111
      if((GOL[t,i-1] == 1) && (GOL[t, i] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[1]}  
      #110
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[2]}  
      #101
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[3]}  
      #100
      if((GOL[t, i-1] == 1) && (GOL[t,i] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[4]}
        
      #011
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[5]}
        
      #010
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 1) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[6]}
        
      #001
      if((GOL[t, i-1] == 0) && (GOL[t,i] == 0) && (GOL[t,i+1] == 1)){
        GOL[t+1,i]<-n[7]}
        
      #000
      if((GOL[t,i-1] == 0) && (GOL[t, i] == 0) && (GOL[t,i+1] == 0)){
        GOL[t+1,i]<-n[8]}
    }  
  }
    
  t<-t+1
} 

state_configuration<-1:N; time <-0:T+1;
GOL<-GOL[time, state_configuration]
image(time, state_configuration, GOL
      , axes=FALSE
      #, asp=1
      , col = hcl.colors(10, "Grays", rev = TRUE)
      , xlab = "time"
      , ylab = "configuration")
axis(1, at = seq(-1, T+1, by = 1), las=2)
axis(2, at = (seq(-1, N, by = 1)), las=3, srt=90)
grid(nx = T+1, ny = N,col = "grey")
#title(main=paste("Rule",as.character(rule_no)))

for (x in 1:ncol(GOL)){
  for (y in 1:nrow(GOL)){
    if(GOL[y,x]==0){
      text(time[y]
           , state_configuration[x]+0.0
           , GOL[y,x]
           , col="black"
           , srt=90)
    } 
    else {
      text(time[y]
            , state_configuration[x]+0.0
            , GOL[y,x]
            , col = "white"
            , srt=90)
    }
  }
}
