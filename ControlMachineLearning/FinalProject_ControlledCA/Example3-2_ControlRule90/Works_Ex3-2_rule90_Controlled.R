# Uncomment the 4 lines below to download and install library: 'binaryLogic' 
# url <- "https://cran.r-project.org/src/contrib/Archive/binaryLogic/binaryLogic_0.3.9.tar.gz"
# pkgFile <- "binaryLogic_0.3.9.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)
rot <- function(x) t(apply(x, 2, rev))  # rotation function required to plot image

'
n     = rule number [0 to 256]
N     = length of grid
T     = number of time steps
Rand  = initial condition is random or one single live cell in the middle  
initial_state = a specific initial condition or NULL
control = 
'

ECA_controlled<-function(n, N, T, initial_state, control){
  library(binaryLogic)# to convert rule number to binary
  n<-as.binary(n, n=8)# converting rule number to binary
  N_i<- N;            # storing input length of grid
  N<-N+2;             # adding one 0 cell on either side
  R0<-rep(0, N);      # initialize the CA space
  GOL<-matrix(rep(0,N), ncol = N);   # initial the CA space
  
  if(is.null(control)){
    control<-matrix(rep(0, 2*T), ncol=2)
  }
  
  # input the initial conditions
  c(control[1,1],initial_state, control[1,2])-> initial_state
  GOL[1,]<-initial_state # given initial state is inserted
  
  # neighbourhood state possibilities: 111, 110, 101, 100, 011, 010, 001, 000
  N_1 <- c(1, 1, 1);  N_2 <- c(1, 1, 0);  N_3 <- c(1, 0, 1);  N_4 <- c(1, 0, 0);
  N_5 <- c(0, 1, 1);  N_6 <- c(0, 1, 0);  N_7 <- c(0, 0, 1);  N_8 <- c(0, 0, 0);
  Nbs <- matrix(c(N_1, N_2, N_3, N_4, N_5, N_6, N_7, N_8), nrow=8, byrow = TRUE)
  
  # initial time is set to 0
  t<-1
  while(t<(T+1)){ # while time is less than or equal to max. time given (T)
    GOL<-rbind(GOL, c(R0))  # attach a new row, R0, to GOL
    GOL[t, 1] <- u[t,1];
    GOL[t, N] <- u[t,2]
    if(t<=T){
      for(i in 2:(N-1)){
        #state of the neighbourhood of the i-th cell
        Neighbourhood<-c(GOL[t,i-1], GOL[t, i], GOL[t,i+1]);  
        j = 1;
        while (j<=8) {  
          #is the state of the neighbourhood identical to the j-th possibility
          if(identical(Neighbourhood,Nbs[j,])){ 
            # if it is the j-th possible neighbourhood, the j-th rule applies
            GOL[t+1,i]<-n[j]  
            j=10  # essentially breaks the while loop and saves a little time
          }
          j=j+1;  
        }
      }  
    }
    
    t<-t+1
  } 
  
  GOL<-GOL[1:T, 1:N];
  state_configuration<-0:N; time <-0:T;
  return(GOL)
  
}

# Example
rule_no = 90;
N = 6; T = 4;
Rands_0 <- FALSE; # not random initial state
s_0 <- NULL;      # initial state is a single 1 in the middle
s_0 <- c(0,1,1,1,0,0);  # initial state mentioned
u1 <- c(0,0);
u2 <- c(0,0);
u3 <- c(0,0);
u4 <- c(0,0);
u <- matrix(c(u1, u2, u3, u4), nrow=4, byrow=TRUE);
ECA_controlled(rule_no, N, T, s_0, u)->GOL;
state_configuration<-seq(0,N+1)
time<-seq(-0.5,T-0.5)
eca<-apply(rot(rot(rot(GOL))), 2, rev);
image( state_configuration, time
       , eca
       , axes=FALSE
       , col = c("#fcfc64", "darkgreen")
       , xlab = "configuration"
       , ylim = rev(range(time))
)
title(paste(paste("Rule", as.character(rule_no)), "(without control)"), cex=2)
axis(1, at = seq(0, N+1, by = 1), las=1)
axis(2, at = rev(seq(0, T-1, by = 1)), las=2)
grid(nx = N+2, ny = T,col = "grey", lwd=1.5)
grid(nx = N+2, ny = T,col = "black", lwd=1.5)
abline(v=0.5, col="red", lwd=2)
abline(v=N+0.5, col="red", lwd=2)
abline(h=T-1.5,lwd=2, col="black")
mtext("null boundary", side=3, at=0, col = "grey", cex=0.9)
mtext("null boundary", side=3, at=7, col = "grey", cex=0.9)
mtext("cellular domain", side=3, at=3.5, cex=1.2)

for (x in 3:ncol(GOL)-1){
  for (y in 1:nrow(GOL)){
    if(GOL[y,x]==0){
      text(
        state_configuration[x]+0.0
        , time[y]+0.5, GOL[y,x]
        , col="black"
        , srt=0
        , cex=2
        )
    }
    else {
      text(
        state_configuration[x]+0.0
        ,time[y]+0.5, GOL[y,x]
        , col = "white"
        , srt=0
        , cex=2
        )
    }
  }
}
