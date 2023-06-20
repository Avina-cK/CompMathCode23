# Uncomment the 4 lines below to download and install library: 'binaryLogic' 
# url <- "https://cran.r-project.org/src/contrib/Archive/binaryLogic/binaryLogic_0.3.9.tar.gz"
# pkgFile <- "binaryLogic_0.3.9.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)

'
n     = rule number [0 to 256]
N     = length of grid
T     = number of time steps
Rand  = initial condition is random or one single live cell in the middle  
initial_state = a specific initial condition or NULL
'

ECA<-function(n, N, T, Rand, initial_state){
  library(binaryLogic)
  n<-as.binary(n, n=8)# converting rule number to binary
  N_i<- N;
  N<-N+2;             # adding one 0 cell on either side
  R0<-rep(0, N);      # initialize the CA space
  GOL<-matrix(rep(0,N), ncol = N); 
  if(Rand==TRUE){GOL[1,]<-sample(c(1,0),N, replace=TRUE)}
  if(Rand==FALSE){
    if (is.null(initial_state)){
      GOL[1,floor(N/2)]<-1
    } else{GOL[1,1:N_i+1]<-initial_state}
  }
  # neighbourhood possibilities
  N_1 <- c(1, 1, 1);  N_2 <- c(1, 1, 0);  N_3 <- c(1, 0, 1);  N_4 <- c(1, 0, 0);
  N_5 <- c(0, 1, 1);  N_6 <- c(0, 1, 0);  N_7 <- c(0, 0, 1);  N_8 <- c(0, 0, 0);
  Nbs <- matrix(c(N_1, N_2, N_3, N_4, N_5, N_6, N_7, N_8), nrow=8, byrow = TRUE)
  t<-1
  rot <- function(x) t(apply(x, 2, rev))
  while(t<(T+1)){
    GOL<-rbind(GOL, c(R0))
    if(t<T){
      for(i in 2:(N-1)){
        Neighbourhood<-c(GOL[t,i-1], GOL[t, i], GOL[t,i+1]);
        j = 1;
        while (j<=8) {
          if(identical(Neighbourhood,Nbs[j,])){
            GOL[t+1,i]<-n[j]
            #j=10  # essentially breaks the while loop and saves a little time
            }
          j=j+1;
        }
      }  
    }
    
    t<-t+1
  } 
  
  GOL<-GOL[1:T, 1:N];
  state_configuration<-0:N; time <-0:T;
  return(image( state_configuration, time
               , rot(rot(rot(GOL)))
               , axes=FALSE
               , col = gray.colors(2,0.95,0)
               , ylim = rev(range(time))
               )
  )
  
}

# Example
rule_no = 30;
N = 6; T = 10;
Rands_0 <- FALSE; # not random initial state
s_0 <- NULL;      # initial state is a single 1 in the middle
s_0 <- c(0,1,1,1,0,0);
ECA(rule_no, N, T, Rands_0, s_0)
title(paste("Rule", as.character(rule_no)))
axis(1, at = seq(0, N+1, by = 1), las=1)
axis(2, at = rev(seq(0, T-1, by = 1)), las=2)
grid(nx = N+2, ny = T,col = "grey")
