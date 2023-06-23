# sets working directory
setwd(".../TransitionGraph")

source("ECA_with_control.R");
# imports a function called ECA_controlled(n, N, T, Rand, initial_state, control)
# where:
# n     = rule number [0 to 256]
# N     = length of grid
# T     = number of time steps
# initial_state = a specific initial condition or NULL
# control = T×2 matrix with the border controls

# Creating a transition matrix/ graph
# Step 0: Possible controls
pos_controls <- expand.grid(rep(list(0:1), 2))
as.matrix(pos_controls) -> pos_controls 
colnames(pos_controls)<-NULL

# Step 1: Create all possible combinations of configuration for given N
N <- 3;
pos_comb <-  expand.grid(rep(list(0:1), N))
as.matrix(pos_comb) -> pos_comb_matrix 
colnames(pos_comb_matrix)<-NULL

# image(1:N, 1:(2^N), rot(pos_comb_matrix)
#     , main="Possible configurations"
#     , axes = TRUE, xlab = "configuration")

# Step 2: initiating Transition matrix
rule_no<-90;

#initiating Transition matrix
TransitionMatrix<-matrix(0, ncol=(2^N), nrow=(2^N))
T = 2;
for (j in 1:nrow(TransitionMatrix)){
  s0<-pos_comb_matrix[j,];  
  for (i in 1:4) {
    u1<-pos_controls[i,]
    u2<-c(0,0)
    u <- matrix(c(u1, u2), nrow=2, byrow=TRUE);
    ECA_trial<-ECA_controlled(rule_no, N, T, s0, u);
    ECA_final<-ECA_trial[T,3:ncol(ECA_trial)-1]
    matching_row <- which(apply(pos_comb_matrix, 1, function(row) all(row == ECA_final)))
    TransitionMatrix[j,matching_row]+1->TransitionMatrix[j,matching_row]
    print(matching_row)
  }
}

library(igraph)

graph_from_adjacency_matrix(TransitionMatrix)->plotgraph
l <-  layout_with_gem(plotgraph)
plot(  plotgraph
     , edge.arrow.size=.4
     , edge.color = "purple"
     , vertex.color = "white"
     , layout = l
     , edge.curved=0
     )
# reference for plotting : https://kateto.net/network-visualization
