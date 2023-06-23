
source("ECA_with_control.R");
# imports a function called ECA_controlled(n, N, T, Rand, initial_state, control)
# where:
# n     = rule number [0 to 256]
# N     = length of grid
# T     = number of time steps
# initial_state = a specific initial condition or NULL
# control = T×2 matrix with the border controls

rule_no<-105;  # rule number 
N <- 3;        # no. of cells

# Creating a transition matrix/ graph
# Step 0: Possible controls
pos_controls <- expand.grid(rep(list(0:1), 2))
as.matrix(pos_controls) -> pos_controls 
colnames(pos_controls)<-NULL

# Step 1: Create all possible combinations of configuration for given N
pos_comb <-  expand.grid(rep(list(0:1), N))
as.matrix(pos_comb) -> pos_comb_matrix 
colnames(pos_comb_matrix)<-NULL

# Step 2: initiating Transition matrix
TransitionMatrix<-matrix(0, ncol=(2^N), nrow=(2^N)) #initiating Transition matrix

T = 2;
# Step 3: Filling values of Transition Matrix
for (j in 1:nrow(TransitionMatrix)){
  s0<-pos_comb_matrix[j,];  # set the initial configuration from the possible combinations
  for (i in 1:4) {
    u1<-pos_controls[i,] # set control from its possible combinations
    u2<-c(0,0)  # final "control" is just (0,0)
    u <- matrix(c(u1, u2), nrow=2, byrow=TRUE);
    ECA_trial<-ECA_controlled(rule_no, N, T, s0, u);  
    ECA_final<-ECA_trial[T,3:ncol(ECA_trial)-1] #Final state with s_0 and u1
    matching_row <- which(apply(pos_comb_matrix, 1, function(row) all(row == ECA_final)))
    TransitionMatrix[j,matching_row]+1->TransitionMatrix[j,matching_row]
  }
}

library(igraph)
# creates a graph from the Transition (Adjacency) matrix
graph_from_adjacency_matrix(TransitionMatrix)->plotgraph
l <-  layout_with_kk(plotgraph)
# plots the transition graph                                
plot(  plotgraph  
     , edge.arrow.size=.4
     , edge.color = "purple"
     , vertex.color = "white"
     , layout = l
     , edge.curved=0
     , main=paste("Rule", rule_no)
     )
# reference: https://kateto.net/network-visualization
