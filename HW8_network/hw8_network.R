library(igraph)
library(networkD3)
#random
g <- erdos.renyi.game(10, 3/10)
plot(g)


## Scale Free
N <- 40
g <- sample_fitness(5*N, sample((1:40)^-2, N, replace=TRUE))
plot(g)


#network data
karate <- make_graph("Zachary")
plot(karate)