library(d3heatmap)
library(networkD3)

#d3 
d3heatmap(flea[,-7], scale="column", colors="Blues")


MisNodes_D3<-MisNodes[,-1]
rownames(MisNodes_D3)<-MisNodes[,1]
d3heatmap(MisNodes_D3, scale="column", colors="Blues")


#nework
src <- c("A", "A", 
         "B", "B","B", "B", 
         "C", "C", 
         "D")
target <- c("B", "C",
            "D", "J","E", "F", 
            "G", "H", 
            "I")
networkData <- data.frame(src, target)

simpleNetwork(networkData)




