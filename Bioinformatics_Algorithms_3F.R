#Bioinformatics_Algorithms_3F.R

#3F: Constructing Eulerian Cycles Problem
#Input: An adjacency list (Eulerian directed graph)
#Output: A Eulerian cycle in this graph


#Input#

df <- read_delim("Dna.txt", delim="-",col_names=FALSE)
Nodes <- gsub(" ","",df$X1)
Edges <- gsub("> ","",df$X2)
Edges <- stringr::str_split(Edges,pattern=",")

AdjacencyList <- lapply(Edges,as.numeric)
names(AdjacencyList) <- Nodes

#Main Function#

EulerianCycle <- function(AdjacencyList){
  randomNode <- sample(names(AdjacencyList),1) #Pick a random node
  Cycle <- c(randomNode) #Make a variable that will keep track of ongoing cycle
  while(length(AdjacencyList[[randomNode]])>0){ #While each random node picked has an edge inside
    randomEdgeIndex <- sample(seq_along(AdjacencyList[[randomNode]]),1) #Pick the index of an edge
    randomEdge <- as.character(AdjacencyList[[randomNode]][randomEdgeIndex]) #Find the value of the edge
    Cycle <- c(Cycle, randomEdge) #Add random Edge to Cycle
    AdjacencyList[[randomNode]] <- AdjacencyList[[randomNode]][-(randomEdgeIndex)] #Get rid of random Edge in AdjacencyList
    randomNode <- randomEdge
  }
  while(sum(unlist(lapply(AdjacencyList,length)))>0){ #while there are still edges to be accessed
    nodeswithEdges <- AdjacencyList[which(unlist(lapply(AdjacencyList,length),use.names=FALSE) != 0)] %>% names %>% as.numeric
    nodesinCycle <- as.numeric(Cycle)
    possiblenewStarts <- intersect(nodeswithEdges,nodesinCycle) 
    newStartIndex <- sample(seq_along(possiblenewStarts),1)
    newStart <- as.character(possiblenewStarts[newStartIndex]) #select a node newStart that's in Cycle and that's also in AdjacencyList that still has unexplored edges
    Cycle <- createnewCycle(Cycle,newStart) #form Cycle' by traversing Cycle
    while(length(AdjacencyList[[newStart]])>0){ #While each random node picked has an edge inside
      randomEdgeIndex <- sample(seq_along(AdjacencyList[[newStart]]),1) #Pick the index of an edge
      randomEdge <- as.character(AdjacencyList[[newStart]][randomEdgeIndex]) #find the value of the edge
      Cycle <- c(Cycle, randomEdge) #add random Edge to Cycle
      AdjacencyList[[newStart]] <- AdjacencyList[[newStart]][-(randomEdgeIndex)] #get rid of random Edge in AdjacencyList
      newStart <- as.character(randomEdge)
    }
  }
  return(Cycle)
}

#Helper Functions#

createnewCycle <- function(Cycle,newStart){
  newCycle <- as.numeric(Cycle)[1:(length(Cycle)-1)] #create a newCycle without the last element (which was a dead end)
  newStartCycleIndices <- which(newCycle == newStart) #find the indices of newStart in newCycle
  newStartCycleIndex <- newStartCycleIndices[sample(seq_along(newStartCycleIndices),1)] #pick a random one
  newCycleIndices <- c(newStartCycleIndex:length(newCycle),1:newStartCycleIndex)
  newCycle <- newCycle[newCycleIndices] #make a newCycle that starts at that node, goes to the end, and wraps around at the beginning
  return(newCycle)
}

#Tests#

AdjacencyList <- EulerianCycle(AdjacencyList)
AdjacencyList2 <- paste(as.numeric(AdjacencyList),"->", collapse="")
AdjacencyList3 <- gsub(" ","",AdjacencyList2)