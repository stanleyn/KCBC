KCBC=function(Net){
	#This function is meant to implement k core based clustering 
	#Algorithm (2) decscribed http://www.mlgworkshop.org/2016/paper/MLG2016_paper_29.pdf
	#objective is to identify structures by doing k core decompositions
	#return a set of 'node2super node assignments'. In this case, each entry of the node-length vector gives the structure assignment for the node. 

	#Inputs:
		#Net: an igraph graph object

	#Output:
		#Node2SN: This is a partition of nodes to 'super nodes.' 
		#In this context each super node is one of the structures discovered through k-core decomposition


#dependencies
library('igraph')

#keep track of number of edges since we go until non empty
EC=ecount(Net)

#define a max structure number
maxStruct=0

#create vector of Node to super node assignments
Node2SN=rep(0,vcount(Net))


while(EC>0){
#step 1 is to find the core numbers for each node
CoreNum=coreness(Net)
MaxCore=max(CoreNum)

#choosing the decomposition sets
for(k in MaxCore:1){
	RelNodes=which(CoreNum==k)
	
	#get subgraph in the decomposition set
	DecompSetSub=induced_subgraph(Net,RelNodes)
	
	#identify connected components and treat them as structures 
	compIds=components(DecompSetSub)$membership
	
	#get max of current Structure
	StructMax=max(Node2SN)

	#transoform compIds to that we have unique labels
	compIds=compIds+StructMax

	#update the node2SN representation
	Node2SN[RelNodes]=compIds

	#update Net by deleting edges that were identified by this structure
	intEdges=which(is.element(E(Net),E(DecompSetSub)))
	
	Net=delete_edges(Net,intEdges)
	EC=ecount(Net)
	
} #k



} #while ecount

Node2SN
} #function