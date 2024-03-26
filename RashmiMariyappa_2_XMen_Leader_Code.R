##Rashmi Mariyappa
##Network Analysis Class
##Final Project

##Load Package
library(igraph)

##Clear Memory
rm(list=ls())


#_______________________________
#OBTAINING THE GRAPH

##Read in File & Construct Graph
marvel <- read.csv('C:/Users/rmari/.../hero_network.csv', header = TRUE, sep = ",")
g_marvel=graph.data.frame(marvel, directed=FALSE, vertices=NULL)


##Edges & Vertices Count
ecount(g_marvel)    #574,467
## Vertices
vcount(g_marvel)    #6,426

#Is This Simple Graph?
## Check whether Self_loops exist, as do multiple edges
is.simple(g_marvel) #FALSE

##Check if Graph Has Weigts & Adding Them
E(g_marvel)$weight            #No Weights
E(g_marvel)$weight <-1        #Adding Weight of 1 to All Edges
E(g_marvel)$weight            #All Weights of 1

##Simply the Graph
g_Smarvel <- simplify(g_marvel, edge.attr.comb="sum")

E(g_Smarvel)$weight           

##Re-Check Whether Self Loops Exist & Multiple Edges on New
is.simple(g_Smarvel)          #Is Simple Now - TRUE

##Edges
ecount(g_Smarvel)   #167,207
##Vertices
vcount(g_Smarvel)   #6,426

##Max and Min Weights
table(E(g_Smarvel)$weight)
max(E(g_Smarvel)$weight)  #1,897
min(E(g_Smarvel)$weight)  #1

##Explore the Graph
is.weighted(g_Smarvel)        #Are the Edges Weighted - TRUE
is.simple(g_Smarvel)          #Is the Graph Simple    - TRUE
is.connected(g_Smarvel)       #Is the Graph Connected - FALSE


#_______________________________
#INITIAL VISUALIZATIONS

#Plot in R
par(mfrow=c(1,1), mar=c(0,0,0,0))                                       
plot(g_Smarvel,layout=layout_nicely, vertex.size=1.6, edge.width=((log(E(g_Smarvel)$weight+1))*.5), vertex.label=NA)
g_M1 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=1))
plot(g_M1,layout=layout_nicely, vertex.size=1.6, edge.width=((log(E(g_M1)$weight+1))*.5), vertex.label=NA)


##Export Node and Edge Data Frame for Gephi
#Code to Extract Frames Found At: http://www.vesnam.com/Rblog/viznets2/

#Nodes Data File
nodes_marvel <- data.frame(ID = c(1:vcount(g_Smarvel)), NAME = V(g_Smarvel)$name)
#write.csv(nodes_marvel, file="C:/Users/rmari/Desktop/IDS 564/Marvel Project/Saves/nodelistALL.csv")

#Edges Data File
edges_marvel <- as.data.frame(cbind(get.edgelist(g_Smarvel), (E(g_Smarvel)$weight)))
edges_marvel2 <- as.data.frame(get.edges(g_Smarvel, c(1:ecount(g_Smarvel))))
#write.csv(edges_marvel2, file="C:/Users/rmari/Desktop/IDS 564/Marvel Project/Saves/edgelistALL2.csv")


#_______________________________
#SUMMARY STATISTICS

##Census of Cliques
table(sapply(maximal.cliques(g_Smarvel), length))
#Results Maximal Cliques
#Output Too Many


##Census of All Connected Components
comp_marvel <-decompose.graph(g_Smarvel)
table(sapply(comp_marvel, vcount))
#Results Connected Components
#2    7    9 6408 
#1    1    1    1

#Graph Density
graph.density(g_Smarvel) #0.008099731

#Average Degree
mean(degree(g_Smarvel))   #52.04077
median(degree(g_Smarvel)) #20
max(degree(g_Smarvel))    #1906
min(degree(g_Smarvel))    #1

#Summary of Degree and Weights
summary(degree(g_Smarvel))
#Min.   1st Qu.  Median Mean  3rd Qu.  Max. 
#1.00   10.00   20.00   52.04 47.00    1906.00 

summary(E(g_Smarvel)$weight)
#Min.     1st Qu.  Median   Mean    3rd Qu. Max. 
#1.000    1.000    1.000    3.422   3.000   1894.000 


#Clustering
transitivity(g_Smarvel)  #0.1945397


#Extacting Giant Componant for Summary Statistics
g_marvSGC <- clusters(g_Smarvel)
top_no <- which.max(g_marvSGC$csize)
g_marv_Giant <- induced.subgraph(g_Smarvel, which(g_marvSGC$membership == top_no))

table(g_marvSGC$csize)
# 2    7    9 6408 
# 1    1    1    1 

##Edges & Vertices Count
ecount(g_marv_Giant)    #167,151
## Vertices
vcount(g_marv_Giant)    #6,408

##Diameter
diameter(g_marv_Giant)  #13

##Average Path Length
average.path.length(g_marv_Giant)  #2.638431

transitivity(g_marv_Giant)  #0.1945347
vertex.connectivity(g_marv_Giant)
edge.connectivity(g_marv_Giant)


#_______________________________
#DATA REDUCTION

#Oscure Characters
deg <- as.data.frame(degree(g_Smarvel))
#Low Degree Characters
#Saja
#Desadia
#Vigil
#Goom


#Delete 1---------------------------------
g_M1 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=1))

comp_marvel1 <-decompose.graph(g_M1)
table(sapply(comp_marvel1, vcount))
#Results Connected Components
#   1    2    3    4    6    7 4539  
#1852    6    2    1    1    1    1 

##Edges
ecount(g_M1)   #77,532
##Vertices
vcount(g_M1)   #6426


#Delete 2---------------------------------
g_M2 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=2))

comp_marvel2 <-decompose.graph(g_M2)
table(sapply(comp_marvel2, vcount))
#Results Connected Components
#   1    2    3    4    5    7 3460   
#2922    5    2    1    2    2    1 

##Edges
ecount(g_M2)   #46,862
##Vertices
vcount(g_M2)   #6,426


#Delete 3---------------------------------
g_M3 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=3))

comp_marvel3 <-decompose.graph(g_M3)
table(sapply(comp_marvel3, vcount))
#Results Connected Components
#   1    2    3    4    5    6 2794   
#3592    7    2    1    2    1    1 

##Edges
ecount(g_M3)   #31,738
##Vertices
vcount(g_M3)   #6,426


#Delete 4---------------------------------
g_M4 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=4))

comp_marvel4 <-decompose.graph(g_M4)
table(sapply(comp_marvel4, vcount))
#Results Connected Components
#   1    2    3    4    5    6   10   12 2343   
#4028    3    1    2    2    1    1    1    1 

##Edges
ecount(g_M4)   #23,402
##Vertices
vcount(g_M4)   #6,426


#Delete 5---------------------------------
g_M5 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=5))

comp_marvel5 <-decompose.graph(g_M5)
table(sapply(comp_marvel5, vcount))
#Results Connected Components
#   1    2    3    4    6    7   12 1987   
#4379    8    3    1    2    1    1    1 

##Edges
ecount(g_M5)   #18,082
##Vertices
vcount(g_M5)   #6,426


##Obtaining the Giant Component for Community Detection
#Sub-Graph of Giant Component
#Please Note: Coding Needed to Be Researched On-line, Code for this Found at the Following Site
#http://jfaganuk.github.io/2015/01/24/basic-network-analysis/
g_M5_comp <- clusters(g_M5)
top_no <- which.max(g_M5_comp$csize)
g_m5_Giant <- induced.subgraph(g_M5, which(g_M5_comp$membership == top_no))

table(g_M5_comp$csize)
#   1    2    3    4    6    7   12 1987
#4379    8    3    1    2    1    1    1

##Edges
ecount(g_m5_Giant)   #17952
##Vertices
vcount(g_m5_Giant)   #1987

#Plot
par(mfrow=c(1,1), mar=c(0,0,0,0))                                       
plot(g_m5_Giant,layout=layout_nicely, vertex.size=1.6, edge.width=((log(E(g_m5_Giant)$weight+1))*.5), vertex.label=NA)
plot(g_m5_Giant,layout=layout.drl, vertex.size=1.6, edge.width=((log(E(g_m5_Giant)$weight+1))*.5), vertex.label=NA)


#Delete 10---------------------------------
g_M10 <- delete.edges(g_Smarvel, which(E(g_Smarvel)$weight <=10))

comp_marvel10 <-decompose.graph(g_M10)
table(sapply(comp_marvel10, vcount))
#Results Connected Components
#   1    2    3    4    5    6    7 1097    
#5261   13    4    3    1    1    1    1

##Edges
ecount(g_M10)   #7,935
##Vertices
vcount(g_M10)   #6,426


#Delete Degree Greater Than 3---------------------------------
g_M10_V3 <- induced.subgraph(g_M10, which(degree(g_M10, mode ="all") > 3))

comp_marvel103 <-decompose.graph(g_M10_V3)
table(sapply(comp_marvel103, vcount))
#Results Connected Components
#   4   5   7 739    
#   1   1   1   1 

##Edges
ecount(g_M10_V3)   #7,262
##Vertices
vcount(g_M10_V3)   #755


#Delete Degree Greater Than 5---------------------------------
g_M10_V5 <- induced.subgraph(g_M10, which(degree(g_M10, mode ="all") > 5))

comp_marvel105 <-decompose.graph(g_M10_V5)
table(sapply(comp_marvel105, vcount))
#Results Connected Components
#   7 554 
#   1   1 

##Edges
ecount(g_M10_V5)   #7,262
##Vertices
vcount(g_M10_V5)   #561


#Delete Degree Greater Than 10---------------------------------
g_M10_V10 <- induced.subgraph(g_M10, which(degree(g_M10, mode ="all") > 10))

comp_marvel1010 <-decompose.graph(g_M10_V10)
table(sapply(comp_marvel1010, vcount))
#Results Connected Components
#   319 
#   1 

##Edges
ecount(g_M10_V10)   #5095
##Vertices
vcount(g_M10_V10)   #319


##Obtaining the Giant Component for Community Detection
#Sub-Graph of Giant Component
#Please Note: Coding Needed to Be Researched On-line, Code for this Found at the Following Site
#http://jfaganuk.github.io/2015/01/24/basic-network-analysis/
##g_M10_comp <- clusters(g_M10)
##top_no <- which.max(g_M10_comp$csize)
##g_m10_Giant <- induced.subgraph(g_M10, which(g_M10_comp$membership == top_no))

##table(g_M10_comp$csize)
#   1    2    3    4    5    6    7 1097
#5261   13    4    3    1    1    1    1

##Edges
##ecount(g_m10_Giant)   #7850
##Vertices
##vcount(g_m10_Giant)   #1097

#Plot
par(mfrow=c(1,1), mar=c(0,0,0,0))                                       
plot(g_M10_V10,layout=layout_nicely, vertex.size=1.6, edge.width=((log(E(g_M10_V10)$weight+1))*.5), vertex.label=NA)
plot(g_M10_V10,layout=layout.drl, vertex.size=1.6, edge.width=((log(E(g_M10_V10)$weight+1))*.5), vertex.label=NA)
plot(g_M10_V10,layout=layout.fruchterman.reingold, vertex.size=1.6, edge.width=((log(E(g_M10_V10)$weight+1))*.5), vertex.label=NA)


#----------------------- Louvain ----------------------------#
M10_V10_louv <- cluster_louvain(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_louvMem <- membership(M10_V10_louv)
table(sizes(M10_V10_louv))

## Plot___________________________
plot(M10_V10_louv, g_M10_V10, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=1.6, edge.width=(log(E(g_M10_V10)$weight+1)*.25))

new_cols <- c("brown1", "goldenrod1", "yellow3", "springgreen2", "aquamarine2", "deepskyblue", "slateblue1", "mediumorchid1", "hotpink")[membership(M10_V10_louv)]
plot(M10_V10_louv, g_M10_V10, layout=layout.fruchterman.reingold, col=new_cols, vertex.label=V(g_M10_V10)$label2, vertex.size=3.5, vertex.label.color="brown", vertex.label.family = "sans", vertex.label.cex=.6, edge.width=(log(E(g_M10_V10)$weight+1)*.07), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])
plot(M10_V10_louv, g_M10_V10, layout=layout.fruchterman.reingold, col=new_cols, vertex.label=NA, vertex.size=3, edge.width=(log(E(g_M10_V10)$weight+1)*.07), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])

plot(M10_V10_louv, g_M10_V10, layout=layout.drl, col=new_cols, vertex.label=NA, vertex.size=3, edge.width=(log(E(g_M10_V10)$weight+1)*.07), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])
plot(M10_V10_louv, g_M10_V10, layout=layout_nicely, col=new_cols, vertex.label=NA, vertex.size=3, edge.width=(log(E(g_M10_V10)$weight+1)*.07), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])


V(g_M10_V10)$label2 <- ""
V(g_M10_V10)[63]$label2 <- "X"    
V(g_M10_V10)[261]$label2 <- "SNOWBIRD"
V(g_M10_V10)[102]$label2 <- "CAP AMERICA" 



dev.off()


#_______________________________________________
#Alpha Flight
Clust_1 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==1))
V(Clust_1)$name #List All Names
V(g_M10_V10)[261]$name
V(g_M10_V10)$name == "SNOWBIRD/NARYA/ANNE "

#Avengers
Clust_2 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==2))
V(Clust_2)$name #List All Names
V(g_M10_V10)[102]$name
V(g_M10_V10)$name == "CAPTAIN AMERICA"

#Defenders?
Clust_3 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==3))
V(Clust_3)$name #List All Names

#Squadron Supreme?
Clust_4 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==4))
V(Clust_4)$name #List All Names

#Spiderman
Clust_5 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==5))
V(Clust_5)$name #List All Names
V(g_M10_V10)[102]$name
V(g_M10_V10)$name == "CAPTAIN AMERICA"

#Fantastic 4
Clust_6 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==6))
V(Clust_6)$name #List All Names

#Thunderbolts
Clust_7 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==7))
V(Clust_7)$name #List All Names

#Thor
Clust_8 <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==8))
V(Clust_8)$name #List All Names

#X-Men
#Clust_9



#_______________________________________________
##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$louvMem <- M10V10_louvMem

#Find X-Man
V(g_M10_V10)$name   #List All Names
V(g_M10_V10)$name == "PROFESSOR X/CHARLES "
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$louvMem #Cluster 9

#Find X-Men
Clust_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==9))
V(Clust_XMen)$name #List All Names
"ROGUE /"
"CYCLOPS/SCOTT SUMMER"
"BEAST/HENRY &HANK& P"
"JUBILEE/JUBILATION L"
"ANGEL/WARREN KENNETH"
"STORM/ORORO MUNROE S"
"MARVEL GIRL/JEAN GRE"
"PSYLOCKE/ELISABETH B"
"WOLVERINE/LOGAN "
"PROFESSOR X/CHARLES "
"ICEMAN/ROBERT BOBBY "
"COLOSSUS II/PETER RA"
"HAVOK/ALEX SUMMERS "
"GAMBIT/REMY LEBEAU "
"BISHOP /"
"NIGHTCRAWLER/KURT WA"
"SHADOWCAT/KATHERINE "

#Find Top X-Men
deg_list  <- as.data.frame(degree(Clust_XMen))
deg_list
V(Clust_XMen)$degree


#----------------------- Spinglass ----------------------------#
M10_V10_spin <- spinglass.community(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_spinMem <- membership(M10_V10_spin)
table(sizes(M10_V10_spin))

##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$spinMem <- M10V10_spinMem

#Find X-Man
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$spinMem #Cluster 6

#Find X-Men
Clust_XMen_Spin <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$spinMem==6))
V(Clust_XMen_Spin)$name #List All Names

#Find Top X-Men
deg_list2  <- as.data.frame(degree(Clust_XMen_Spin))


#----------------------- Leading Eigen ----------------------------#
M10_V10_eig <- cluster_leading_eigen(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_eigMem <- membership(M10_V10_eig)
table(sizes(M10_V10_eig))

##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$eigMem <- M10V10_eigMem

#Find X-Man
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$eigMem #Cluster 2

#Find X-Men
Clust_XMen_Eig <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$eigMem==2))
V(Clust_XMen_Eig)$name #List All Names

#Find Top X-Men
deg_list7  <- as.data.frame(degree(Clust_XMen_Eig))


#----------------------- Fast Greedy ----------------------------#
M10_V10_fast <- fastgreedy.community(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_fastMem <- membership(M10_V10_fast)
table(sizes(M10_V10_fast))

##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$fastMem <- M10V10_fastMem

#Find X-Man
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$fastMem #Cluster 6

#Find X-Men
Clust_XMen_Fast <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$fastMem==5))
V(Clust_XMen_Fast)$name #List All Names

#Find Top X-Men
deg_list3  <- as.data.frame(degree(Clust_XMen_Fast))


#----------------------- WalkTrap ----------------------------#
M10_V10_walk <- walktrap.community(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_walkMem <- membership(M10_V10_walk)
table(sizes(M10_V10_walk))

##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$walkMem <- M10V10_walkMem

#Find X-Man
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$walkMem #Cluster 4

#Find X-Men
Clust_XMen_Walk <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$walkMem==4))
V(Clust_XMen_Walk)$name #List All Names

#Find Top X-Men
deg_list4  <- as.data.frame(degree(Clust_XMen_Walk))


#----------------------- Label Prop ----------------------------#
M10_V10_lab <- label.propagation.community(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_labMem <- membership(M10_V10_lab)
table(sizes(M10_V10_lab))

##Get Cluster Membership and Add Vertex Attribute
V(g_M10_V10)$labMem <- M10V10_labMem

#Find X-Man
#63 Professor X

#Find X-Men
V(g_M10_V10)[63]$name
V(g_M10_V10)[63]$labMem #Cluster 2

#Find X-Men
Clust_XMen_Lab <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$labMem==2))
V(Clust_XMen_Lab)$name #List All Names

#Find Top X-Men
deg_list5  <- as.data.frame(degree(Clust_XMen_Lab))

#Find Index of X-Men
V(g_M10_V10)[33]$name
V(g_M10_V10)$name == "ANGEL/WARREN KENNETH"
#[33] = "ANGEL/WARREN KENNETH"


#___________________________________
#Differences Between Clusters
list.vertex.attributes(g_M10_V10)

table(V(g_M10_V10)$louvMem)   #9  Clusters | #92 X-Men | #6/9  sig  = .66
table(V(g_M10_V10)$spinMem)   #13 Clusters | #92 X-Men | #7/13 sig  = .54
table(V(g_M10_V10)$fastMem)   #8  Clusters | #92 X-Men | #6/8  sig  = .75
table(V(g_M10_V10)$walkMem)   #12 Clusters | #81 X-Men | #6/12 sig  = .55
table(V(g_M10_V10)$labMem)    #9  Clusters | #86 X-Men | NA
table(V(g_M10_V10)$eigMem)    #9  Clusters | #95 X-Men | NA

Louv_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$louvMem==9))
V(Louv_XMen)$name

Spin_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$spinMem==6))
V(Spin_XMen)$name

Fast_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$fastMem==5))
V(Fast_XMen)$name

Walk_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$walkMem==4))
V(Walk_XMen)$name

Lab_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$labMem==2))
V(Lab_XMen)$name

Eig_XMen <- induced_subgraph(g_M10_V10, which(V(g_M10_V10)$eigMem==2))
V(Eig_XMen)$name

##Test Community Significance
community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  # Total degree among nodes in the vs list, minus the degree within the subgraph 
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}


#Test Louvain
for(i in 1:9) {
  comp_sig <- V(g_M10_V10)[louvMem==i]
  print(i)
  print(community.significance.test(g_M10_V10, comp_sig))
}


#Test SpinMem
for(i in 1:13) {
  comp_sig <- V(g_M10_V10)[spinMem==i]
  print(i)
  print(community.significance.test(g_M10_V10, comp_sig))
}


#Test FastMem
for(i in 1:8) {
  comp_sig <- V(g_M10_V10)[fastMem==i]
  print(i)
  print(community.significance.test(g_M10_V10, comp_sig))
}


#Test WalkMem
for(i in 1:12) {
  comp_sig <- V(g_M10_V10)[walkMem==i]
  print(i)
  print(community.significance.test(g_M10_V10, comp_sig))
}


#___________________________________
#Centrality Measures
V(Clust_XMen)$name #List All Names

#Closeness
?closeness
close <- as.data.frame(closeness(Clust_XMen, mode = "all", weights=Clust_XMen$weight, normalized =TRUE))

#Eigen
eigen <- as.data.frame(eigen_centrality(Clust_XMen, directed=FALSE, scale = TRUE, weights=Clust_XMen$weight))

#Betweenness
?betweenness

# Will use the inverse of log weight for shortest path calculations
inv_weight <- 1/log(E(g_M10_V10)$weight + 1)
#betw <- as.data.frame(betweenness(g_M10_V10, directed=FALSE, weights = inv_weight, nobigint=TRUE, normalized=TRUE))
betw2 <- as.data.frame(betweenness(g_M10_V10, directed=FALSE, weights = inv_weight, nobigint=TRUE, normalized=TRUE))
#betw3 <- as.data.frame(betweenness(g_M10_V10, directed=FALSE, weights = inv_weight, nobigint=FALSE, normalized=TRUE))

g_M10_V10_copy <- g_M10_V10
V(g_M10_V10_copy)$betw <- betw2[,1]

list.vertex.attributes(g_M10_V10_copy)

#X-Men Only Betweeness Scale
Clust_XMen_bet <- induced_subgraph(g_M10_V10_copy, which(V(g_M10_V10_copy)$louvMem==9))
V(Clust_XMen_bet)$name #List All Names

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
Rnorm <- range01(V(Clust_XMen_bet)$betw)
V(Clust_XMen_bet)$betwnorm <- Rnorm

V(Clust_XMen_bet)$betwnorm

b <- as_data_frame(Clust_XMen_bet, what="vertices")



#___________________________________
#Sub-Graph of Top 10
#Find Index Numbers of Top X-Men

#Find X-Man
V(Clust_XMen)$name   #List All Names
V(Clust_XMen)$name == "CYCLOPS/SCOTT SUMMER"
V(Clust_XMen)[27]$name
"CYCLOPS/SCOTT SUMMER" #(6)
"STORM/ORORO MUNROE S"
"WOLVERINE/LOGAN "
"COLOSSUS II/PETER RA"
"CANNONBALL II/SAM GU"
"ROGUE /"
"BEAST/HENRY &HANK& P"
"MARVEL GIRL/JEAN GRE"
"NIGHTCRAWLER/KURT WA"
"ANGEL/WARREN KENNETH"


par(mfrow=c(1,1), mar=c(0,0,0,0))
g_topten <- induced.subgraph(Clust_XMen, v=c("CYCLOPS/SCOTT SUMMER", "STORM/ORORO MUNROE S", "WOLVERINE/LOGAN ", "COLOSSUS II/PETER RA", "CANNONBALL II/SAM GU", "ROGUE /", "BEAST/HENRY &HANK& P", "MARVEL GIRL/JEAN GRE", "NIGHTCRAWLER/KURT WA", "ANGEL/WARREN KENNETH"))
plot(g_topten)

new_cols <- c("brown1", "goldenrod1", "yellow3", "springgreen2", "aquamarine2", "deepskyblue", "slateblue1", "mediumorchid1", "hotpink")[membership(M10_V10_louv)]

plot(g_topten, layout=layout.fruchterman.reingold, col=new_cols, vertex.label=V(g_M10_V10)$label2, vertex.size=3.5, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.6, edge.width=(log(E(g_topten)$weight+1)*.5), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])

plot(M10_V10_louv, g_M10_V10, layout=layout.fruchterman.reingold, col=new_cols, vertex.label=NA, vertex.size=3, edge.width=(log(E(g_M10_V10)$weight+1)*.07), edge.color = c("grey47", "firebrick1")[crossing(M10_V10_louv,g_M10_V10) + 1])


#New Graph
V(g_topten)$name
V(g_topten)[10]$name
V(g_topten)$label2 <- ""
V(g_topten)[1]$label2 <- "Rogue"    
V(g_topten)[2]$label2 <- "Cyclops"
V(g_topten)[3]$label2 <- "Beast" 
V(g_topten)[4]$label2 <- "Angel"   
V(g_topten)[5]$label2 <- "Storm"
V(g_topten)[6]$label2 <- "Jean Grey" 
V(g_topten)[7]$label2 <- "Wolverine"    
V(g_topten)[8]$label2 <- "Colossus II"
V(g_topten)[9]$label2 <- "Cannonball II" 
V(g_topten)[10]$label2 <- "Nightcrawler"

V(g_topten)$color <- "grey"
V(g_topten)[1]$color <- "palegreen3"    
V(g_topten)[2]$color <- "palevioletred"
V(g_topten)[3]$color <- "dodgerblue" 
V(g_topten)[4]$color <- "linen"   
V(g_topten)[5]$color <- "azure"
V(g_topten)[6]$color <- "tomato" 
V(g_topten)[7]$color <- "sienna3"    
V(g_topten)[8]$color <- "grey60"
V(g_topten)[9]$color <- "lightgoldenrod" 
V(g_topten)[10]$color <- "lightslateblue"


#Plot1
plot(g_topten, layout=layout.fruchterman.reingold, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5), edge.color="grey47")


#Plot2 - Do not Use
plot(g_topten, layout=layout.fruchterman.reingold, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, vertex.frame.color="grey47", edge.width=(log(E(g_topten)$weight+1)*.5), edge.color="grey47")


#Plot3 0 Use this One
p1 <- V(g_topten)[7]
p2 <- V(g_topten)[2]
E(g_topten)$color <- "grey47"
E(g_topten)[p1%--%p2]$color <- "firebrick1"
plot(g_topten, layout=layout.fruchterman.reingold, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5))

#other Versions of 3
plot(g_topten, layout=layout_nicely, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5))
plot(g_topten, layout=layout.kamada.kawai, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5))
plot(g_topten, layout=layout.sphere, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5))
plot(g_topten, layout=layout.drl, vertex.label=V(g_topten)$label2, vertex.size=25, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.8, edge.width=(log(E(g_topten)$weight+1)*.5))


#___________________________________
##OTHER TESTS

#AlphaCentrality________________
?alpha_centrality
#alpha_centrality(graph, nodes = V(graph), alpha = 1, loops = FALSE, exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE)


#EdgeBetweenness________________

topten_GirNew <- edge.betweenness.community(g_topten, weights=E(g_topten)$weight)
topten_GirMem <- membership(topten_GirNew)

dendPlot(topten_GirNew)
is_hierarchical(MM10_V10_GirNew)

GN_test<-cut_at(MM10_V10_GirNew, no = 2)
table(GN_test, V(g_primschool)$classname, useNA = c("no"))

GN_test<-cut_at(schoolCom_GirNew, no = 10)
table(GN_test, V(g_primschool)$classname, useNA = c("no"))
plot(schoolCom_GirNew, g_primschool, layout=layout.fruchterman.reingold, vertex.label=stud.class, vertex.size=4, vertex.label.color="black", vertex.label.family = "sans", vertex.label.cex=.48, edge.width=(log(E(g_primschool)$weight+1))*.07, edge.color = c("grey27", "firebrick1")[crossing(schoolCom_GirNew,g_primschool) + 1])
dev.off()

#----------------------- Label Prop ----------------------------#
M10_V10_lab <- label.propagation.community(g_M10_V10, weights=E(g_M10_V10)$weight)
M10V10_labMem <- membership(M10_V10_lab)
table(sizes(M10_V10_lab))


#___________________________________
#LOAD AND SAVE VERSIONS

##Save Verions
#save(g_M10_V10, file="C:/Users/rmari/...RedE10V10mem.rda")
#save(g_topten, file="C:/Users/rmari/...gtopten.rda")
#save(Clust_XMen, file="C:/Users/rmari/...XmenLouv.rda")

##Load Saved Versions
load(file="C:/Users/rmari/...RedE10V10mem.rda")
list.vertex.attributes(g_M10_V10)
