library (igraph)

swiss_data<-load("hw3.RData")

colnames (coauthorship) %in% (node.attr$name)
# Check that the names of the authors in the matrix match the names in the node attribute data frame
graph <- graph.adjacency(coauthorship, mode = "directed")
# Creating network
graph

# Add a node attributes(one of them is University affiliation)
V(graph)$affiliation<-node.attr$affiliation
V(graph)$status<-node.attr$status
V(graph)$subfield<-node.attr$subfield
V(graph)$male<-node.attr$male

par(mar=c(0,0,0,0)) # get rid of plot margins
# Plot the network in such a way that different colours will represent scientists' university affiliations
plot(graph, #our network
     vertex.size = 6, 
     vertex.label = V(graph)$name, # display name as node labels 
     vertex.label.cex = 0.1, 
     vertex.label.color = "black",
     edge.color = "lightblue", 
     edge.arrow.size = 0.1, 
     vertex.color = V(graph)$affiliation, # the colour of nodes represent scientists' university affiliations.
     layout=layout_with_fr # the layout of the network
)
# There seems to be more collaboration between the same affiliation because there are more intense arrows between the same colors.
# However, there are also cross-university collaborations, but more common are the same affiliations.
# Therefore, we cannot say that cross-university collaborations are very common.

# Degree centrality
degree_in<-degree(graph, mode = "in")
degree_in
max (degree_in) #what is the highest indegree value in this network?
in_author <- which.max (degree_in) #who (which node) has the highest indegree value in the network?
node.attr[in_author,]
# According to indegree centrality,the author Varone, F, is the more prominent in the network.

degree_out<-degree(graph, mode = "out")
degree_out
max (degree_out) #what is the highest outdegree value in this network?
max_author<-which.max (degree_out) #who (which node) has the highest outdegree value in the network?
node.attr[max_author,]
# According to outdegree centrality,the author Varone, F, again is the more prominent in the network.

betweenness_g <- betweenness(graph, directed=TRUE, normalized=TRUE)
b_author<-which.max(betweenness_g)
node.attr[b_author,]
# The author Sciarini, P has the highest betweenness centrality so that sits in between the most of the pairs of other nodes.

ec <- round(evcent(graph)$vector, 2)
ec
max (ec)
ec_author <- which.max (ec)
node.attr[ec_author,]
# According to the eigenvector, the most important node in the network is author Varone,F.
# So, we can argue that this node that has high eigenvector centrality is connected to other
# highly connected nodes in the network.

# Summing up centrality scores
cent_graph <- data.frame(name=V(graph)$name, indegree=degree_in,
                           outdegree=degree_out,
                           betweenness=betweenness_g, 
                           eigenvector=ec)

# Let's examine the top 10 authors for different centralities

# Sort the data frame by indegree centralities
sorted_indeg <- cent_graph[order(-cent_graph$indegree),]
# Print the top 10 scientists by degree centralities
top10_in<- (head (sorted_indeg, 10))
top10_in

# Sort the data frame by outdegree centralities
sorted_outdeg <- cent_graph[order(-cent_graph$outdegree),]
# Print the top 10 scientists by outdegree centralities
top10_out<- (head (sorted_outdeg, 10))
top10_out

# Sort the data frame by between centralities
sorted_bw <- cent_graph[order(-cent_graph$betweenness),]
# Print the top 10 scientists by betweenness centralities
top10_bw<- (head (sorted_bw, 10))
top10_bw

sorted_ec <- cent_graph[order(-cent_graph$eigenvector),]
# Print the top 10 scientists by eigenvector centralities
top10_ec<- (head (sorted_ec, 10))
top10_ec

# Making the graph undirected for some algorithms
graph_u <- as.undirected(graph)
graph_u

# First algorithm,let's try Cluster_fast_greedy 
cfg <- cluster_fast_greedy(graph_u)

membership (cfg) #print each node's community membership 

table (membership(cfg)) #how many members are there in each community?

modularity (cfg) #check the modularity value
# I achieved a modularity value of 0.66 indicating the network has a relatively strong structure with well-defined communities.

par(mar=c(0,0,0,0))
plot(cfg, graph_u, vertex.cex = 0.5, vertex.size=degree(graph_u, mode="in")+1, 
     vertex.label.cex = 0.3, vertex.label.color= "black", edge.curved=T,  edge.arrow.size = 0.5)

# When I looked at the cluster, I saw that people from the same city and university were in the mostly same clusters.
# For example, the large author nodes (Sciriani P, and Hug, S) in the community with yellow color, both have the same affiliations (Uni-GE) and the city of GE.
# Also, many other nodes have GE city in the yellow nodes community. There was also a same similarity for ETH-ZH in blue nodes, ZH as cities, and subfields as the common point.
# This means that the city and affiliations, subfield may have been effective in the clustering .I think that researchers tend to collaborate with the same universities and subfields 

# Second algorithm,let's try cluster_walktrap
cw <- cluster_walktrap(graph)
membership (cw)
table (membership(cw))
modularity (cw)
# I reached the modularity value of 0.75. This value is better than the previous one,
# but we can make a more accurate decision after drawing the network.

par(mar=c(0,0,0,0))
plot(cw, graph, vertex.cex = 0.6, vertex.size=degree(graph, mode="in")/3, 
     vertex.label.cex = 0.2, vertex.label.color= "black", edge.curved=T,  edge.arrow.size = 0.1)
# As a result of the cluster_edge_betweenness community detection algorithm, it confirms the importance of its sub-discipline.
# Researchers have a strong tendency to publish with other scientists from the same sub-discipline rather than different sub-disciplines.

# Moreover, I see that doctoral students often publish in their local community, like the same university.
# On the other hand, professors, tend to keep their collaborators out of Switzerland or the different disciplines.


#Let's try another algorithm and interpret the results

# Third algorithm,cluster_edge_betweenness
c_between<-cluster_edge_betweenness(graph_u)
membership (c_between)
modularity (c_between)
# I reached the modularity value of 0.68 which is reasonable and good rate.
table (membership(c_between))

par(mar=c(0,0,0,0))
plot(c_between, graph_u, vertex.cex = 0.5, vertex.size=igraph::degree(graph_u)+1, 
     vertex.label.cex = 0.3, vertex.label.color= "black", edge.curved=T,  edge.arrow.size = 0.5)

# It is possible to see similar situations in other community detection algorithms,
# and many authors with the same subfields and cities seem more likely to work together.
# We can even say that there are more collaborate when the universities are the same.
# So, I think all these 3 areas (subfields, affiliations, city) have an effect on clustering.