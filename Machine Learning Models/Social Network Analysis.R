install.packages("maps")
install.packages("igraph")
library(dplyr)
library(maps)
library(igraph)
library(tidyverse)

# Load data
senators <- read.csv("senators.csv")
senatorLinks <- read.csv("senateCosponsorship.csv", stringsAsFactors=FALSE)
view(senators)
G <- graph.data.frame(senatorLinks, directed=FALSE, senators)
# add Senators' names to edges
G = set_edge_attr(G, "name1", index = E(G), senators$name[(senatorLinks$V1)])
G = set_edge_attr(G, "name2", index = E(G), senators$name[(senatorLinks$V2)])
# add Senators' party to edges
G = set_edge_attr(G, "party1", index = E(G), senators$party[(senatorLinks$V1)])
G = set_edge_attr(G, "party2", index = E(G), senators$party[(senatorLinks$V2)])

# Extract connected sub-graph
comp = components(G)
in.max.comp = comp$membership == which.max(comp$csize)
sg = induced_subgraph(G, in.max.comp)
sg.Senators = senators[in.max.comp,]

# Basic plot
plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y), # fix the upper/lower bounds of the plot
     edge.lty = ifelse(E(sg)$n>=50, "solid","blank"), # solid lines for edges with more than 'threshold' cosponsorships
     vertex.color = ifelse(V(sg)$party == "R", "red", ifelse(V(sg)$party == "D","blue", ifelse(V(sg)$party == "I", "yellow", "grey")))
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states\

# Independent Senators
I.senators = senators$name[senators$party=="I"]

### PLOT WITH SENATORS MODIFICATION
plot(sg, 
     vertex.size = 50, # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y), # fix the upper/lower bounds of the plot
     # solid lines for edges that touch an "Independent" vertex, other edges are blank
     edge.lty = ifelse("I"==E(sg)$party1|"I"==E(sg)$party2, "solid", "blank"),
     # orange lines for the first senator, purple for the other edges
     edge.color = ifelse(E(sg)$name1==I.senators[1]|E(sg)$name2==I.senators[1], "orange", "purple" ),
     # edge width proportional to n
     edge.width = 0.1*E(sg)$n,
     vertex.color = ifelse(V(sg)$party == "R", "red", ifelse(V(sg)$party == "D","blue", ifelse(V(sg)$party == "I", "yellow", "grey")))
     )
maps::map("state",  add=TRUE, col="black") # add a map of the US states\

#### DEGREE CENTRALITY
#CENTRALITY - HOW MANY NEIGHBOURS A NODE HAS
degree.centrality = centr_degree(sg,mode='all')$res
sg.Senators %>% mutate(degree.centrality) %>% arrange(desc(degree.centrality)) %>% head(n=10) -> top_10_degree

#CLOSENESS - SUM OF THE SHORTEST PATHS TO EVERY OTHER NODE (RECIPROCAL - 1/X) - LARGE NUMBERS ARE CLOSE TO THE CENTER
# PEOPLE CAN EASILY CAN TO EVERYONE
clo.centrality = centr_clo(sg,mode='all')$res
sg.Senators %>% mutate(clo.centrality) %>% arrange(desc(clo.centrality)) %>% head(n=10) -> top_10_clo

#BETWEENNESS - NUMBER OF SHORTEST PATHS IN WHICH A SPECIFIC NODE APPEARS (HOW MANY MOST EFFICIENT WAYS GO THROUGH ME?
betw.centrality = centr_betw(sg,directed=FALSE)$res
sg.Senators %>% mutate(betw.centrality) %>% arrange(desc(betw.centrality)) %>% head(n=10) -> top_10_betw

# PLOT WITH CENTRALITY
plot(sg, 
    # you can modify this so that the points are visible
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     edge.curved = TRUE, # make the edges curved
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y), # fix the upper/lower bounds of the plot
     edge.lty = ifelse(E(sg)$n>=30, "solid","blank"), # solid lines for edges with more than 'threshold' cosponsorships
    vertex.color = ifelse(V(sg)$party == "R", "red", ifelse(V(sg)$party == "D","blue", ifelse(V(sg)$party == "I", "yellow", "grey"))),
      # INCLUDE THE VERTEX CENTRALIC METRIC HERE BELOW - 2X TO ADJUST PROPORTION
     vertex.size = clo.centrality*300,
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states\

top_10_degree
top_10_clo
top_10_betw

### quotient of betweenness centrality divided by degree centrality 
centr.ratio = betw.centrality/degree.centrality
sg.Senators %>% mutate(centr.ratio) %>% arrange(desc(centr.ratio)) %>% head(n=10) -> top_10_ratio
top_10_ratio

### ADDING COMMUNITIES
set.seed(173)
spinglass = cluster_spinglass(sg,spins=100,weights=E(sg)$n)
spinglass$csize
spinglass$modularity

# add communities to dataframe
sg.Senators$clust = spinglass$membership
# add communities to edges
sg = set_edge_attr(sg, "c1", index=E(sg), sg.Senators[paste(senatorLinks$V1),"clust"])
sg = set_edge_attr(sg, "c2", index=E(sg), sg.Senators[paste(senatorLinks$V2),"clust"])
# colors for plots:
clrs = c("darkgreen","orange","lightblue", "purple","magenta")

### MODIFIED PLOT
# assign vertex shapes by party
vertex.shape=recode(V(sg)$party,"R"="square","D"="circle", "I"="rectangle")
# assign vertex color by community
vertex.color=clrs[sg.Senators$clust]
# only show edges within the same community
edge.lty = ifelse(E(sg)$c1==E(sg)$c2,"solid","blank")
# edge color by community
edge.color = clrs[E(sg)$c1]

plot(sg, 
     vertex.label=NA, # don't plot the Senator's names next to each vertex
     vertex.color=vertex.color,
     edge.curved = TRUE, # make the edges curved
     edge.lty = edge.lty,
     vertex.size = 50,
     edge.color = edge.color,
     vertex.shape=vertex.shape,
     layout = cbind(V(sg)$x,V(sg)$y), # plot each Senator in their state
     rescale=FALSE, # fix the plot axes to the co-ordinate scale
     xlim = range(senators$x), ylim = range(senators$y) # fix the upper/lower bounds of the plot
)
maps::map("state",  add=TRUE, col="black") # add a map of the US states


### TABLE
pivot_party <- table(sg.Senators$party, sg.Senators$clust)
pivot_party
### PERCENTUAL OF SENATORS
dem_1 <- pivot_party[1,1]/sum(pivot_party[,1])
rep_1 <- pivot_party[3,1]/sum(pivot_party[,1])
dem_2 <- pivot_party[1,2]/sum(pivot_party[,2])
rep_2 <- pivot_party[3,2]/sum(pivot_party[,2])
dem_3 <- pivot_party[1,3]/sum(pivot_party[,3])
rep_3 <- pivot_party[3,3]/sum(pivot_party[,3])
dem_4 <- pivot_party[1,4]/sum(pivot_party[,4])
rep_4 <- pivot_party[3,4]/sum(pivot_party[,4])
dem_5 <- pivot_party[1,5]/sum(pivot_party[,5])
rep_5 <- pivot_party[3,5]/sum(pivot_party[,5])

pivot_state <- table(sg.Senators$state, sg.Senators$clust)
pivot_state

table(sg.Senators$name, sg.Senators$clust)






