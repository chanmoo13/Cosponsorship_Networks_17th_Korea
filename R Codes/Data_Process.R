#################################################################################
########################    Cosponsorship Networks in    ########################
########################  the 17th National Assembly of  ########################
########################        Republic of Korea        ########################
#################################################################################


#### Chanmoo Park,  Woncheol Jang
#### Department of Statistics,
#### Seoul National University, KOREA
#### Any issues for codes : "chanmoo13@snu.ac.kr" or "chanmoopark13@gmail.com" (Chanmoo Park)



########## Data Process (constructing networks from raw data) ##########



library("igraph")
library("network")
library("ergm.count")



###### Data Loading ######


raw.data<-read.csv("Data/17th_Assembly.csv",header=TRUE)       ## Legislation information
party.m<-read.csv("Data/17th_Party.csv")                       ## Party information
nelec<-read.csv("Data/17th_nelec.csv", header=TRUE)            ## Number of Election information



###### Pre-Processing ######


cospon.m<-raw.data[,-c(1:3,5:13)]                              ## Cosponsorship matrix
names(cospon.m)<-as.character(c("represent",c(1:322)))
covariate<-raw.data[,c(1:3,5:13)]                              ## Covariate matrix

party<-unlist(lapply(c(1:(ncol(party.m)-1)), function(t)
  names(which(table(party.m[,(t+1)])==max(table(party.m[,(t+1)])))) ))
party.table<-table(party)

party.4<-party                                                 ## Aggregated Party vector (4 parties)
party.4[party.4!="1" & party.4!="2" & party.4!="3"] = "D"
party.4[party.4=="1"] = "A"
party.4[party.4=="2"] = "B"
party.4[party.4=="3"] = "C"

first<-which(covariate$session<=251)                           ## Period index vector
second<-which(covariate$session<=257 & covariate$session>251)
third<-which(covariate$session<=264 & covariate$session>257)
fourth<-which(covariate$session<=274 & covariate$session>264)

nelec<-nelec$n_elec                                            ## Number of Election vector



###### Constructing Network ######


cotoadj<-function(co.m){                                       ## "cotoadj" Function ; From Cosponsorship matrix To Adjacency matrix
  edge<-c(1,1)
  for(i in 1:nrow(co.m)){
    temp<-which(co.m[i,-1]==1)
    temp<-temp[-which(which(co.m[i,-1]==1)==co.m[i,1])]
    te<-cbind(temp,rep(co.m[i,1],length(temp)))
    edge<-rbind(edge,te)
  }
  edge<-edge[-1,]
  count<-rep(1,nrow(edge))   
  edge<-cbind(edge,count)
  vertex<-sort(unique(c(edge[,1],edge[,2])))
  graph<-graph.data.frame(edge, directed=TRUE,vertices=vertex)
  sum.graph<-simplify(graph, edge.attr.comb = list(count="sum"))
  adj<-as_adjacency_matrix(sum.graph, attr = "count",sparse=FALSE)
  return(adj)
}

adj.tot <- cotoadj(cospon.m)                                   ## Adjacency Matrix of Network ; Whole(total), period1, period2, period3, period4
adj.1 <- cotoadj(cospon.m[first,])
adj.2 <- cotoadj(cospon.m[second,])
adj.3 <- cotoadj(cospon.m[third,])
adj.4 <- cotoadj(cospon.m[fourth,])

                                                               ## Network Object ; Whole(total), period1, period2, period3, period4
net.tot <- as.network(adj.tot, directed = TRUE, matrix.type = "a",
                      ignore.eval = FALSE, names.eval = "count")
ind<-as.numeric(net.tot %v% "vertex.names")
net.tot %v% "party" <- party.4[ind]
net.tot %v% "nelec" <- nelec[ind]

net.1 <- as.network(adj.1, directed = TRUE, matrix.type = "a",
                    ignore.eval = FALSE, names.eval = "count")
ind<-as.numeric(net.1 %v% "vertex.names")
net.1 %v% "party" <- party.4[ind]
net.1 %v% "nelec" <- nelec[ind]

net.2 <- as.network(adj.2, directed = TRUE, matrix.type = "a",
                    ignore.eval = FALSE, names.eval = "count")
ind<-as.numeric(net.2 %v% "vertex.names")
net.2 %v% "party" <- party.4[ind]
net.2 %v% "nelec" <- nelec[ind]

net.3 <- as.network(adj.3, directed = TRUE, matrix.type = "a",
                    ignore.eval = FALSE, names.eval = "count")
ind<-as.numeric(net.3 %v% "vertex.names")
net.3 %v% "party" <- party.4[ind]
net.3 %v% "nelec" <- nelec[ind]

net.4 <- as.network(adj.4, directed = TRUE, matrix.type = "a",
                    ignore.eval = FALSE, names.eval = "count")
ind<-as.numeric(net.4 %v% "vertex.names")
net.4 %v% "party" <- party.4[ind]
net.4 %v% "nelec" <- nelec[ind]



###### Saving Workspace ######


save.image(file = "Data/constructedNetwork.RData")



###### Data Exports for Visualization (Gephi) ######

adjtoelist <- function(adj.m, vnames){
  if(nrow(adj.m)!=length(vnames)){
    cat("ERROR")
    break;
  }
  n<-length(vnames)
  elist<-c(1,1,1)
  for(i in 1:n){
    for(j in 1:n){
      if(adj.m[i,j]!=0) {
        elist<-rbind(elist,c(vnames[i],vnames[j],adj.m[i,j]))
      }
    }
  }
  elist<-elist[-1,]
  return(elist)
}


id2names<-read.table("Data/id2names.txt", header = TRUE)
vertexlist.1<-data.frame(ID = ID <- net.1 %v% "vertex.names", 
                         Label = Label <- id2names[net.1 %v% "vertex.names",2], 
                         party = party <- net.1 %v% "party")               


edgelist.1<-adjtoelist(adj.1, as.numeric(net.1%v%"vertex.names"))
colnames(edgelist.1)<-c("Source", "Target","Weight")


write.table(vertexlist.1,file="Data/Vertex_17_1.csv",sep=",", row.names = FALSE)  ### Exporting vertices
write.table(edgelist.1,file="Data/Edge_17_1.csv",sep=",", row.names = FALSE)      ### Exporting edges


