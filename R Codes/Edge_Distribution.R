#################################################################################
########################    Cosponsorship Networks in    ########################
########################  the 17th National Assembly of  ########################
########################        Republic of Korea        ########################
#################################################################################


#### Chanmoo Park,  Woncheol Jang
#### Department of Statistics,
#### Seoul National University, KOREA
#### Any issues for codes : "chanmoo13@snu.ac.kr" or "chanmoopark13@gmail.com" (Chanmoo Park)



########## Edge value distribution of Cosponsorship Networks ##########



load("Data/constructedNetwork.RData")
library("igraph")
library("network")
library("ergm.count")
library("Cairo")
library("gridExtra")
library("ggplot2")



### Whole Period


count.tot <- net.tot %e% "count"  ### Count distribution
zero.tot <- sum(adj.tot==0)-length(net.tot %v% "vertex.names")
count.tot0<-c(count.tot, rep(0,zero.tot))
m <- ggplot(as.data.frame(count.tot0), aes(x=as.data.frame(count.tot0)))
mt <- m + geom_histogram(bins = 17, aes(fill = ..count..)) + xlim(-1, 15) + ylim(0, 90000) + scale_fill_gradient("Count", low = "#FFAA00", high = "red") + 
  labs(title="Edge Distibution of Whole Network", x = "Edge Value", y = "Count") + 
  theme(plot.title = element_text(  size=20)) +
  theme(axis.title.x = element_text(  size = 15)) + 
  theme(axis.title.y = element_text(  size = 15)) + 
  theme(axis.text.x = element_text(  size = 10)) +
  theme(axis.text.y = element_text(  size = 10)) +
  annotate("text", x=10, y=70000, label= paste("mean : ", round(mean(c(count.tot, rep(0,zero.tot))),4)), size = 6) + 
  annotate("text", x=10, y=67000, label= paste("var : ", round(var(c(count.tot, rep(0,zero.tot))),4)), size = 6) + 
  annotate("text", x=10, y=64000, label= paste("max : ", round(max(c(count.tot, rep(0,zero.tot))),4)), size = 6)

cairo_pdf(filename = "Figures/whole_edge_dist.pdf")
mt
dev.off()



### First Period


count.1 <- net.1 %e% "count"  ### Count distribution
zero.1 <- sum(adj.1==0)-length(net.1 %v% "vertex.names")

count.10<-c(count.1, rep(0,zero.1))
m <- ggplot(as.data.frame(count.10), aes(x=as.data.frame(count.10)))
m1 <- m + geom_histogram(bins = 17, aes(fill = ..count..)) + xlim(-1, 15) + ylim(0, 90000) + scale_fill_gradient("Count", low = "#FFAA00", high = "red") +
  labs(title="Edge Distibution of 1st Period Network", x = "Edge Value", y = "Count") + 
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) + 
  annotate("text", x=10, y=70000, label= paste("mean : ", round(mean(c(count.1, rep(0,zero.1))),4)), size = 5) + 
  annotate("text", x=10, y=62500, label= paste("var : ", round(var(c(count.1, rep(0,zero.1))),4)), size = 5) + 
  annotate("text", x=10, y=55000, label= paste("max : ", round(max(c(count.1, rep(0,zero.1))),4)), size = 5)



### Second Period


count.2 <- net.2 %e% "count"  ### Count distribution
zero.2 <- sum(adj.2==0)-length(net.2 %v% "vertex.names")

count.20<-c(count.2, rep(0,zero.2))
m <- ggplot(as.data.frame(count.20), aes(x=as.data.frame(count.20)))
m2 <- m + geom_histogram(bins = 17, aes(fill = ..count..)) + xlim(-1, 15) + ylim(0, 90000) + scale_fill_gradient("Count", low = "#FFAA00", high = "red") +
  labs(title="Edge Distibution of 2nd Period Network", x = "Edge Value", y = "Count") + 
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) + 
  annotate("text", x=10, y=70000, label= paste("mean : ", round(mean(c(count.2, rep(0,zero.2))),4)), size = 5) + 
  annotate("text", x=10, y=62500, label= paste("var : ", round(var(c(count.2, rep(0,zero.2))),4)), size = 5) + 
  annotate("text", x=10, y=55000, label= paste("max : ", round(max(c(count.2, rep(0,zero.2))),4)), size = 5)



### Third Period


count.3 <- net.3 %e% "count"  ### Count distribution
zero.3 <- sum(adj.3==0)-length(net.3 %v% "vertex.names")

count.30<-c(count.3, rep(0,zero.3))
m <- ggplot(as.data.frame(count.30), aes(x=as.data.frame(count.30)))
m3 <- m + geom_histogram(bins = 17, aes(fill = ..count..)) + xlim(-1, 15) + ylim(0, 90000) + scale_fill_gradient("Count", low = "#FFAA00", high = "red") +
  labs(title="Edge Distibution of 3rd Period Network", x = "Edge Value", y = "Count") + 
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) + 
  annotate("text", x=10, y=70000, label= paste("mean : ", round(mean(c(count.3, rep(0,zero.3))),4)), size = 5) + 
  annotate("text", x=10, y=62500, label= paste("var : ", round(var(c(count.3, rep(0,zero.3))),4)), size = 5) + 
  annotate("text", x=10, y=55000, label= paste("max : ", round(max(c(count.3, rep(0,zero.3))),4)), size = 5)



### Fourth Period


count.4 <- net.4 %e% "count"  ### Count distribution
zero.4 <- sum(adj.4==0)-length(net.4 %v% "vertex.names")

count.40<-c(count.4, rep(0,zero.4))
m <- ggplot(as.data.frame(count.40), aes(x=as.data.frame(count.40)))
m4 <- m + geom_histogram(bins = 17, aes(fill = ..count..)) + xlim(-1, 15) + ylim(0, 90000) + scale_fill_gradient("Count", low = "#FFAA00", high = "red") +
  labs(title="Edge Distibution of 4th Period Network", x = "Edge Value", y = "Count") + 
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) + 
  annotate("text", x=10, y=70000, label= paste("mean : ", round(mean(c(count.4, rep(0,zero.4))),4)), size = 5) + 
  annotate("text", x=10, y=62500, label= paste("var : ", round(var(c(count.4, rep(0,zero.4))),4)), size = 5) + 
  annotate("text", x=10, y=55000, label= paste("max : ", round(max(c(count.4, rep(0,zero.4))),4)), size = 5)


cairo_pdf(filename = "Figures/period_1to4_edge_dist.pdf", width = 11.69, height = 8.27)
grid.arrange(m1, m2, m3, m4, ncol = 2)
dev.off()