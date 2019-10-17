setwd("/Users/tian/R Lab/Lab8")


library(igraph)
library(devtools)
library(rgl)
library(markdown)
library(rmarkdown)

class.data<-read.csv(file.choose(),header=T, stringsAsFactors = F)
node.info<-read.csv(file.choose(),header=T, stringsAsFactors = F)

View(class.data)
colnames(class.data)<-gsub("\\.","",colnames(class.data))
class.data$X<-gsub(" ","",class.data$X)

M<-as.matrix(class.data[,-1])
rownames(M)<-class.data$X

M[is.na(M)]<-0
g<-graph_from_adjacency_matrix(M)
g<-simplify(g)
vcount(g) #nodes
ecount(g) #edges

plot.igraph(g)
par(mar=c(0,0,0,0))
plot.igraph(g)
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="IST719(Tues and Weds)")

par(mar=c(5,4,4,2))
plot(sort(degree(g))) #number of connections

par(mar=c(5,4,4,2),cex.axis=.5)

#most friends
barplot(sort(degree(g)),horiz = T,las=2,border = NA,
        col = "darkgreen",main = "most friends",space=1)
#most popular
barplot(sort(degree(g,mode = "in")),horiz = T,las=2,border = NA,
        col = "darkorange",main = "most popular",space=1)
#most friendly
barplot(sort(degree(g,mode = "out")),horiz = T,las=2,border = NA,
        col = "darkorchid",main = "most fiendly",space=1)

node.info$Name<-gsub(" ","",node.info$Name)
V(g)$day<-node.info$Class
V(g)$degree<-degree(g,mode = "in")
par(mar=c(0,0,0,0))
V(g)$color<-rgb(255,69,0,alpha = 180,maxColorValue = 255)
V(g)$color[V(g)$day=="Tuesday"]<-rgb(238,154,73,alpha = 180,maxColorValue = 255)
V(g)$color[V(g)$day=="Wednesday"]<-rgb(92,172,238,alpha = 180,maxColorValue = 255)
V(g)$frame.color=NA
V(g)$size<-4
plot.igraph(g,edge.arrow.size=0)


deg.size<-degree(g,mode="in")
plot(sort(deg.size),pch=16,type="l")


par(mfrow=c(2,4))

tmp<-sort(deg.size)
plot(tmp,pch=16,type = "l")
plot(tmp/max(tmp),pch=16,type = "l")
plot(log10(tmp),pch=16,type = "l")
plot(tmp^2,pch=16,type = "l")
plot(sqrt(tmp),pch=16,type = "l")
plot(tmp^1/5,pch=16,type = "l")
plot(tmp^1/10,pch=16,type = "l")

par(mfrow=c(1,1),mar=c(0,0,0,0))
tmp<-sqrt(V(g)$degree)
V(g)$size<-1+(20*tmp/max(tmp))

plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0)

V(g)$label.color<-"black"
V(g)$deg.out<-degree(g,mode = "out")
V(g)$label.cex<-.25+(V(g)$deg.out/max(V(g)$deg.out))
gl<-delete_vertices(g,"JeffHemsley")
plot.igraph(gl,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")


E(g)$color<-"gold"
tmp.name<-V(g)$name[which.max(V(g)$deg.out)]
E(g)[from(tmp.name)]$color<-"blue"
E(g)[to("JeffHemsley")]$color<-"red"
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")

l<-layout_on_grid(g)
V(g)$x<-l[,1]
V(g)$y<--l[,2]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")

l<-layout_with_kk(g)
V(g)$x<-l[,1]
V(g)$y<-l[,2]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")

l<-layout_with_gem(g)
V(g)$x<-l[,1]
V(g)$y<-l[,2]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")



l<-layout_as_star(g,center="JeffHemsley")
V(g)$x<-l[,1]
V(g)$y<-l[,2]
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0,main="Tues & Weds Info Viz")

l<-layout


coords<-layout_with_kk(g,dim=3)
rglplot(g,layout=coords)



rgl.snapshot(filename = "IST719-3Dnetwork.png")


writeWebGL(dir = ".\\web\\",width=500,reuse=T)

tmp<-data.frame(from=0:(360*6))
tmp$to<-tmp$from+1
fname.list<-paste0(".//movie//","network_",tmp[,1],"_",tmp[,2],".png")
V(g)$label<-""
rglplot(g,layout=coords)
rgl.bringtotop()

par3d(windowRect=c(100,100,640,640))
rgl.bg(color = "black")
rgl.viewpoint(0,20)

max.loops<-360*6
for(i in 1:max.loops){
  rgl.viewpoint(i,20,zoom = 1.2-i/max.loops)
  snapshot.fname<-fname.list[i]
  rgl.snapshot(snapshot.fname)
}

setwd("h:\\IST719\\Week8\\movie")

dir<-paste0(getwd(),"\\movie")
library(animation)

ani.options(interval=.1)
imgs<-list.files(dir,pattern="*.png")
saveGIF({
  for (img in imgs){
    im<-magick::image_read(paste0(dir,"\\",img))
    plot(as.raster(im))
  }
}, movie.name=paste0(getwd(),"\\ClassNetwork.gif"))







