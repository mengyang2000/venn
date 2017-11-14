### Rscript venn.R --args input output
options(stringsAsFactors=F) 
args<-commandArgs(TRUE)
input=args[2]
out=args[3]
t=read.table(input,sep="\t",header=T,check.names=F)
xx=c()
for(i in 2:5){
	        if(sum(t[,i])==0) xx=c(xx,i)
}
sum(t[,2]);sum(t[,3]);sum(t[,4]);sum(t[,5]);
if(is.null(xx)) t=t else t=t[,-xx]
z=list()
name=colnames(t)[-1]
Name=c("group1","group2","group3","group4")
if(!is.na(match(Name[1],name))) z$group1=t[,1][with(t,group1)==1]
if(!is.na(match(Name[2],name))) z$group2=t[,1][with(t,group2)==1]
if(!is.na(match(Name[3],name))) z$group3_Seq=t[,1][with(t,group3)==1]
if(!is.na(match(Name[4],name))) z$group4=t[,1][with(t,group4)==1]
print(length(z))

if(length(z)==2){
a = colnames(t)[2]
b = colnames(t)[3]
aa <-  (subset(t, t[,a]>=1))[,1]
bb <-  (subset(t, t[,b]>=1))[,1]
pdf(out, width=10, height=10)
library(VennDiagram,lib.loc="/home/meng_yang/R/x86_64-pc-linux-gnu-library/3.3")
draw.pairwise.venn(area1=length(aa), area2=length(bb), cross.area=length(intersect(aa,bb)),scaled = FALSE,category =c(a,b),lwd=1.5,col=c("white","white"),fill=c(rgb(22/255,134/255,198/255),rgb(220/255,104/255,151/255)),alpha = 0.50,cex=2.2,cat.cex=2.2,fontfamily = rep("sans",3),cat.fontfamily = rep("sans", 2),label.col=c(rgb(22/255,134/255,198/255),"white",rgb(220/255,104/255,151/255)),cat.col =c(rgb(22/255,134/255,198/255),rgb(220/255,104/255,151/255)),cat.pos=c(0,0))
dev.off()
}
if(length(z)==3){
a = colnames(t)[2]
b = colnames(t)[3]
c = colnames(t)[4]
aa <- (subset(t, t[,a]>=1))[,1]
bb <- (subset(t, t[,b]>=1))[,1]
cc <- (subset(t, t[,c]>=1))[,1]
pdf(out, width=10, height=10)
library(VennDiagram,lib.loc="/home/meng_yang/R/x86_64-pc-linux-gnu-library/3.3")
draw.triple.venn(area1=length(aa),area2=length(bb),area3=length(cc),n12=length(intersect(aa,bb)),n13=length(intersect(aa,cc)),n23=length(intersect(bb,cc)),n123=length(intersect(intersect(aa,bb),cc)),category = c(a,b,c),col=rep("white",3),fill=c(rgb(22/255,134/255,198/255),rgb(220/255,104/255,151/255),rgb(243/255,152/255,28/255)),cat.col=c(rgb(22/255,134/255,198/255),rgb(220/255,104/255,151/255),rgb(243/255,152/255,28/255)),reverse = FALSE,alpha = 0.50,cex=2,cat.cex=2,fontfamily = rep("sans",7),cat.fontfamily = rep("sans", 3),label.col=c(rgb(22/255,134/255,198/255),"white",rgb(220/255,104/255,151/255),"white","white","white",rgb(243/255,152/255,28/255)))

dev.off()

}

if(length(z)==4){
library(VennDiagram,lib.loc="/home/meng_yang/R/x86_64-pc-linux-gnu-library/3.3")
p = venn.diagram(z,filename=NULL,height=1200,width=1200,col = "white",lwd=0.5,fill = c(rgb(243/255,152/255,28/255),rgb(220/255,104/255,151/255),rgb(53/255,166/255,56/255),rgb(22/255,134/255,198/255)),alpha = 0.50,label.col = c(rgb(53/255,166/255,56/255), "white", rgb(22/255,134/255,198/255), "white", "white", "white", "white", "white", rgb(243/255,152/255,28/255), "white", "white", "white", "white", rgb(220/255,104/255,151/255), "white"),cat.col = c(rgb(243/255,152/255,28/255),rgb(220/255,104/255,151/255),rgb(53/255,166/255,56/255),rgb(22/255,134/255,198/255)),cat.cex=1.2,cex=1.4,cat.fontfamily=rep("sans",4),fontfamily=rep("sans",15))

pdf(out)
grid.draw(p)

}
