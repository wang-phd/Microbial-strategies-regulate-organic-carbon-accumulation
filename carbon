setwd("E:/1/R/细菌0508/物种-class、family、genus")
BF <- read.delim("Class4_2.txt",sep='\t',row.names=1)
#BF<-BF/colSums(BF)
BF<-t(BF)
write.table(BF,file='Class4_3.txt',sep = '\t')
f<-read.delim('Class4_3.txt',row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
f$sum<-rowSums(f)
f<-f[order(f$sum,decreasing = TRUE),]
f_top15<-f[1:15, -ncol(f)]
f_top15<-f_top15 [,1:96]*100
f_top15['Others', ] <- 100 - colSums(f_top15)
write.csv(f_top15, 'Class4_top15.csv', quote = FALSE)
library(ggplot2)
nmds<-read.table("bac_Class4_top15DEPTH.txt",header=T,check.names=FALSE, sep="\t")
nmds$Phylum = factor(nmds$Phylum, levels=c('Others','Saccharimonadia','Verrucomicrobiae','Acidimicrobiia','Gemmatimonadetes','Blastocatellia','Bacilli','Planctomycetes','Thermoleophilia','Bacteroidia','Methylomirabilia','Vicinamibacteria','Anaerolineae','Alphaproteobacteria','Actinobacteria','Gammaproteobacteria'))
nmds$Treatment <- factor(nmds$Treatment,levels = c("D1","D2","D3","D4"))
p=ggplot(nmds,aes(x=Treatment,y=RA ,fill=Phylum))+
  geom_bar(stat="identity",width = .5,position="stack",colour = "transparent")+#facet_wrap(.~Leaf,nrow=1)+
  scale_fill_manual(values= c('#6E6E6E',"#B1A4C0",'#FF83FA','#FFB5C5','#8B4513','#5E4FA2','#4876FF',"#7CB6DB","#479E9B",'#ABDDA4','#E6F598','#FEE08B','#FDAE61','#F46D43','#D53E4F','#9E0142'))
p1<-p+theme_bw()+ylab("Bacteria dominant phyla Relative abundance(%)")+ theme(axis.text.x = element_text(angle = 0, hjust = 1))
p1
ggsave(paste("Bac_CLASS15.DEPTH.pdf", sep=""), p1, width =6, height =4.5)
