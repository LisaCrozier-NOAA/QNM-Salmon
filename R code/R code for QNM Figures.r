
#FIGURES----------------------------      
source("R code upload for QNM.r")
#or, load output files


#Fig S2.1 Outcomes from H1-H6, Base and Mammal, for all nodes----------
#Base network
windows(width = 6, height = 8)
#      png(file = "Final Figures\\Meanimpacts.C10.base.png", width = 10, height = 12, units = "in", res = 800)
par(mfrow=c(2,3),mar=c(3,0,3,2),oma=c(5,10,2,2))
a=1.1
for (i in 1:length(Hlist)){
     H<-get(paste0("H",i,".sim.base"))
     x<-sign.determinacy(H);x$surv.SprCh  
     meanspr<-(x$surv.SprCh["pos"]+ -1*x$surv.SprCh["neg"])/ H$accepted;meanspr
     meanfall<-(x$surv.FallCh["pos"]+ -1*x$surv.FallCh["neg"])/ H$accepted;meanfall
     if(i %in% c(1,4,7))           mybarplot.rev(survsum=(x$mean.impacts[1:20]),scenario=paste0("H",i,": ",Hlist[i]),cex.axis = a,whichpress=1);abline(h=21.7)
     if(i %in% c(2,3,5,6,8,9))    nonames.mybarplot.rev(survsum=x$mean.impacts[1:20],scenario=paste0("H",i,": ",Hlist[i]),whichpress=1,cex.axis = a);abline(h=21.7)
     abline(h=c(2.5,6,9.7,14.5,18.2),lty=2)    
} #end dataframe creation and barplots     
mtext(side=3,outer=T,"Mean impacts in base food web with a1.Clim1 press")
mtext(side=1,outer=T,"Mean outcome")
#      dev.off()

#Mammal network
windows(width = 6, height = 8)
#      png(file = "Final Figures\\Meanimpacts.C10.mammal.png", width = 10, height = 12, units = "in", res = 800)
par(mfrow=c(2,3),mar=c(3,0,3,2),oma=c(5,10,2,2))
a=1.1
for (i in 1:length(Hlist)){
     H<-get(paste0("H",i,".sim.mammal"))
     x<-sign.determinacy(H);x$surv.SprCh  
     meanspr<-(x$surv.SprCh["pos"]+ -1*x$surv.SprCh["neg"])/10000;meanspr
     meanfall<-(x$surv.FallCh["pos"]+ -1*x$surv.FallCh["neg"])/10000;meanfall
     if(i %in% c(1,4,7))           mybarplot.rev(survsum=(x$mean.impacts[1:20]),scenario=paste0("H",i,": ",Hlist[i]),cex.axis = a,whichpress=1);abline(h=21.7)
     if(i %in% c(2,3,5,6,8,9))    nonames.mybarplot.rev(survsum=x$mean.impacts[1:20],scenario=paste0("H",i,": ",Hlist[i]),whichpress=1,cex.axis = a);abline(h=21.7)
     abline(h=c(2.5,6,9.7,14.5,18.2),lty=2)    
     
} #end dataframe creation and barplots     
mtext(side=3,outer=T,"Mean impacts in mammal food web with a1.Clim1 press")
mtext(side=1,outer=T,"Mean outcome")
#      dev.off()



#Fig 4. XY graph of spring and fall outcomes for all H -----------
 allXP<-read.csv(file="R code/outputs/allXP.csv",row.names = NULL)     
myplot <- ggplot(allXP[allXP$Model %in% c("Base","Mammals"),], aes(x= SprCh, y = FallCh,col=Hdescription2)) + 
     geom_point( size = 3) +  xlab("Spring Chinook Adults") + ylab("Fall Chinook Adults") +
     geom_smooth(aes( y=FallCh), method="lm", formula = y~x, se = FALSE) +
     scale_x_continuous(breaks=c(-1,-0.6,-0.2,-0.6,0.2,0.6,1),limits=c(-1,1))+
     scale_y_continuous(breaks=c(-1,-0.6,-0.2,-0.6,0.2,0.6,1),limits=c(-1,1))+
     geom_hline(yintercept=0,col="darkgrey",lty=2) +     geom_vline(xintercept=0,col="darkgrey",lty=2) + 
     annotate("rect", xmin = -0.2, xmax = 0.2, ymin = -0.2, ymax = 0.2, alpha = .7,fill = "grey")+
     geom_text_repel(aes(label = H),box.padding   = 0.35,point.padding = 0.5,segment.color = 'grey50',show.legend = F) +
     facet_wrap(~Model) + scale_color_discrete(name = "Temperature\n hypothesis")  +
     theme_classic(base_size = 16) +     theme(plot.title = element_text(hjust = 0.5)) 

myplot

#This puts a grey rectangle between them, from https://stackoverflow.com/questions/42848826/add-a-coloured-border-between-facets-in-ggplot2
gt <- ggplotGrob(myplot)
panels = subset(gt$layout, grepl("panel", gt$layout$name), t:r)
# The span of the vertical gap
Bmin = min(panels$t) - 1
Bmax = max(panels$t)
# The columns of the gaps (two to the right of the panels
cols = unique(panels$r)[-length(unique(panels$r))] + 2
# The grob - grey rectangle
g = rectGrob(gp = gpar(col = NA, fill = "grey40"))
## Add grey rectangles into the vertical gaps
gt <- gtable_add_grob(gt,rep(list(g), length(cols)),t=Bmin, l=cols, b=Bmax)

windows(width = 8, height = 4)
grid.newpage()
grid.draw(gt)           

library(cowplot)
library("magick")
library(png)

#Fig 5.  Boxplots by Food Web (top) and Temperature (bottom) -----------
bird = readPNG("C:/Users/Lisa.Crozier/Documents/Marine survival/QNM/Figures/Su/murreleft.png")
sealion = readPNG("C:/Users/Lisa.Crozier/Documents/Marine survival/QNM/Figures/Su/sea lion.PNG")
# allXP<-read.csv(file="Final Outputs/allXP.csv",row.names = NULL)     

#Top boxplot by foodweb
dat<-allXP
myshift=0.1
sprbox.FW<- ggplot(dat, aes(Model, SprCh)) +
     labs(y= "Chinook Outcome", x = " ", title="Spring Chinook") +
     ylim(c(-1,1)) +     
     geom_boxplot(fill="gray")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.20, ymax = 0.20,
              alpha = .4,fill = "grey") +
     annotate("rect", xmin = -0.20, xmax = 7, ymin = 0.20, ymax = 0.60,
              alpha = .4,fill = "green")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.60, ymax = -0.20,
              alpha = .4,fill = "yellow")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -1, ymax = -0.60,
              alpha = .4,fill = "red")+
     annotate(
          "text", label = "H1",
          x = 4, y = -0.5, size = 4, colour = "black" ) +
     theme_classic(base_size = 15) +
     theme(axis.text.x = element_text(vjust=1,hjust = 1,angle=45),
           plot.title = element_text(vjust=-15,hjust = 0.5)) 



fallbox.FW<- ggplot(dat, aes(Model, FallCh)) +
     labs(y= "Chinook Outcome", x = " ", title="Fall Chinook") +
     ylim(c(-1,1)) +     
     geom_boxplot(fill="gray")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.20, ymax = 0.20,
              alpha = .4,fill = "grey") +
     annotate("rect", xmin = -0.20, xmax = 7, ymin = 0.20, ymax = 0.60,
              alpha = .4,fill = "green")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.60, ymax = -0.20,
              alpha = .4,fill = "yellow")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -1, ymax = -0.60,
              alpha = .4,fill = "red")+
     annotate(
          "text", label = "H1",
          x = c(1,2,4,5, 6), y = c(0.59,0.49,0.57,0.47,0.37), size = 4, colour = "black") +
     theme_classic(base_size = 15) +
     theme(axis.text.x = element_text(vjust=1,hjust = 1,angle=45),
           plot.title = element_text(vjust=-15,hjust = 0.5)) 

#Create the x-axis label 
bottom.FW <- textGrob("Food web", gp = gpar(fontsize = 20))
#Plot
gridExtra::grid.arrange(sprbox.FW,fallbox.FW,nrow=1,ncol=2,bottom=bottom.FW)
#Save plot
FoodWeb.boxplots<- gridExtra::grid.arrange(sprbox.FW,fallbox.FW,nrow=1,ncol=2,bottom=bottom.FW)



#Bottom plot: "Temperature hypothesis" 
sprbox<- ggplot(dat, aes(Hdescription2, SprCh)) +
     labs(y= "Chinook Outcome", x = "  ", title="Spring Chinook") +
     ylim(c(-1,1)) +     
     geom_boxplot(fill="gray")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.20, ymax = 0.20,
              alpha = .4,fill = "grey") +
     annotate("rect", xmin = -0.20, xmax = 7, ymin = 0.20, ymax = 0.60,
              alpha = .4,fill = "green")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.60, ymax = -0.20,
              alpha = .4,fill = "yellow")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -1, ymax = -0.60,
              alpha = .4,fill = "red")+
     theme_classic(base_size = 15) +
     theme(axis.text.x = element_text(vjust=1,hjust = 1,angle=45),
           plot.title = element_text(vjust=-15,hjust = 0.5)) 



fallbox<- ggplot(dat, aes(Hdescription2, FallCh)) +
     labs(y= "Chinook Outcome", x = " ", title="Fall Chinook") +
     ylim(c(-1,1))+     
     geom_boxplot(fill="gray")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.20, ymax = 0.20,
              alpha = .5,fill = "grey")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = 0.20, ymax = 0.60,
              alpha = .4,fill = "green")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -0.60, ymax = -0.20,
              alpha = .4,fill = "yellow")+
     annotate("rect", xmin = -0.20, xmax = 7, ymin = -1, ymax = -0.60,
              alpha = .4,fill = "red")+
     theme_classic(base_size = 15) +
     theme(axis.text.x = element_text(vjust=1,hjust = 1,angle=45),
           plot.title = element_text(vjust=-15,hjust = 0.5)) 


sprbox.T<-ggdraw(sprbox) + 
     draw_image(sealion, x = 0.88, y =0.43, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) +
     draw_image(sealion, x = 0.77, y =0.45, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) + 
     draw_image(sealion, x = 0.66, y =0.50, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2)  +
     draw_image(sealion, x = 0.45, y =0.58, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2)+
     draw_image(sealion, x = 0.34, y =0.79, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) 


fallbox.T<-
     ggdraw(fallbox) +
     draw_image(bird, x = 0.7, y =0.77, hjust = 0.8, vjust = 1.1, width = 0.06, height = 0.2)+
     draw_image(bird, x = 0.8, y =0.76, hjust = 0.8, vjust = 1.1, width = 0.06, height = 0.2)+
     draw_image(bird, x = 0.91, y =0.64, hjust = 0.8, vjust = 1.1, width = 0.06, height = 0.2)+
     draw_image(sealion, x = 0.36, y =0.77, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) +
     draw_image(sealion, x = 0.45, y =0.55, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) +
     draw_image(sealion, x = 0.67, y =0.53, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) +
     draw_image(sealion, x = 0.78, y =0.5, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) +
     draw_image(sealion, x = 0.89, y =0.44, hjust = 0.8, vjust = 1.1, width = 0.08, height = 0.2) 


bottom.T <- textGrob("Temperature hypothesis", gp = gpar(fontsize = 20))
gridExtra::grid.arrange(sprbox.T,fallbox.T,nrow=1,ncol=2,bottom=bottom.T,padding = unit(0.5, "line"))
TempHyp.boxplots<- gridExtra::grid.arrange(sprbox.T,fallbox.T,nrow=1,ncol=2,bottom=bottom.T,padding = unit(0.5, "line"))




#Print Fig 5   
windows(width = 8, height = 6)
gridExtra::grid.arrange(sprbox.FW,fallbox.FW,nrow=1,ncol=2,bottom=bottom.FW)

windows(width = 8, height = 6)
gridExtra::grid.arrange(sprbox.T,fallbox.T,nrow=1,ncol=2,bottom=bottom.T)



#Fig 6. and Fig S3.1 Plot BRT Results--------------
# brt.spr<-read.csv(file="output/BRT/BRT.spr.influence.table.csv",row.names = NULL)
# brt.fall<-read.csv(file="output/BRT/BRT.fall.influence.table.csv",row.names = NULL)

run="spring"
#run="fall"

if(run=="spring") dat<-brt.spr
if(run=="fall") dat<-brt.fall
#reduce dataset to top 20% of variation
dat<-dat[dat$model %in% c("null","g1.d2d4") & dat$cumsum<20,]
#fix model numbers and colors  
dat[dat$H==1,"H2"]<-"H1"
dat[dat$H==8,"H2"]<-"H5"
dat[dat$H==9,"H2"]<-"H6"

#Reset all colors

#Default is green
dat$color2<-"#548235"
dat$color2[which(dat$link %in% str_subset(dat$link, "^d"))]<-"#C00001"
dat$color2[which(dat$link %in% str_subset(dat$link, "^b"))]<-"lightblue"
dat$color2[which(dat$link %in% str_subset(dat$link, "^a"))]<-"black"
dat$color2[which(dat$link %in% str_subset(dat$link, "^c"))]<-"pink"
#need to move this below: dat$color2[which(dat$color2=="#548235" & dat$To %in% str_subset(dat$To, "^d"))]<-"#C00001"


color2levels = c("black", "lightblue","#548235","pink", "#C00001")
# grouplevels = c("Climate","Prey","Salmon","Competitor","Predator")
dat$color2<-factor(dat$color2,levels=color2levels)
levels( dat$color2) 

#Add the From and To columns using an existing H.sim
from.code<- substr(dat$link, 1, 2)
to.code  <- substr(dat$link, 4, 5)
code.name<- substr(y, 1, 2)

for(i in 1:nrow(dat)){
     dat$From[i]<-grep(from.code[i],y,value=TRUE); head(dat,10) 
     dat$To[i]<-grep(to.code[i],y,value=TRUE); head(dat,10) 
}


#order From so that predators come first, then salmon, then prey, then climate
dat$From<-factor(dat$From,levels=unique(rev(sort(dat$From))))
a<-levels(dat$From);a
a<- c(str_subset(a, "^d"),str_subset(a, "^[^d]"));a
dat$From<-factor(dat$From,levels=a)

dat$color2[which(dat$color2=="#548235" & dat$To %in% str_subset(dat$To, "^d"))]<-"#C00001"

dat$To<-factor(dat$To,levels=unique(rev(sort(dat$To))))

dat$group2<-" "

assign(paste0("brt.dat.",run),dat,.GlobalEnv)


#separate out null model and plot
dat.base<-dat[dat$model=="null",]

myplot<-
     
     ggplot(dat.base, aes(x = (influence), y = interaction((To), (From),  sep = "&"),fill=color2)) +
     geom_col(position="dodge",width=0.5) +
     labs(x="Relative Influence",y=" ") +
     
     guides(
          y = guide_axis_nested(delim = "&"),
          y.sec = guide_axis_manual(
               breaks = interaction(dat$To, dat$From, sep = "&"),
               labels = dat$group2
          )
     ) +
     theme(
          axis.text.y.left = element_text(margin = margin(r = 5, l = 5)),
          ggh4x.axis.nesttext.y = element_text(margin = margin(r = 5, l = 5)),
          ggh4x.axis.nestline = element_blank()
     ) +
     
     facet_grid(~ H2)
myplot

#hack for colors
if(run=="spring")  mycol3<-c("#C00001","#548235","lightblue", "black")
if(run=="fall")  mycol3<-c("#C00001","#548235", "black")

unique(dat.base$color2)


brtplot.base<-
     myplot +
     ggtitle(paste0("Base network, ",run,"-run Chinook")) +
     scale_fill_manual(values =rev(mycol3))  +
     theme(legend.position ='none')

brtplot.base
assign(paste0("brtplot.base.",run),brtplot.base,.GlobalEnv)


#separate out mammal model and plot
dat.mammal<-dat[dat$model=="g1.d2d4",]

myplot<-
     
     ggplot(dat.mammal, aes(x = (influence), y = interaction((To), (From),  sep = "&"),fill=color2)) +
     geom_col(position="dodge",width=0.5) +
     labs(x="Relative Influence",y=" ") +
     
     guides(
          y = guide_axis_nested(delim = "&"),
          y.sec = guide_axis_manual(
               breaks = interaction(dat$To, dat$From, sep = "&"),
               labels = dat$group2
          )
     ) +
     theme(
          axis.text.y.left = element_text(margin = margin(r = 5, l = 5)),
          ggh4x.axis.nesttext.y = element_text(margin = margin(r = 5, l = 5)),
          ggh4x.axis.nestline = element_blank()
     ) +
     
     facet_grid(~ H2)
myplot

#hack for colors
if(run=="spring")   mycol4<-c("#C00001","pink","#548235", "black")
if(run=="fall") mycol4<-c("#C00001","#548235","lightblue", "black")


brtplot.mammal<-
     myplot +
     ggtitle(paste0("Mammal network, ",run,"-run Chinook")) +
     scale_fill_manual(values = rev(mycol4))  +
     theme(legend.position ='none')

brtplot.mammal
assign(paste0("brtplot.mammal.",run),brtplot.mammal,.GlobalEnv)

#repeat for fall 

windows(width = 6, height = 8)
gridExtra::grid.arrange(brtplot.base.fall,
                        brtplot.mammal.fall,
                        nrow=2,ncol=1)
windows(width = 6, height = 8)
gridExtra::grid.arrange(brtplot.base.spring,
                        brtplot.mammal.spring,
                        nrow=2,ncol=1)


