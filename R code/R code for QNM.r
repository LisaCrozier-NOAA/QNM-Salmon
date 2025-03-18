#This code was written by Lisa Crozier for a model described in the paper:
#Climate change and marine food webs: navigating structural uncertainty using qualitative network analysis with insights for salmon survival
#Authors:  Crozier, L.G, D. G. E. Gomes, D. D. Huff
#Published in Global Change Biology in 2025
#This code was last updated on March 10, 2025
getwd()
setwd("C:/Users/Lisa.Crozier/Documents/Marine survival/QNM/GCB Submission Sep 2024/R code")
rm(list=ls())

#Packages for QNM
library(XML)
library(tcltk2)
#install.packages("QPress",,"http://rforge.net/",type="source")
library("QPress")
library(tidyr)

#Packages for BRT analysis
library(dismo) #for BRT
library(gbm) #for BRT

#Packages for figures
library(ggplot2)
library(RColorBrewer) #for Functions.network.r
library(ggrepel) # for geom_text_repel for tradeoff plots Fig 4
library(grid) #for putting a line between facets in Fig 4
library(gtable) #for putting a line between facets in Fig 4

library(png)
library(cowplot) #for drawing the bird and sealion pngs onto plot
library(magick) #for drawing the bird and sealion pngs onto plot
library(ggh4x) #for facet_nested and guide_axis_nested in BRT graph 

#Custom-made functions
Hlist<-c("Prey","Condition","Competitors","JuvPredators","AllPredators","All")
source("inputs/Functions.network.FY25.r")
source("inputs/Digraphs.define.r")
source("inputs/BRT.fxn.r")
source("inputs/BRT.topvar.fxn.r")


#Input files------
model="HypoLayers null11.birdsSSLnocond.nolayers.dia"
digraph.base<-extract.digraph(dia=paste0("inputs/",model))

#Run SST H -----------
     SST.H.simulate.fxn(mydigraph=digraph.base,nsim=1000,digraphname="base")
     SST.H.simulate.fxn(mydigraph=digraph.g1.d2d4,nsim=1000,digraphname="mammal")

#Run all scenarios (allXP) --------
     target.list<-c("Base","Mammals","Birds","AltPrey","FishPred1","FishPred2")
     digraph.list<-c("base","g1.d2d4","d1.e1e2e3","c1.juv","d3.b1","d3.f1")
     
     set.seed(1000)
     nsim=100000
     XP<-data.frame(H=paste0("H",1:6),Model="Base",Nmodels=NA,PctAccept=NA,SprCh=NA,FallCh=NA,Hdescription2=paste0(paste0("H",1:6),": ",Hlist),Hdescription=Hlist)
     for (d in 1:length(digraph.list)){
          digraphname<-digraph.list[d]
          XP$Model<-target.list[d]
          
          SST.H.simulate.fxn(mydigraph=get(paste("digraph",digraphname,sep=".")),nsim=nsim,digraphname=digraphname)
          for(i in 1:length(Hlist)){
               H<-get(paste0("H",i,".sim.",digraphname))
               x<-sign.determinacy(H)
               XP[i,c("SprCh","FallCh")]<-x$mean.impacts[c("g1.SprCh.adult","g3.FallCh.adult")]
               XP[i,c("Nmodels","PctAccept")]<-c(H$total,round(100*H$accepted/H$total))
          }
          
          if(d==1) allXP<-XP
          if(d>1) allXP<-rbind(allXP,XP);allXP
     }
     
     #save table     
      write.csv(allXP,file="outputs/allXP.csv",row.names = FALSE)     
     
     
#Table TS4: Stability indices of all food webs --------
     TS4.base<-allXP[1:6,]
     TS4.H1<-allXP[which(allXP$H=="H1"),]
     TS4<-rbind.data.frame(TS4.base,TS4.H1[rev(order(TS4.H1$PctAccept)),])
     TS4
     
     #save table     
      write.csv(TS4,file="outputs/TS4.csv",row.names = FALSE)     
     
#Table TS5: Impact on all nodes -----------
     TS5.meanimpacts<-data.frame(Node=node.labels(H1.sim.base$edges))
          for(hh in 1:length(Hlist)){
               H<-get(paste0("H",hh,".sim.base"))
               x<-sign.determinacy(H)
               TS5.meanimpacts[,paste0("H",hh)]<-round(x$mean.impacts,2)
          }
     TS5.meanimpacts
     
     #save table     
      write.csv(TS5.meanimpacts,file="outputs/TS5.meanimpacts.csv",row.names = FALSE)     
     
#Table TS6 One-at-a-time analysis of adding a climate press to each node, outcomes for salmon ----------     
     nsim=10000
      mylabels=substr(nodes, start = 1, stop = 2)
      mylabels[21:22]<-c("H1","H2")
      

     for(d in 1:2){
          sim<-get(paste0("H1.sim.",digraph.list[d]))
          nodes<-node.labels(sim$edges)
          nodes.testable<-4:length(nodes) 
     #Set up data frame     
          H1.allpress<-data.frame(Node=nodes, Press=1,Network=target.list[d],SpringPos=NA,FallPos=NA,SpringNeg=NA,FallNeg=NA)
     #Add positive and negative links to each node
          for(i in 1:length(nodes)){
          #Add a link in the positive direction from a1 to node[i]
               SST.H1.simulate.fxn(node=nodes[i],sign="pos",mydigraph=get(paste0("digraph.",digraph.list[d])),nsim=nsim)
               H<-get(paste0("H1.sim.pos.",nodes[i]))
               #whichpress here is a vector of which nodes you want to change 
               #press here is the direction of the perturbation for each node listed in whichpress (-1 or 1), must be same length as whichpress
               pos<-sign.determinacy.perturb(sim=H,perturb0=c(1,rep(0,length(nodes)-1)),whichpress= 1 ,press=  1 ,epsilon=1.0E-5) 
               
          
          #Add a link in the negative direction from a1 to node[i]
               SST.H1.simulate.fxn(node=nodes[i],sign="neg",mydigraph=get(paste0("digraph.",digraph.list[d])),nsim=nsim)
               H<-get(paste0("H1.sim.neg.",nodes[i]))
               neg<-sign.determinacy.perturb(sim=H,perturb0=c(1,rep(0,length(nodes)-1)),whichpress=1 ,press= 1 ,epsilon=1.0E-5) 
               
          #Collect results
               H1.allpress[i,c("SpringPos","FallPos")]<-round(pos$mean.impacts[c("g1.SprCh.adult", "g3.FallCh.adult")],2)
               H1.allpress[i,c("SpringNeg","FallNeg")]<-round(neg$mean.impacts[c("g1.SprCh.adult", "g3.FallCh.adult")],2)
               
          } #end nodes to test
          
          
          
          #Worst case: positive link on predators and competitors, negative link on prey and salmon
               
               H1.allpress$WorstScenario<-"neg"  
               H1.allpress$WorstScenario[grep("d",mylabels)]<-"pos"
               H1.allpress$WorstScenario[grep("c",mylabels)]<-"pos"
               H1.allpress[order(H1.allpress$WorstScenario),]
               
               H1.allpress$WorstScenarioN<-H1.allpress$SpringNeg
               H1.allpress$WorstScenarioN[H1.allpress$WorstScenario=="pos"]<-H1.allpress$SpringPos[H1.allpress$WorstScenario=="pos"]
               H1.allpress<-H1.allpress[order(H1.allpress$WorstScenarioN),]
               
                     assign(paste("H1.allpress",target.list[d],sep="."),H1.allpress,envir = .GlobalEnv)
          } #end digraph.list
          
     H1.allpress<-rbind(H1.allpress.Base,H1.allpress.Mammals)
     head(H1.allpress)  
     
     #save table     
      write.csv(H1.allpress,file="outputs/TS6.H1.allpress.csv",row.names = FALSE)     
      
#BRT analysis -------------------------------------------------------------------          
      #run sims of desired size
      model="HypoLayers null11.birdsSSLnocond.nolayers.dia"
      digraph.base<-extract.digraph(dia=paste0("dia/",model))
      nsim=10000
      set.seed(nsim)
      for (d in 1:2){
           SST.H.simulate.fxn(mydigraph=get(paste("digraph",digraphname,sep=".")),nsim=nsim,digraphname=digraph.list[d])
      }
      
      #run BRT model for spring and fall run, 3 temperature hypotheses, base and mammal food webs (slow)
      for (h in c(1,5,6)){
           for (dd in c("base","g1.d2d4")){
                BRT.fxn(hh=h,digraphname=dd,tc=10,lr=0.01,nsim=1000)
           }}
      
      #Extract the most influential variables from each model
      brt<-data.frame(run="Spring",MG="C10",H="H1",model="Base",simname=NA,link=NA,influence=NA,cumsum=0,rank=1,color=NA);brt
      brt.spr<-brt.fall<-brt[-1,]
      
      
      for (hh in c(1,5,6)){
           for (dd in c("null","g1.d2d4")){
                digraphname=dd
                tc=10;lr=0.01
                
                simname=paste0("H",hh,".sim.",digraphname)
                spr.modelname=paste0("spr.",simname,".C10",".tc",tc,".lr",lr);modelname<-spr.modelname
                print(modelname)
                load(paste("outputs/BRT",modelname,"10000.Rdata",sep="."),verbose=T)
                x<-BRT.topvar.fxn(modelname,pct=25)
                
                topvar2<-x
                names(topvar2)<-c("link","influence","cumsum","rank","color")
                topvar2[1,c("run","MG","H","model","simname")]<-c("Spring","C10",hh,dd,simname);topvar2
                topvar2 <- topvar2 %>% fill(c("run","MG","H","model","simname"), .direction = "down")
                brt.spr<-rbind(brt.spr,topvar2)
                
                fall.modelname=paste0("fall.",simname,".C10",".tc",tc,".lr",lr);modelname<-fall.modelname
                print(modelname)
                load(paste("outputs/BRT",modelname,"10000.Rdata",sep="."),verbose=T)
                x<-BRT.topvar.fxn(modelname,pct=25);x
                topvar2<-x
                names(topvar2)<-c("link","influence","cumsum","rank","color")
                topvar2[1,c("run","MG","H","model","simname")]<-c("Fall",".C10",hh,dd,simname);topvar2
                topvar2 <- topvar2 %>% fill(c("run","MG","H","model","simname"), .direction = "down")
                brt.fall<-rbind(brt.fall,topvar2);brt.fall
           }}
      brt.spr
      brt.fall      
      
      brt.spr<-brt.spr[!duplicated(brt.spr),]
      brt.fall<-brt.fall[!duplicated(brt.fall),]
      
     #Save results      
      write.csv(brt.spr,file="outputs/brt.spr.allvar.csv")
      write.csv(brt.fall,file="outputs/brt.fall.allvar.csv")
      
 