#Functions for QNM
#1st started this script 10/25/2021
# mycol=rev(brewer.pal(11,"Spectral"))



#Extract links from dia, then rebuild model -- apply method to my dia model ----------
extract.digraph<-function(dia="dia/HypoLayers null5b.dia"){
          ss0=model.dia(dia)
               ss0$sign1<-">"
               ss0$sign1[ss0$Type=="N"]<-"*"
               ss0$sign2<-NA
               ss0$sign2[ss0$Type=="P"]<- "<"
               ss0$sign2[ss0$Type=="N"]<- "*"
     #Add columns for the characters that represent the sign of the impact, then paste them together to create the link format (e.g., A->B)
          for (i in 1:max(ss0$Pair)){
               n<-which(ss0$Pair==i);n
               sign1<-ss0[n[1],"sign1"]
               sign2<-""
               if(length(n)==2){sign2<-ss0[n[2],"sign2"]}
               link<-paste(ss0$From[n[1]],ss0$To[n[1]],sep=paste0(" ",sign2,"-",sign1," "));link
               if(i==1){digraph<-link}
               if(i>1){digraph<-c(digraph,link)}
          
          }
     #Return the vector of links, with either one way or two way impacts, to create a new model
          return(digraph)
     #the next step is to rebuild the model as follows:  ss <- parse.digraph(digraph);ss
}

#SST.H.simulate.fxn is to run all of the temperature hypotheses for a given digraph
SST.H.simulate.fxn<-function(mydigraph=digraph.base,nsim=10000,digraphname="base"){
     set.seed(1000)   
     
     Hlist<-c("Prey","Condition","Competitors","JuvPredators","AllPredators","All")
     
     Hlinks<-list(  Prey=NA,
                    Condition=c("a1.Clim1 -*  e1.Ch.cond.yr","a1.Clim1 -*   e2.Ch.cond.sub","a1.Clim1 -*  e3.Ch.cond.Sep"),
                    Competitors=c("a1.Clim1 ->  c2.Comp.jelly","a1.Clim1 ->  c3.Comp","a1.Clim1 ->   c1.Comp.altprey"),
                    JuvPredators=c("a1.Clim1 ->  d1.Pred.sm", "a1.Clim1 ->  d2.Pred.lg",  "a1.Clim1 ->  d3.Pred"),     
                    AllPredators=c("a1.Clim1 ->  d1.Pred.sm", "a1.Clim1 ->  d2.Pred.lg",  "a1.Clim1 ->  d3.Pred","a1.Clim1 ->  d4.Pred.adult"),     
                    All=c("a1.Clim1 -*  e1.Ch.cond.yr","a1.Clim1 -*   e2.Ch.cond.sub","a1.Clim1 -*  e3.Ch.cond.Sep",
                          "a1.Clim1 ->   c1.Comp.altprey","a1.Clim1 ->  c2.Comp.jelly","a1.Clim1 ->  c3.Comp",
                          "a1.Clim1 ->  d1.Pred.sm","a1.Clim1 ->  d2.Pred.lg","a1.Clim1 ->  d3.Pred",
                          "a1.Clim1 ->  d4.Pred.adult"
                    )              )
     for (i in 1:length(Hlinks)){
          #          print(paste0("H",i))
          mydigraph1<-mydigraph
          if(i>1) mydigraph1<-c(mydigraph1,Hlinks[[i]])
          
          ss <-parse.digraph(mydigraph1);ss <-enforce.limitation(ss) #this puts self-limiting ss0 to every node
          H <- system.simulate(nsim, ss)
          assign(paste0("H",i,".digraph.",digraphname),ss,envir = .GlobalEnv)
          assign(paste0("H",i,".sim.",digraphname),H,envir = .GlobalEnv)
          print(paste0("H",i,".digraph.",digraphname))
          print(paste0("H",i,".sim.",digraphname))
     }#end H
}#end function


#SST.H1.simulate.fxn is for adding climate links one at a time to all nodes
SST.H1.simulate.fxn<-function(node="b1.Prey.yr",sign="neg",mydigraph=digraph.base,nsim=10000){
     # set.seed(1000)   
     
     digraphname=node  
     if(sign=="neg"){ link<-paste0("a1.Clim1 -* ",node);link }
     if(sign=="pos"){ link<-paste0("a1.Clim1 -> ",node);link } 
     
     
     if(!node %in% c("a1.Clim1","b1.Prey.yr","b2.Prey.sub"))  { mydigraph1<-c(mydigraph,link);print("change digraph")}
     if(node %in% c("a1.Clim1","b1.Prey.yr","b2.Prey.sub"))  { mydigraph1<-mydigraph;print("do not change digraph")}
     
     ss <-parse.digraph(mydigraph1);ss <-enforce.limitation(ss) #this puts self-limiting ss0 to every node
     H <- system.simulate(nsim, ss)
     assign(paste0("H1.digraph.",sign,".",digraphname),ss,envir = .GlobalEnv)
     assign(paste0("H1.sim.",sign,".",digraphname),H,envir = .GlobalEnv)
     # print(paste0("H1.digraph.",sign,".",digraphname))
     print(paste0("H1.sim.",sign,".",digraphname))
     
     # H<-get(paste0("H1.sim.pos.",nodes[nodes.testable[i]]))
     # pos<-sign.determinacy.perturb(sim=H,perturb0=c(1,rep(0,length(nodes)-1)),whichpress= nodes.testable[i] ,press=  1 ,epsilon=1.0E-5) 
     # round(pos$mean.impacts[c("g1.SprCh.adult", "g3.FallCh.adult")],2)
     
}#end function


#Results -- sign determinacy, mean impacts --------

sign.determinacy<-function(sim,epsilon=1.0E-5) {
     edges <- sim$edges
     As <- sim$A #this is where they store the impact of the model run on each node, a list of #accepted models (e.g., 1000), and each element is
     nodes <- node.labels(edges)
     monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses
     perturb<-c(1,0,rep(0,(length(nodes)-2)))
     
       #Set up matrix for all results at all nodes
          impacts.all <- matrix(0,length(As),length(nodes),dimnames=list(paste0("sim",1:length(As)),nodes))
     SprCh<-grep("SprCh",nodes)
     FallCh<-grep("FallCh",nodes)
     
       #Sign determinacy:
       #Collect results -- impacts.all shows the sign at every node, survresults adds results as you go through all the models and produces the sum
       results <- matrix(0,length(nodes),3)
         for(k in 1:length(As)) {
           impact <- signum(drop(As[[k]]%*%perturb),epsilon=epsilon)
           if(all(monitor==impact,na.rm=T)) {
             results <- results + outer(impact,-1:1,'==')
           rownames(results) <- nodes
           impacts.all[k,]=t(impact) #This is the sign at each node for a given model run 
           surv.SprCh=results[SprCh,] #This specifies the node of interest, e.g. Survival [node[20]="g1.SprCh.Adult"]
           surv.FallCh=results[FallCh,] #This specifies the node of interest, e.g. Survival [node[21]="g3.LCFallCh.Adult"]
           }
         }
       names(surv.SprCh)<-names(surv.FallCh)<-c("neg","neutral","pos")
       mean.impacts<-apply(impacts.all,2,mean)
     x<-  list("surv.SprCh"=surv.SprCh,"surv.FallCh"=surv.FallCh,"mean.impacts"=mean.impacts,"impacts.all"=impacts.all)
       return(x)
} 

sign.determinacy.perturb<-function(sim,perturb0=rep(0,length(node.labels(sim$edges))),whichpress=1,press=1,epsilon=1.0E-5) {
     #whichpress here is a vector of which nodes you want to change (e.g., from grep)
     #press here is the direction of the perturbation for each node listed in whichpress (-1 or 1), must be same length as whichpress
     edges <- sim$edges
     As <- sim$A #this is where they store the impact of the model run on each node, a list of #accepted models (e.g., 1000), and each element is
     nodes <- node.labels(edges)
     monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses
     perturb<-perturb0
     perturb[whichpress]<-press
     #Set up matrix for all results at all nodes
     impacts.all <- matrix(0,length(As),length(nodes))
     colnames(impacts.all) <- nodes
     SprCh<-grep("SprCh",nodes)
     FallCh<-grep("FallCh",nodes)
     #Sign determinacy:
     #Collect results -- impacts.all shows the sign at every node, survresults adds results as you go through all the models and produces the sum
     results <- matrix(0,length(nodes),3)
     for(k in 1:length(As)) {
          impact <- signum(drop(As[[k]]%*%perturb),epsilon=epsilon)
          if(all(monitor==impact,na.rm=T)) {
               results <- results + outer(impact,-1:1,'==')
               rownames(results) <- nodes
               impacts.all[k,]=t(impact) #This is the sign at each node for a given model run 
               surv.SprCh=results[SprCh,] #This specifies the node of interest, e.g. Survival [node[20]="g1.SprCh.Adult"]
               surv.FallCh=results[FallCh,] #This specifies the node of interest, e.g. Survival [node[21]="g3.LCFallCh.Adult"]
          }
     }
     names(surv.SprCh)<-names(surv.FallCh)<-c("neg","neutral","pos")
     mean.impacts<-apply(impacts.all,2,mean)
     x<-  list("surv.SprCh"=surv.SprCh,"surv.FallCh"=surv.FallCh,"mean.impacts"=mean.impacts,"impacts.all"=impacts.all)
     return(x)
} 



#Barplots--------
#reverse plot orientation
     mybarplot.rev<-function(survsum,scenario,cex.axis = 0.8,whichpress=0){
               n=length(survsum)     
               lwidth <- max(strwidth(s=names(survsum),units="inches",cex=cex.axis))
               #this line colors all rows red, blue, or grey
               if(whichpress==0) {
                          barplot(height=rev(survsum),horiz=T, las=1,col=ifelse(rev(survsum) > 0.6, 4, ifelse(rev(survsum) < -0.6,2,8)), border=NA,
                                  xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
               }
               #this line makes the first entry black, to indicate that is was a pulse
               if(whichpress==1) {
                        barplot(height=rev(survsum),horiz=T, las=1,col=c(ifelse(rev(survsum[2:n]) > 0.6, 4, ifelse(rev(survsum[2:n]) < -0.6,2,8)),1), border=NA,
                                 xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
               }
               if(whichpress==2) {
                        barplot(height=rev(survsum),horiz=T, las=1,col=c(ifelse(rev(survsum[3:n]) > 0.6, 4, ifelse(rev(survsum[3:n]) < -0.6,2,8)),1,1), border=NA,
                                 xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
               }
     }

nonames.mybarplot.rev<-function(survsum,scenario,cex.axis = 0.8,whichpress=0){
     #we are reversing survsum so it puts climate on the top of the graph, and adults on the bottom
     n=length(survsum)
          lwidth <- max(strwidth(s=names(survsum),units="inches",cex=cex.axis))
          #this line colors all rows red, blue, or grey
          if(whichpress==0) {
               barplot(height=rev(survsum),horiz=T, las=1,col=ifelse(rev(survsum) > 0.6, 4, ifelse(rev(survsum) < -0.6,2,8)), border=NA,
                          names.arg="",  xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
          }
          #this line makes the first entry black, to indicate that is was a pulse
          if(whichpress==1) {
                   barplot(height=rev(survsum),horiz=T, las=1,col=c(ifelse(rev(survsum[2:n]) > 0.6, 4, ifelse(rev(survsum[2:n]) < -0.6,2,8)),1), border=NA,
                           names.arg="",  xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
          }
          #this line makes the first entry black, to indicate that is was a pulse
          if(whichpress==2) {
                   barplot(height=rev(survsum),horiz=T, las=1,col=c(ifelse(rev(survsum[3:n]) > 0.6, 4, ifelse(rev(survsum[3:n] )< -0.6,2,8)),1,1), border=NA,
                           names.arg="",  xlab="Mean outcome",main=scenario,cex.names = cex.axis,xlim=c(-1,1))
          }
     }

