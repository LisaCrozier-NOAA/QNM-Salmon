#BRT Function

BRT.fxn<-function(hh=1,digraphname="base",tc=10,lr=0.1,nsim=10000){
     
     simname=paste0("H",hh,".sim.",digraphname);print(simname)
     spr.modelname=paste0("spr.",simname,".C10",".tc",tc,".lr",lr);spr.modelname
     fall.modelname=paste0("fall.",simname,".C10",".tc",tc,".lr",lr);fall.modelname
     
          perturb=c(1,rep(0,length(node.labels(sim$edges))-1))
     
     print(Sys.time())               
          sim<-get(simname)
          edges <- sim$edges
          As <- sim$A
          nodes <- node.labels(edges)
          wt<-sim$w
          
          outcomes <- matrix(0,length(As),2,dimnames=list(NULL,c("sprCh","fallCh")))
          for(k in 1:length(As)) {
               impact <- signum(drop(As[[k]]%*%perturb),epsilon=0.5)
               names(impact)<-nodes
               outcomes[k,c("sprCh","fallCh")]<-impact[c("g1.SprCh.adult","g3.FallCh.adult")]
          }
          wt.outcomes<-cbind(outcomes,wt)
 
          
          #fix colnames     
               n<-grep("non.NCC", colnames(wt))
               name1<-substr(colnames(wt),1,2);name1
               name1[n]<-c("NCCspr","NCCfall","NCCfall","NCCspr")
               
               name2<-sapply(strsplit(colnames(wt), "-"), function(x){return(x[[2]])})
               name2<-substr(name2,3,4);name2
               n<-grep("no", name2)
               name2[n]<-c("NCCfall","NCCspr")
               mynames<-paste(name1,name2,sep=".")
               mynames<-c("sprCh","fallCh",mynames)
               
               colnames(wt.outcomes)<-mynames
               wt.outcomes<-as.data.frame(wt.outcomes)

          #save the response for each set of link weights 
               assign(paste("wt.outcomes",simname,sep="."),wt.outcomes,envir = .GlobalEnv)
          
          #Set up model selection
               dat<-get(paste("wt.outcomes",simname,sep="."))
               dat<-as.data.frame(dat[1:nsim,])
               dat$sprCh[dat$sprCh==-1]<-0
               dat$fallCh[dat$fallCh==-1]<-0
               
               
               spr.model <- gbm.step(data=dat, gbm.x = 3:ncol(dat), gbm.y = 1,
                                     family = "bernoulli", tree.complexity = tc,
                                     learning.rate = lr, bag.fraction = 0.5)
               assign(spr.modelname,spr.model,envir = .GlobalEnv)
               eval(parse(text = paste0("save('",spr.modelname,"',file = '","BRT.",spr.modelname,".",nsim,".RData')")))
               
               summary(fall.model)
               print(spr.modelname)
               
               fall.model <- gbm.step(data=dat, gbm.x = 3:ncol(dat), gbm.y = 2,
                                      family = "bernoulli", tree.complexity = tc,
                                      learning.rate = lr, bag.fraction = 0.5)
               
               assign(fall.modelname,fall.model,envir = .GlobalEnv)
               eval(parse(text = paste0("save('",fall.modelname,"',file = '","BRT.",fall.modelname,".",nsim,".RData')")))
               
               summary(fall.model)
               print(fall.modelname)
          
          print(Sys.time())            
}