
#digraph names -------
digraph.list<-c("base","g1.d2d4","d1.e1e2e3","c1.juv","d3.b1","d3.f1")
target.list<-c("Base","Mammals","SprCh","FallCh","Birds","AltPrey","FishPred1","FishPred2")
mytitle.list<-c("Base",
                "Mammals: Positive impact from consuming adult salmon removed",
                "Birds: Add compensatory effect of birds",
                "AltPrey: Remove alt prey benefit to spring Ch",
                "FishPred1: reduce consumption of Spr Ch prey",
                "FishPred2: reduce predation of Spr Ch")




#Define digraphs ----------
     digraphname="base"; mytitle="Base";target="Base"
     model="HypoLayers null11.birdsSSLnocond.nolayers.dia"
     digraph.base<-extract.digraph(dia=paste0("inputs/",model))
     
     digraphname="g1.d2d4"; mytitle="Mammals: Positive impact from consuming adult salmon removed";target="Mammals"
     n=grepl("(?=.*Pred)(?=.*adult)",digraph.base,perl=TRUE);digraph.base[n]
     digraph.g1.d2d4<-digraph.base
     digraph.g1.d2d4[n]<-c( "g1.SprCh.adult *- d4.Pred.adult","g1.SprCh.adult *- d2.Pred.lg",
                            "g3.FallCh.adult *- d4.Pred.adult","g3.FallCh.adult *- d2.Pred.lg")
     
     digraphname="d1.e1e2e3";mytitle="Birds: Add compensatory effect of birds";target="Birds"
     newdigraph<-c(digraph.base,"d1.Pred.sm -> e1.Ch.cond.yr",  "d1.Pred.sm -> e3.Ch.cond.Sep", "d1.Pred.sm -> e2.Ch.cond.sub")
     n=which(grepl("(?=.*d1)(?=.*e1|.*e2|.*e3)",newdigraph,perl=TRUE));n;newdigraph[n]
     assign(paste("digraph",digraphname,sep="."),newdigraph,envir = .GlobalEnv)
     
     digraphname="c1.juv";mytitle="AltPrey: Remove alt prey benefit to spring Ch";target="AltPrey"
     n=which(grepl("(?=.*c1)(?=.*juv)",digraph.base,perl=TRUE));n;digraph.base[n]
     newdigraph<-digraph.base[-n]
     newdigraph<-c(newdigraph,"b1.Prey.yr *-> c1.Comp.altprey")
     n=which(grepl("(?=.*c1)",newdigraph,perl=TRUE));n;newdigraph[n]
     assign(paste("digraph",digraphname,sep="."),newdigraph,envir = .GlobalEnv)
     
     digraphname="d3.b1";mytitle="FishPred1: reduce consumption of Spr Ch prey";target="FishPred1"
     n=which(grepl("(?=.*d3)(?=.*b1)",digraph.base,perl=TRUE));n;digraph.base[n]
     newdigraph<-digraph.base[-n]
     assign(paste("digraph",digraphname,sep="."),newdigraph,envir = .GlobalEnv)
     
     digraphname="d3.f1";mytitle="FishPred2: reduce predation of Spr Ch";target="FishPred2"
     n=which(grepl("(?=.*d3)(?=.*f1)",digraph.base,perl=TRUE));n;digraph.base[n]
     newdigraph<-digraph.base[-n]
     assign(paste("digraph",digraphname,sep="."),newdigraph,envir = .GlobalEnv)



