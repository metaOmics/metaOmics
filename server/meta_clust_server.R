meta_clust_server <- function(input, output,session) {
    s <- DB.load.active(db)
    if (is.null(s))
        s <- "No active study"
    else
        datasets <- s@datasets
                                        #k=input$k,wbounds=input$wBounds,lambda=input$lambda
    runClust <- reactiveValues(data = NULL)
    
    tuneIndStudyK <- function(aS, topPercent=0.1, B = 100, seed=15213, verbose=FALSE){
      ## aS: a study, n*p, sample*gene
      ## topPercent: top percentage of genes with the largest variance
      ## B: number of permutation
      ## seed: random seed
      ## verbose: whether print out intermediate result
      varS <- apply(aS,2,var)
      bS <- aS[,varS > quantile(varS,1-topPercent)]
      set.seed(seed)
      gskmn <- clusGap(bS, FUN = kmeans, nstart = 20, K.max = 8, B = B, verbose=verbose)
      gskmn
    }
    
    observeEvent(input$tuneK, {
      if (is.null(s))
        s <- "No active study"
      else{
        wait(session, "Tuning number of clusters for meta sparse k means...")
        for(i in 1:length(datasets)){	
          gskmn <- tuneIndStudyK(datasets[[i]], topPercent=0.1, B = 10, verbose=TRUE)
          png(paste(input$outDir,'/gskmn',i,'.png',sep=""))
          plot(gskmn, main = paste('gap statistics for ',names(datasets)[i],sep=''))
          dev.off()
        }
        done(session)
      }
    })

    observeEvent(input$tuneW, {
      wait(session, "Tuning wbounds for meta sparse k means...")
      gapStatResult <- calculateGap(datasets,K=input$KforW,wbounds=input$min:input$max,B=input$B)
      png(paste(input$outDir,'/mskmGapStatstics.png',sep=""))
      plot(gapStatResult$wbounds,gapStatResult$gapStat,type='b',xlab='mu',ylab='gapStat') 
      arrows(gapStatResult$wbounds, gapStatResult$gapStat-gapStatResult$se.score, 
             gapStatResult$wbounds, gapStatResult$gapStat+gapStatResult$se.score, length=0.05, angle=90, code=3)
      dev.off()
      done(session)
    })
    
    
    observeEvent(input$clustGo, {
        runClust <- TRUE
        
        wait(session, "Running meta sparse k means...")
        set.seed(15213)
        res = MetaSpaKmeans(x=datasets,K=input$k,wbounds=input$wBounds)
        
        ## output gene list
        geneList <- res$ws
        write.csv(geneList,paste(input$outDir,'/geneList.csv',sep="")) # need to add the output directory ??
        
        ## output labels
        for(i in 1:length(datasets)){	
          afileName <- paste(input$outDir,'/mskm_label_',names(datasets)[i],'.csv',sep='')
          alabel <- res$Cs[[i]]
          names(alabel) <- rownames(datasets[[i]])	
          write.csv(alabel, afileName)
        }
        done(session)
        
        wait(session, "Saving Sparse K Means results...")
        ## visualization.
        for(i in 1:length(datasets)){	
          afileName <- paste('heatmap_',names(datasets)[i],'.png',sep='')
          
          png(afileName)
          if(i==1){
            geneOrder = getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i],Rowv=TRUE,labCol="",labRow="")	
          } else {
            getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
          }
          
          dev.off()
        }
        done(session)
        output$heatmap <- renderPlot({
          wait(session, "Visualizing Sparse K Means results...")
          #par(mfrow=c(1,length(datasets))) #does this work?
          #OW try https://gist.github.com/wch/5436415/
          
          for(i in 1:length(datasets)){	
            if(i==1){
              geneOrder = getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i],Rowv=TRUE,labCol="",labRow="")	
            } else {
              getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
            }
          }
          done(session)
          #par(mfrow=c(1,1))
          #         getWsHeatmap(t(datasets[[1]]),res$Cs[[1]],res$ws,main=names(datasets)[1],Rowv=TRUE,labCol="",labRow="")
       })
    })
    
    output$heatmap <- renderPlot({
        if (is.null(runClust))          return()
    })
}
