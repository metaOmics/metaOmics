meta_clust_server <- function(input, output,session) {
    s <- DB.load.active(db)
    datasets <- NULL
    if (is.null(s))
        sendErrorMessage(session, "No active study")
    else
        datasets <- s@datasets
    datasets <- t(datasets) # What's the dimension like??
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
            wait(session, "Tuning number of clusters for meta sparse k means")
            for(i in 1:length(datasets)){	
                gskmn <- tuneIndStudyK(datasets[[i]], topPercent=0.1, B = 10, verbose=TRUE)
                png(paste(input$outDir,'/gskmn',i,'.png',sep=""))
                plot(gskmn, main = paste('gap statistics for ',names(datasets)[i],sep=''))
                dev.off()
            }

            
            output$plots <- renderUI({
                plot_output_list <- lapply(1:input$n, function(i) {
                    plotname <- paste("meta_clust-plot", i, sep="")
                    plotOutput(plotname, height = 280, width = 250)
                })
                
                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
                do.call(tagList, plot_output_list)
            })
            
            for (i in 1:length(datasets)) {
                                        # Need local so that each item gets its own number. Without it, the value
                                        # of i in the renderPlot() will be the same across all instances, because
                                        # of when the expression is evaluated.
                local({
                    my_i <- i
                    plotname <- paste("plot", my_i, sep="")
                    
                    output[[plotname]] <- renderPlot({
                        plot(1:my_i, 1:my_i,
                             xlim = c(1, length(datasets)),
                             ylim = c(1, length(datasets)),
                             main = paste("1:", my_i, ".  n is ", length(datasets), sep = "")
                             )
                    })
                })
            }
            
            
            done(session)
            
            updateSliderInput(        )#....??
            sendSuccessMessage(session, "K updated")
        }
    })

    observeEvent(input$tuneW, {
        wait(session, "Tuning wbounds for meta sparse k means")
        gapStatResult <- calculateGap(datasets,K=input$KforW,wbounds=input$min:input$max,B=input$B)
        png(paste(input$outDir,'/mskmGapStatstics.png',sep=""))
        plot(gapStatResult$wbounds,gapStatResult$gapStat,type='b',xlab='mu',ylab='gapStat') 
        arrows(gapStatResult$wbounds, gapStatResult$gapStat-gapStatResult$se.score, 
               gapStatResult$wbounds, gapStatResult$gapStat+gapStatResult$se.score, length=0.05, angle=90, code=3)
        dev.off()
        done(session)

        updateSliderInput(        )#....??
        sendSuccessMessage(session, "Wbounds updated")
    })
    
    
    observeEvent(input$clustGo, {
        runClust <- TRUE
        
        wait(session, "Running meta sparse k means. Go get a coffee.")
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
        
        wait(session, "Saving sparse K means results")
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
            wait(session, "Visualizing sparse K means results...")
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
            sendSuccessMessage(session, "Meta sparse K means complete. Heatmap saved to output directory")
                                        #par(mfrow=c(1,1))
        })
    })            

}
