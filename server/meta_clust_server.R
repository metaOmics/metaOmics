meta_clust_server <- function(input, output,session) {
    s <- DB.load.active(db)
    datasets <- NULL
    if (is.null(s))
        sendErrorMessage(session, "No active study")
    else
        datasets <- s@datasets
    datasets <- lapply(datasets,t)
    observeEvent(input$tabChange, {
      dir.create(paste(DB.load.working.dir(db),"metaClust",sep="/"))
    })

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
        return(gskmn)
    }

    observeEvent(input$tuneK, {
        if (is.null(s))
            s <- "No active study"
        else{
            gskmn <- list()
            wait(session, "Tuning number of clusters for meta sparse k means")
            for(i in 1:length(datasets)){	
                gskmn[[i]] <- tuneIndStudyK(datasets[[i]], topPercent=0.1, B = 10, verbose=TRUE)
                png(paste( DB.load.working.dir(db),"/metaClust/gskmn",i,".png",sep=""))
                plot(gskmn[[i]], main = paste('gap statistics for ',names(datasets)[i],sep=''))
                dev.off()
            }

            
            output$plotsK <- renderUI({
                plot_output_list <- lapply(1:length(datasets), function(i) {
                    plotname <- paste("meta_clust-plotsK", i, sep="")
                    plotOutput(plotname, height = 280, width = 350)
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
                    plotname <- paste("plotsK", my_i, sep="")
                    
                    output[[plotname]] <- renderPlot({
                        plot(gskmn[[my_i]], main = paste('gap statistics for ',names(datasets)[my_i],sep=''))
                    })
                })
            }
            
            done(session)
            
            sendSuccessMessage(session, "K updated")
        }
    })

    observeEvent(input$tuneW, {
        wait(session, "Tuning wbounds for meta sparse k means")
        gapStatResult <- calculateGap(datasets,K=input$KforW,wbounds=input$min:input$max,B=input$B)
        png(paste( DB.load.working.dir(db),"/metaClust/mskmGapStatstics.png",sep=""))
        plot(gapStatResult$wbounds,gapStatResult$gapStat,type='b',xlab='mu',ylab='gapStat') 
        arrows(gapStatResult$wbounds, gapStatResult$gapStat-gapStatResult$se.score, 
               gapStatResult$wbounds, gapStatResult$gapStat+gapStatResult$se.score, length=0.05, angle=90, code=3)
        dev.off()

        output$plotW <- renderPlot({
            plot(gapStatResult$wbounds,gapStatResult$gapStat,type='b',xlab='mu',ylab='gapStat') 
            arrows(gapStatResult$wbounds, gapStatResult$gapStat-gapStatResult$se.score, 
                   gapStatResult$wbounds, gapStatResult$gapStat+gapStatResult$se.score, length=0.05, angle=90, code=3)            
        })
        
        done(session)

        sendSuccessMessage(session, "Wbounds updated")
    })

    observe({
        val <- input$KforW 
        updateSliderInput(session, "k", value = val,
                          min = 0, max = max(20, val+5), step = 1)
    })

    observeEvent(input$clustGo, {
        runClust <- TRUE
        
        wait(session, "Running meta sparse k means. Go get a coffee.")
        set.seed(15213)
        res = MetaSpaKmeans(x=datasets,K=input$k,wbounds=input$wBounds)
        
        ## output gene list
        geneList <- res$ws
        write.csv(geneList,paste( DB.load.working.dir(db),"/metaClust/geneList.csv",sep=""))
        
        ## output labels
        for(i in 1:length(datasets)){	
            afileName <- paste( DB.load.working.dir(db),"/metaClust/mskm_label_",names(datasets)[i],".csv",sep='')
            alabel <- res$Cs[[i]]
            names(alabel) <- rownames(datasets[[i]])	
            write.csv(alabel, afileName)
        }

        ## visualization.
        for(i in 1:length(datasets)){	
            afileName <- paste( DB.load.working.dir(db),"/metaClust/heatmap_",names(datasets)[i],".png",sep='')
            
            png(afileName)
            if(i==1){
                geneOrder = getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i],Rowv=TRUE,labCol="",labRow="")	
            } else {
                getWsHeatmap(t(datasets[[i]]),res$Cs[[i]],res$ws,main=names(datasets)[i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
            }
            
            dev.off()
        }

        done(session)
        wait(session, "Visualizing sparse K means results...")

        output$heatmaps <- renderUI({
            plot_output_list <- lapply(1:length(datasets), function(i) {
                plotname <- paste("meta_clust-plot", i, sep="")
                tags$li(class="DocumentItem",
                  plotOutput(plotname, height = 480, width = 450)
                )
            })
            
                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
            tags$div(class="DocumentList",
              tags$ul(class="list-inline",
                plot_output_list
              )
            )
        })
        
        for (i in 1:length(datasets)) {
                                        # Need local so that each item gets its own number. Without it, the value
                                        # of i in the renderPlot() will be the same across all instances, because
                                        # of when the expression is evaluated.
            local({
                my_i <- i
                plotname <- paste("plot", my_i, sep="")
                
                output[[plotname]] <- renderPlot({
                    if(my_i==1){
                        geneOrder = getWsHeatmap(t(datasets[[my_i]]),res$Cs[[my_i]],res$ws,main=names(datasets)[my_i],Rowv=TRUE,labCol="",labRow="")	
                    } else {
                        getWsHeatmap(t(datasets[[my_i]]),res$Cs[[my_i]],res$ws,main=names(datasets)[my_i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
                    }
                    
                })
            })
        }
        
        done(session)
        sendSuccessMessage(session, "Meta sparse K means complete. Heatmap saved to output directory")
                                        #par(mfrow=c(1,1))
    
    })            

}
