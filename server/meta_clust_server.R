meta_clust_server <- function(input, output,session) {
    library(MetaSparseKmeans)
    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)
    
    validate <- function(){
        study <- DB.load.active(db)
        if(is.null(study)) stop(MSG.no.active)
    }

    updateSelectizeInput(session, "studyforK", label="Select studies to be tuned", choices = NULL, server = TRUE)  
    observeEvent(input$tabChange, {
        try({
            validate()
            DB <- reactiveValues(acitve=study,transpose=lapply(study@datasets,t))

            dir.create(paste(DB.load.working.dir(db),"metaClust",sep="/"))
            
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,t)
            
            updateSelectizeInput(session, "studyforK", label="Select studies to be tuned", choices = names(DB$active@datasets), server = TRUE)
        }, session)
    })
    
    output$plotsK <- renderUI({
        if (!is.null(DB$active)){
            plot_output_list <- lapply(1:length(DB$active@datasets), function(i) {
                plotname <- paste("meta_clust-plotsK", i, sep="")
                tags$li(class="DocumentItem",
                        plotOutput(plotname, height = 280, width = 350)
                        )
            })

            tags$div(class="DocumentList",
                     tags$ul(class="list-inline",
                             plot_output_list
                             )
                     )
        }
    })
    

    output$summaryTable <- renderTable({
        if(!is.null(DB$active)){
            table <- matrix(NA, length(DB$active@datasets), 2 )
            colnames(table) <- c("#Genes","#Samples")
            rownames(table) <- names(DB$transpose)
            for (i in 1:length(DB$transpose)){
                table[i,2] <- dim(DB$transpose[[i]])[1]
                table[i,1] <- dim(DB$transpose[[i]])[2]
            }
            return(table)
        }
    })

    tuneIndStudyK <- function(aS, topPercent=0.1, B = 100, seed=15213,maxK=8, verbose=FALSE){
        ## aS: a study, n*p, sample*gene
        ## topPercent: top percentage of genes with the largest variance
        ## B: number of permutation
        ## seed: random seed
        ## verbose: whether print out intermediate result
        varS <- apply(aS,2,var)
        bS <- aS[,varS > quantile(varS,1-topPercent)]
        set.seed(seed)
        gskmn <- clusGap(bS, FUN = kmeans, nstart = 20, K.max = maxK, B = B, verbose=verbose)
        return(gskmn)
    }

    observeEvent(input$tuneK, {
        wait(session, "Tuning number of clusters")
        try({
            validate()
            studyIdx <- which(names(DB$active@datasets)%in%input$studyforK)
            print(studyIdx)

            gskmn <- list()

            for(i in 1:length(studyIdx)){	
                gskmn[[studyIdx[i]]] <- tuneIndStudyK(DB$transpose[[studyIdx[i]]], topPercent=input$topPerc, B = input$BforK, maxK=input$maxK, verbose=TRUE)
                png(paste( DB.load.working.dir(db),"/metaClust/gskmn",studyIdx[i],".png",sep=""))
                plot(gskmn[[studyIdx[i]]], main = paste('gap statistics for ',names(DB$transpose)[studyIdx[i]],sep=''))
                dev.off()
            }


            
            for (i in 1:length(studyIdx)) {
                                        # Need local so that each item gets its own number. Without it, the value
                                        # of i in the renderPlot() will be the same across all instances, because
                                        # of when the expression is evaluated.
                local({
                    my_i <- studyIdx[i]
                    plotname <- paste("plotsK", my_i, sep="")
                    
                    output[[plotname]] <- renderPlot({
                        plot(gskmn[[my_i]], main = paste('gap statistics for ',names(DB$transpose)[my_i],sep=''))
                    })
                })
            }
            
        sendSuccessMessage(session, "K updated")
        }, session)
        done(session)
        

    })

    observeEvent(input$tuneW, {
        wait(session, "Tuning wbounds for meta sparse k means. Go get a coffee")

        try({
            validate()
            gapStatResult <- calculateGap(DB$transpose,K=input$KforW,wbounds=seq(input$WRange[1],input$WRange[2],by=input$byW),B=input$BforW)
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
        sendSuccessMessage(session, "Wbounds updated")

        }, session)
        
        done(session)

    })

    observe({
        val <- input$KforW
        updateNumericInput(session, "k", value = val)
    })
    
    observe({
        val <- input$byW
        updateSliderInput(session, "WRange",value=c(1,15), min=0,max=30,step=val)
    })

    observeEvent(input$clustGo, {
        runClust <- TRUE
        
        wait(session, "Running meta sparse k means.")
        try({
            validate()
            set.seed(15213)
            res = MetaSpaKmeans(x=DB$transpose,K=input$k,wbounds=input$wBounds, method=input$methods, sampleSizeAdjust=input$sizeAdj)
            
            ## output gene list
            geneList <- res$ws
            write.csv(geneList,paste( DB.load.working.dir(db),"/metaClust/geneList.csv",sep=""))
            
            ## output labels
            for(i in 1:length(DB$transpose)){	
                afileName <- paste( DB.load.working.dir(db),"/metaClust/mskm_label_",names(DB$transpose)[i],".csv",sep='')
                alabel <- res$Cs[[i]]
                names(alabel) <- rownames(DB$transpose[[i]])	
                write.csv(alabel, afileName)
            }

            ## visualization.
            for(i in 1:length(DB$transpose)){	
                afileName <- paste( DB.load.working.dir(db),"/metaClust/heatmap_",names(DB$transpose)[i],".png",sep='')
                
                png(afileName)
                if(i==1){
                    geneOrder = getWsHeatmap(t(DB$transpose[[i]]),res$Cs[[i]],res$ws,main=names(DB$transpose)[i],Rowv=TRUE,labCol="",labRow="")	
                } else {
                    getWsHeatmap(t(DB$transpose[[i]]),res$Cs[[i]],res$ws,main=names(DB$transpose)[i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
                }
                
                dev.off()
            }

            done(session)
            wait(session, "Visualizing sparse K means results...")

            output$heatmaps <- renderUI({
                plot_output_list <- lapply(1:length(DB$transpose), function(i) {
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
            
            for (i in 1:length(DB$transpose)) {
                                        # Need local so that each item gets its own number. Without it, the value
                                        # of i in the renderPlot() will be the same across all instances, because
                                        # of when the expression is evaluated.
                local({
                    my_i <- i
                    plotname <- paste("plot", my_i, sep="")
                    
                    output[[plotname]] <- renderPlot({
                        if(my_i==1){
                            geneOrder = getWsHeatmap(t(DB$transpose[[my_i]]),res$Cs[[my_i]],res$ws,main=names(DB$transpose)[my_i],Rowv=TRUE,labCol="",labRow="")	
                        } else {
                            getWsHeatmap(t(DB$transpose[[my_i]]),res$Cs[[my_i]],res$ws,main=names(DB$transpose)[my_i], Rowv = geneOrder$Rowv,labCol="",labRow="")	
                        }
                        
                    })
                })
            }
        sendSuccessMessage(session, "Meta sparse K means complete. Heatmap saved to output directory")

        }, session)
        done(session)
                                       
        
    })            

}
