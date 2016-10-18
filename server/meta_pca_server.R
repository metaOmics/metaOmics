meta_pca_server <- function(input, output,session) {
    library(metaPCA)
    library(doMC)
    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)

    validate <- function(){
        study <- DB.load.active(db)
        if(is.null(study)) stop(MSG.no.active)
        if(DB.load.working.dir(db)=="") stop(MSG.no.working.dir)    
    }
    optimalLambda <- 6
    
    observeEvent(input$tabChange, {
        try({
            validate()
            study <- DB.load.active(db)
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,function(x) t(scale(t(x))))
 #           print(colnames(DB$transpose[[1]]))
 #           print(levels(as.factor(DB$active@clinicals[[1]])))
 #           print(names(DB$transpose))
        }, session)
        dir.create(paste(DB.load.working.dir(db),"metaPCA",sep="/"))
    })

    output$summaryTable <- renderTable({
        if(!is.null(DB$active)){
            table <- matrix(NA, length(DB$active@datasets), 2 )
            colnames(table) <- c("#Genes","#Samples")
            rownames(table) <- names(DB$transpose)
            for (i in 1:length(DB$transpose)){
                table[i,2] <- dim(DB$transpose[[i]])[2]
                table[i,1] <- dim(DB$transpose[[i]])[1]   
            }
            return(table)
        }
    })

    observeEvent(input$tuneGo, {
        
        wait(session, "Searching the optimal tuning parameter based on the proportion of increased explained variance. This may take a while...")
        try({
            validate()
            sequence = seq(input$min,input$max,input$step)
            var.tmp <-foreach(i= sequence,.combine=rbind) %do% {
                res6 <- meta.pca(DList=DB$transpose, method=input$methods,
                                 Meta.Dim=input$dim, is.auto.Dim = input$dimAuto,
                                 is.sparse=TRUE, Lambda=i)
                
                non.zero <- sum(res6$v != 0)
                c(sum(foreach(ii = 1: length(res6$coord),.combine=c) %do% {
                    sum(diag(var(res6$coord[[ii]])))
                }), non.zero)
            }
            num.nonzero <- var.tmp[,2]
            var.tmp <- var.tmp[,1]
                                        # plot(var.tmp ~ num.nonzero, type='o',ylab="Explained Variance",xlab="The number of non-penalized features",cex.lab=2,lwd=2,cex.axis=1.5)
            scree <-foreach(i= sequence,.combine=c) %do% {
                (var.tmp[i+1] - var.tmp[i]) / var.tmp[i+1]
            }
            png(paste(DB.load.working.dir(db),"/metaPCA/metaPCA_tuning_lambda.png",sep=""))  
            par(mfrow=c(1,1), mar=c(5, 5, 2, 4))
            plot(na.omit(scree) ~ num.nonzero[1:length(na.omit(scree))], type='o',ylab="Proportion of Increased Explained Variance",xlab="The number of non-penalized features",cex.lab=2,lwd=2,cex.axis=1.5)
            abline(h=0.1,col='red',lwd=2)
            dev.off()

            output$tuningPlot <- renderPlot({
                plot(na.omit(scree) ~ num.nonzero[1:length(na.omit(scree))], type='o',ylab="Proportion of Increased Explained Variance",xlab="The number of non-penalized features",cex.lab=2,lwd=2,cex.axis=1.5)
                abline(h=0.1,col='red',lwd=2)            
            })
            optimalLambda <-  sequence[sum(scree> 0.1, na.rm=TRUE)]

            updateNumericInput(session, "lambda", value=optimalLambda)
            
            sendSuccessMessage(session, paste("Sparsity parameter tuned and updated. Recommended lambda is ", optimalLambda,sep=""))
        },session)
        done(session)
    })            

    observeEvent(input$pcaGo, {        
        wait(session, "Running Meta PCA.")
        try({
            validate()
            if (input$sparse==FALSE){
                res6 <- meta.pca(DList=DB$transpose, method=input$methods,
                                 Meta.Dim=input$dim, is.auto.Dim = input$dimAuto,
                                 is.sparse=input$sparse)
            } else {
                res6 <- meta.pca(DList=DB$transpose, method=input$methods,
                                 Meta.Dim=input$dim, is.auto.Dim = input$dimAuto,
                                 is.sparse=input$sparse, Lambda=input$lambda)
            }

            coord <- res6$coord
            
            if( (input$dimAuto==TRUE)&(input$sparse==FALSE))
                png(paste(DB.load.working.dir(db),"/metaPCA/metaPCA_results_dim",input$dim,input$methods,"_autoDim",".png",sep=""))
            if( (input$dimAuto==FALSE)&(input$sparse==FALSE))
                png(paste(DB.load.working.dir(db),"/metaPCA/metaPCA_results_dim",input$dim,input$methods,".png",sep=""))
            if( (input$dimAuto==FALSE)&(input$sparse==TRUE))
                png(paste(DB.load.working.dir(db),"/metaPCA/metaPCA_results_dim",input$dim,input$methods,"_sparse","_lambda",input$lambda,".png",sep=""))
            if( (input$dimAuto==TRUE)&(input$sparse==TRUE))
                png(paste(DB.load.working.dir(db),"/metaPCA/metaPCA_results_dim",input$dim,input$methods,"autoDim_sparse","_lambda",input$lambda,".png",sep=""))
            
            par(mfrow=c(2,2))
            label <- DB$active@clinicals
            for(i in 1:length(coord)){
                acoord <- coord[[i]]
                alabel <- as.factor(unlist(label[[i]]))
                #print(alabel)
                plot(acoord[,1], acoord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n"
                    ,ylim=c(min(acoord[,2])-1.5, max(acoord[,2])+1.5)
                    ,xlim=c(min(acoord[,1])-1, max(acoord[,1])+1),main=names(DB$transpose)[i])
                points(acoord[,1], acoord[,2], col=as.numeric(alabel), cex=1)
                legend('topright',legend=levels(alabel),col=unique(as.numeric(alabel)),pch=1)
            }
            dev.off()
            sendSuccessMessage(session, "Meta PCA complete.")
        },session)
        done(session)

        
        wait(session,"Visualizing results.")
        try({
            validate()
            output$plots <- renderUI({
                plot_output_list <- lapply(1:length(coord), function(i) {
                    plotname <- paste("meta_pca-plot", i, sep="")
                    #plotOutput(plotname, height = 280, width = 350)
                    
                     tags$li(class="DocumentItem",
                            plotOutput(plotname, height = 480, width = 450)
                    )
                })
                
                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
                #do.call(tagList, plot_output_list)
                tags$div(class="DocumentList",
                         tags$ul(class="list-inline",
                                 plot_output_list
                         )
                )
            })
            
            for (i in 1:length(coord)) {
                                        # Need local so that each item gets its own number. Without it, the value
                                        # of i in the renderPlot() will be the same across all instances, because
                                        # of when the expression is evaluated.
                local({
                    my_i <- i
                    plotname <- paste("plot", my_i, sep="")
                    
                    output[[plotname]] <- renderPlot({
                        acoord <- coord[[my_i]]
                        alabel <- as.factor(unlist(label[[my_i]]))
                        plot(acoord[,1], acoord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n"
                            ,ylim=c(min(acoord[,2])-1.5, max(acoord[,2])+1.5)
                            ,xlim=c(min(acoord[,1])-1, max(acoord[,1])+1),main=names(DB$transpose)[my_i])
                        points(acoord[,1], acoord[,2], col=as.numeric(alabel), cex=1)
                        legend('topright',legend=levels(alabel),col=unique(as.numeric(alabel)),pch=1)
                    })
                })
            }
            
            sendSuccessMessage(session, "Visualization complete.")
        },session)
        done(session)    
    })            

}
