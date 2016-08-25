meta_pca_server <- function(input, output,session) {
    s <- DB.load.active(db)
    datasets <- NULL
    if (is.null(s))
        sendErrorMessage(session, "No active study")
    else
        datasets <- s@datasets
    datasets <- lapply(datasets,function(x) t(scale(t(x))))
  #  print( dim(datasets[[1]]) )

    
    observeEvent(input$pcaGo, {
        if (is.null(s))
            s <- "No active study"
        else{
            wait(session, "Running Meta PCA. Go get a coffee.")

            if (is.null(input$lambda))
            res6 <- meta.pca(DList=datasets, method=input$methods, Meta.Dim=input$dim, is.auto.Dim = input$dimAuto, #as.logical
                             is.sparse=linput$sparse)
            else
            res6 <- meta.pca(DList=datasets, method=input$methods, Meta.Dim=input$dim, is.auto.Dim = input$dimAuto,
                             is.sparse=linput$sparse, Lambda=input$lambda)
            
            coord <- res6$coord

            png(paste(input$outDir,'/metaPCA_Leukemia_res6.png',sep=""))
            par(mfrow=c(2,2))
            for(i in 1:length(coord)){
                acoord <- coord[[i]]
                alabel <- as.factor(label[[i]])
                plot(acoord[,1], acoord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n"
                    ,ylim=c(min(acoord[,2])-1.5, max(acoord[,2])+1.5)
                    ,xlim=c(min(acoord[,1])-1, max(acoord[,1])+1),main=names(Leukemia)[i])
                points(acoord[,1], acoord[,2], col=as.numeric(alabel), cex=1)
                legend('topright',legend=levels(alabel),col=unique(as.numeric(alabel)),pch=1)
            
            }
            dev.off()
            done(session)
            
            sendSuccessMessage(session, "Meta PCA complete.")
            
            wait(session,"Visualizing results.")
            output$plots <- renderUI({
                plot_output_list <- lapply(1:length(coord), function(i) {
                    plotname <- paste("meta_pca-plot", i, sep="")
                    plotOutput(plotname, height = 280, width = 350)
                })
                
                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
                do.call(tagList, plot_output_list)
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
                        alabel <- as.factor(label[[my_i]])
                        plot(acoord[,1], acoord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n"
                            ,ylim=c(min(acoord[,2])-1.5, max(acoord[,2])+1.5)
                            ,xlim=c(min(acoord[,1])-1, max(acoord[,1])+1),main=names(datasets)[my_i])
                        points(acoord[,1], acoord[,2], col=as.numeric(alabel), cex=1)
                        legend('topright',legend=levels(alabel),col=unique(as.numeric(alabel)),pch=1)
                    })
                })
            }
            
            done(session)
            
            sendSuccessMessage(session, "Visualization complete.")
        }
    })            

}
