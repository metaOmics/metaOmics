meta_pca_server <- function(input, output,session) {
    library(metaPCA)
    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)



    validate <- function(){
        study <- DB.load.active(db)
        if(is.null(study)) stop(MSG.no.active)
        if(DB.load.working.dir(db)=="") stop(MSG.no.working.dir)    
    }
    
    observeEvent(input$tabChange, {
        try({
            validate()
            study <- DB.load.active(db)
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,function(x) t(scale(t(x))))
        }, session)
        dir.create(paste(DB.load.working.dir(db),"metaPCA",sep="/"))
    })


    observeEvent(input$pcaGo, {
        
        wait(session, "Running Meta PCA. Go get a coffee.")
        try({
            validate()
            if (is.null(input$lambda))
                res6 <- meta.pca(DList=DB$transpose, method=input$methods, Meta.Dim=input$dim, is.auto.Dim = input$dimAuto, #as.logical
                                 is.sparse=linput$sparse)
            else
                res6 <- meta.pca(DList=DB$transpose, method=input$methods, Meta.Dim=input$dim, is.auto.Dim = input$dimAuto,
                                 is.sparse=linput$sparse, Lambda=input$lambda)
            
            coord <- res6$coord

            png(paste(input$outDir,'/metaPCA_results.png',sep=""))
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
        },session)
        done(session)
        sendSuccessMessage(session, "Meta PCA complete.")
            
        wait(session,"Visualizing results.")
        try({
            validate()
  
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
                            ,xlim=c(min(acoord[,1])-1, max(acoord[,1])+1),main=names(DB$transpose)[my_i])
                        points(acoord[,1], acoord[,2], col=as.numeric(alabel), cex=1)
                        legend('topright',legend=levels(alabel),col=unique(as.numeric(alabel)),pch=1)
                    })
                })
            }
            
            done(session)
            
            sendSuccessMessage(session, "Visualization complete.")
        },session)
    })            

}
