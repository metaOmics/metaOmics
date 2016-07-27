meta_clust_server <- function(input, output,session) {
    s <- DB.load.active(db)
    if (is.null(s))
        s <- "No active study"
    else
        datasets <- s@datasets
                                        #k=input$k,wbounds=input$wBounds,lambda=input$lambda
    runClust <- reactiveValues(data = NULL)

    observeEvent(input$clustGo, {
        runClust <- TRUE
    })

    output$heatmap <- renderPlot({
        if (is.null(runClust)) return()
        res <- MetaSpaKmeans(x=datasets,K=input$k,wbounds=input$wBounds,lambda=input$lambda)
#        getWsHeatmap(t(S[[1]]),res$Cs[[1]],res$ws,main="two study after metaSparseKMeans, S1")
#        getWsHeatmap(t(S[[2]]),res$Cs[[2]],res$ws,main="two study after metaSparseKMeans, S2")
        plot(res$ws,main="metaSparseKmeans weight dist",xlab="geneIndex")
    })
}
