meta_ktsp_server <- function(input, output,session) {
    library(doMC)
    registerDoMC(3)
    library(MetaKTSP)

    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)

    validate <- function(){
        study <- DB.load.active(db)
        if(is.null(study)) stop(MSG.no.active)
        if(DB.load.working.dir(db)=="") stop(MSG.no.working.dir)
    } 
    updateSelectizeInput(session, "twoLabels", label="Please select only two labels to cluster", choices = NULL, server = TRUE)
    
    observeEvent(input$tabChange, {
        try({
            validate()
            study <- DB.load.active(db)
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,t)
            labels  <- DB$active@clinicals[[1]][,clinical.options[1]]
            labels <- levels(as.factor(labels))
            updateSelectizeInput(session, "twoLabels", label="Please select only two labels to cluster", choices = labels, server = TRUE)
        }, session)
        dir.create(paste(DB.load.working.dir(db),"metaKTSP",sep="/"))
    
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


    observeEvent(input$ktspGo, {
        wait(session, "Running meta KTSP")
        try({
            validate()
    
            test.dat <- DB$active[[1]] #testing data
            test.grp <- rownames(t(test.dat)) #testing data labels
            if(input$methods=="Mean score"){
                tspobj <- MetaTSP.mean(DList = DB$active, K = input$kMax, is.VO=input$vo)      
            } else{
                tspobj <- MetaTSP.mean(DList = DB$active,Method =input$methods, K = input$kMax, is.VO=input$vo)    
            }
            K <- dim(tspobj$model)[1]
            pred.rule <- meta.mul.rule.predict(test.dat = test.dat, tspobj = tspobj, K = K, is.youden=TRUE)
            result$meta.ktsp.mean_VO <- pred.rule$youden
            
            sendSuccessMessage(session, "Meta KTSP completed.")
        
        }, session)
        done(session)
        })
    
}
