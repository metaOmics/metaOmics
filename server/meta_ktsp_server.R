meta_ktsp_server <- function(input, output,session) {
    library(doMC)
    registerDoMC(2)
    library(MetaKTSP)

    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)

    labels <- NULL
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
            labels <- DB$active@clinicals
            labelLevels <- levels(as.factor( unlist(DB$active@clinicals[[1]]) ))
            updateSelectizeInput(session, "twoLabels", label="Please select only two labels to cluster", choices = labelLevels, server = TRUE)
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
        wait(session, "Running meta KTSP. This may take a while")
        try({
            validate()

            if(length(input$twoLabels)!=2){
                stop("Two and only two labels should be selected for clustering")
            }
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,t)
            labels <- DB$active@clinicals
     
            selectedIdx <- list()
            DList <- list()
            for(i in 1:length(DB$active@datasets)){
                selectedIdx[[i]] <- which(labels[[i]] %in% input$twoLabels)
                DList[[i]] <- DB$active@datasets[[i]][,selectedIdx[[i]]]
                colnames(DList[[i]]) <- labels[[i]][selectedIdx[[i]]]
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[[1]]] <- "0"
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[[2]]] <- "1" 
            }

            DBind <- DList[[1]]
            sampleNameBind <- colnames(DB$active@datasets[[1]])[selectedIdx[[1]]]
            labelBind <- colnames(DList[[1]])

            for (i in 2:length(DB$active@datasets)){
                DBind <- cbind(DBind, DList[[i]])
                sampleNameBind <- c(sampleNameBind, colnames(DB$active@datasets[[i]])[selectedIdx[[i]]])
                labelBind <- c(labelBind, colnames(DList[[i]]))
            }
            
            colnames(DBind) <- labelBind

            test.dat <- DBind #testing data
            test.grp <- rownames(t(test.dat)) #testing data labels

            result <- list()
            if(input$methods=="Mean score"){
                tspobj <- MetaTSP.mean(DList = DList, K = input$kMax, is.VO=input$vo)      
            } else{
                tspobj <- MetaTSP.mean(DList = DList,Method =input$methods, K = input$kMax, is.VO=input$vo)    
            }

            K <- dim(tspobj$model)[1]
            pred.rule <- meta.mul.rule.predict(test.dat = test.dat, tspobj = tspobj, K = K, is.youden=TRUE)
            result$meta.ktsp.mean_VO <- pred.rule$youden

            labelBind[labelBind =="0" ] <- input$twoLabels[[1]]
            labelBind[labelBind == "1"] <- input$twoLabels[[2]] 
            pred.rule$mul.rule[pred.rule$mul.rule =="0" ] <- input$twoLabels[[1]]
            pred.rule$mul.rule[pred.rule$mul.rule == "1"] <- input$twoLabels[[2]]
            
            finalResult <- cbind(sampleNameBind,labelBind, pred.rule$mul.rule)
            colnames(finalResult) <- c("ID","Original Label","Predicted Label")
            write.csv(finalResult, paste(DB.load.working.dir(db), "/metaKTSP/Class_label_method_", input$methods, "_labels_",input$twoLabels[1],"_",input$twoLabels[2],".csv",sep="" ),quote=F,row.names=FALSE)

            output$diffTable <- renderTable({
                difftable <- as.matrix(finalResult)[which(as.matrix(finalResult)[,2]!=as.matrix(finalResult)[,3]),]
                return(difftable)    
            })            
            
            sendSuccessMessage(session, paste("Meta KTSP completed. Predicted cluster labels are save under ", DB.load.working.dir(db),"/metaKTSP",sep=""))
            
        }, session)
        done(session)
        })
    
}
