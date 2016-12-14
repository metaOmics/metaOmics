meta_ktsp_server <- function(input, output,session) {
    library(doMC)
    #registerDoMC(2)
    library(MetaKTSP)

    study <- DB.load.active(db)
    DB <- reactiveValues(acitve=NULL,transpose=NULL)

    labels <- NULL
    labelEmpty <- NULL
    validate <- function(){
        study <- DB.load.active(db)
        if(is.null(study)) stop(MSG.no.active)
        if(DB.load.working.dir(db)=="") stop(MSG.no.working.dir)
        if(length(intersect(input$trainStudy,input$testStudy))!=0) stop("Test and training studies cannot be the same.")
        
    } 
    updateSelectizeInput(session, "twoLabels", label="Please select TWO labels to cluster", choices = NULL, server = TRUE)
    updateSelectizeInput(session, "trainStudy", label = "Please select studies for training", choices = NULL, server = TRUE)
    updateSelectizeInput(session, "testStudy", label = "Please select ONE study for testing", choices = NULL, server = TRUE)    

    observeEvent(input$tabChange, {
        try({
            validate()
            study <- DB.load.active(db)
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,t)
            labels <- DB$active@clinicals
                                        #          labelLevels <- levels(as.factor( unlist(DB$active@clinicals[[1]]) ))
            for(i in 1:length(DB$transpose)){
                if( length(DB$active@clinicals[[1]])!=0 )
                    labelLevels <- levels(as.factor( unlist(DB$active@clinicals[[i]]) ))
            }
            updateSelectizeInput(session, "twoLabels", label="Please select TWO labels to cluster", choices = labelLevels, server = TRUE)
            updateSelectizeInput(session, "trainStudy", label = "Please select studies for training", choices = names(DB$transpose), server = TRUE)
            updateSelectizeInput(session, "testStudy", label = "Please select ONE study for testing", choices = names(DB$transpose), server = TRUE)    
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

    trainedFlag <- 0
    Dtest <- NULL
    tspobj <- NULL
    trainIdx <- NULL
    testIdx <- NULL
    labelEmpty <- NULL
    DList <- list()
    selectedIdx <- list()
    observeEvent(input$trainStudy, {
        registerDoMC(cores=input$core)
          updateSelectizeInput(session, "testStudy", label = "Please select ONE study for testing", choices = setdiff(names(DB$transpose),input$trainStudy), server = TRUE)   
      })

    observeEvent(input$ktspTrain, {
        wait(session, "Training meta KTSP model. This may take a while")
        try({
            validate()

            if(length(input$twoLabels)!=2){
                stop("Two and only two labels should be selected for clustering")
            }
            if(length(input$testStudy)!=1){
                stop("One and only one testing study should be selected at a time.")
            }
            DB$active <- DB.load.active(db)
            DB$transpose <- lapply(DB$active@datasets,t)
            labels <- DB$active@clinicals

            labelEmpty <- rep(1,length(DB$transpose))
            for(i in 1:length(DB$transpose)){
                if( length(DB$active@clinicals[[1]])!=0 )
                    labelEmpty[i] <- 0
            }
            
            for(i in 1:length(DB$active@datasets)){
                if(labelEmpty[i]==0){
                selectedIdx[[i]] <- which(unlist(labels[[i]]) %in% input$twoLabels)
                DList[[i]] <- DB$active@datasets[[i]][,selectedIdx[[i]]]
                colnames(DList[[i]]) <- unlist(labels[[i]])[selectedIdx[[i]]]
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[1]] <- "0"
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[2]] <- "1"
            }
                if(labelEmpty[i]==1){
                    DList[[i]] <- DB$active@datasets[[i]]
                    selectedIdx[[i]] <- 1:(dim(DList[[i]])[2])
                colnames(DList[[i]]) <- rep("-9",dim(DList[[i]])[2])
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[1]] <- "0"
                colnames(DList[[i]])[colnames(DList[[i]]) == input$twoLabels[2]] <- "1"
                }
            }

            trainIdx <- which( names(DB$active@datasets) %in% input$trainStudy )
            testIdx <- which( names(DB$active@datasets) %in% input$testStudy )
            
            if(sum(labelEmpty[trainIdx])!=0){
                stop("Not all training studies have label. Please go back and check")
            }

            Dtrain <- DList[trainIdx]

            if(input$methods=="Mean score"){
                tspobj <- MetaTSP.mean(DList = Dtrain, K = input$kMax)
            } else{
                tspobj <- MetaTSP.Pvalue(DList = Dtrain, Method =input$methods, K = input$kMax)    
            }
            K <- dim(tspobj$model)[1]

            updateNumericInput(session, "K", label= "Number of top scoring pairs (K)",value=K)

            output$genePairTable <- renderTable({
                return(tspobj$gene.pair.table)    
            })
#            output$genePairTable2 <- renderTable({
#                return(tspobj$model)    
#            })
            

            output$voPlot <- renderPlot({
                plot(2:input$kMax, tspobj$VO,type="l",xlab="K",ylab="VO")
                tmpInterval <- max(tspobj$VO) - min(tspobj$VO)
                arrows(x0=K, y0= min(tspobj$VO) + 0.75*tmpInterval, y1=min(tspobj$VO) + 0.95*tmpInterval, code=2,  length=.2,lwd=3,col=colors()[472]) 
            })
            trainedFlag<-1
            
            sendSuccessMessage(session, paste("Meta KTSP model trained. Recommended K updated to be", K, sep=""))


                observeEvent(input$ktspTest, {
        registerDoMC(cores=input$core)
        wait(session, "Running meta KTSP prediction")
        try({
            validate()
            
            if(trainedFlag!=1){
                stop("We need to train a model first.")
            }
            if(length(input$twoLabels)!=2){
                stop("Two and only two labels should be selected for clustering")
            }
            if(length(input$testStudy)!=1){
                stop("One and only one testing study should be selected at a time.")
            }
            
            sampleNameBind <- colnames(DB$active@datasets[[testIdx]])[selectedIdx[[testIdx]]]
            labelBind <- colnames(DList[[testIdx]])


            ## DBind <- DList[[1]]
            ## sampleNameBind <- colnames(DB$active@datasets[[1]])[selectedIdx[[1]]]
            ## labelBind <- colnames(DList[[1]])

            ## for (i in 2:length(DB$active@datasets)){
            ##     DBind <- cbind(DBind, DList[[i]])
            ##     sampleNameBind <- c(sampleNameBind, colnames(DB$active@datasets[[i]])[selectedIdx[[i]]])
            ##     labelBind <- c(labelBind, colnames(DList[[i]]))
            ## }
            Dtest <- DList[[testIdx]]
            
#            colnames(DBind) <- labelBind

#            test.dat <- DBind #testing data
            test.grp <- rownames(t(Dtest)) #testing data labels

            result <- list()

            pred.rule <- meta.mul.rule.predict(test.dat=Dtest, tspobj=tspobj, K=input$K, is.youden=TRUE)

            ##pred.rule <- meta.mul.rule.predict(test.dat = test.dat, tspobj = tspobj, K = K, is.youden=TRUE)
#            result$meta.ktsp.mean_VO <- pred.rule$youden

            labelBind[labelBind =="0" ] <- input$twoLabels[1]
            labelBind[labelBind == "1"] <- input$twoLabels[2]
            labelBind[labelBind == "-9"] <- "No original label"            
            pred.rule$mul.rule[pred.rule$mul.rule =="0" ] <- input$twoLabels[1]
            pred.rule$mul.rule[pred.rule$mul.rule == "1"] <- input$twoLabels[2]

            finalResult <- cbind(sampleNameBind,labelBind, pred.rule$mul.rule)
            colnames(finalResult) <- c("ID","Original Label","Predicted Label")
            write.csv(finalResult, paste(DB.load.working.dir(db), "/metaKTSP/Class_label_method_", input$methods, "_labels_",input$twoLabels[1],"_",input$twoLabels[2],".csv",sep="" ),quote=F,row.names=FALSE)

            if(labelEmpty[testIdx]==1){
                output$confusionTitle <- renderText({""})
                output$confusionTable <- renderTable({})
            } else {
                output$confusionTitle <- renderText({"Confusion Table"})
                output$confusionTable <- renderTable({
                    cells<-matrix(NA,2,2)
                    colnames(cells) <- paste("Original",input$twoLabels,sep="_")
                    rownames(cells) <- paste("Predicted",input$twoLabels,sep="_")
                    finalResult <- as.matrix(finalResult)
                    cells[1,1] <- length(which( (finalResult[,2]==input$twoLabels[1]) & (finalResult[,3]==input$twoLabels[1]) ))
                    cells[1,2] <- length(which( (finalResult[,2]==input$twoLabels[2]) & (finalResult[,3]==input$twoLabels[1]) ))
                    cells[2,1] <- length(which( (finalResult[,2]==input$twoLabels[1]) & (finalResult[,3]==input$twoLabels[2]) ))
                    cells[2,2] <- length(which( (finalResult[,2]==input$twoLabels[2]) & (finalResult[,3]==input$twoLabels[2]) ))
                return(cells)    
            })            
       }
            sendSuccessMessage(session, paste("Meta KTSP prediction completed. Predicted cluster labels are save under ", DB.load.working.dir(db),"/metaKTSP",sep=""))
        }, session)
        done(session)
        })
            
        }, session)
        done(session)
    })

    

    
}
