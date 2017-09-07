best <- function(state, outcome) {
        outcomedt <- read.csv("~/R And GITHUB AND EVERYTHING/R/Projects/R programming/Hospital/outcome-of-care-measures.csv")
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        outcomecheck <- intersect(outcome, outcomelist)
        stateabbreviation <- state.abb
        stateabbreviations <- c(stateabbreviation, c("DC", "PR", "VI", "GU"))
                if (length(outcomecheck) == 0) {
                        stop("invalid outcome")
                }
        statecheck <- intersect(state, stateabbreviations)
                if (length(statecheck) == 0){
                       stop("invalid state")
                }
        outcomealls <- outcomedt[, 7]
        hospitalnames <- outcomedt[, 2]
        hospitalnamedt <- as.data.frame(hospitalnames)
        outcomeallstated <- as.data.frame(outcomealls)
        outcomeallstates <- cbind.data.frame(outcomeallstated, hospitalnamedt)
        outcomeallstate <- data.frame(Date=as.Date(character()),
                                      File=character(), 
                                      User=character(), 
                                      stringsAsFactors=FALSE) 
                if (outcome == "heart attack"){
                heartaallstates <- outcomedt[, 11]
                haallstates <- as.data.frame(heartaallstates)
                hadata <- cbind.data.frame(outcomeallstates, haallstates)
                outcomeallstate <- hadata 
                }
                if (outcome == "heart failure"){
                heartfallstates <- outcomedt[, 17] 
                hfallstates <- as.data.frame(heartfallstates)
                hfdata <- cbind.data.frame(outcomeallstates, hfallstates)
                outcomeallstate <- hfdata
                }
                if (outcome == "pneumonia"){
                pneuallstates <- outcomedt [, 23] 
                pnallstates <- as.data.frame(pneuallstates)
                pndata <- cbind.data.frame(outcomeallstates, pnallstates)
                outcomeallstate <- pndata
                }
        
        colnames(outcomeallstate) <- c("state", "hospital name", "data")
       statedlist <- outcomeallstate[outcomeallstate$"state" == state,]
      almostdonelist <- statedlist[statedlist$"data" != "Not Available",]
       almostdonelist$"data" <- as.numeric(as.character(almostdonelist$"data"))
     orderedlist <- almostdonelist[order(almostdonelist$"data"),]
     datamin <- min(orderedlist[, 3])
     topmaybetied <- orderedlist[orderedlist$"data" == datamin,]
     inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
     print(paste(inaorder[[1,2]]))
}

rankhospital <- function(state, outcome, num = "best") {
        
        if (num == "best"){
                bestname <- best(state, outcome)
                bestname
        }
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        outcomecheck <- intersect(outcome, outcomelist)
        if (length(outcomecheck) == 0) {
                stop("invalid outcome")
        }
        stateabbreviation <- state.abb
        stateabbreviations <- c(stateabbreviation, c("DC", "PR", "VI", "GU"))
        statecheck <- intersect(state, stateabbreviations)
        if (length(statecheck) == 0){
                stop("invalid state")
        }
        outcomedt <- read.csv("~/R And GITHUB AND EVERYTHING/R/Projects/R programming/Hospital/outcome-of-care-measures.csv")
        outcomealls <- outcomedt[, 7]
        hospitalnames <- outcomedt[, 2]
        hospitalnamedt <- as.data.frame(hospitalnames)
        outcomeallstated <- as.data.frame(outcomealls)
        outcomeallstates <- cbind.data.frame(outcomeallstated, hospitalnamedt)
        outcomeallstate <- data.frame(Date=as.Date(character()),
                                      File=character(), 
                                      User=character(), 
                                      stringsAsFactors=FALSE) 
        if (outcome == "heart attack"){
                heartaallstates <- outcomedt[, 11]
                haallstates <- as.data.frame(heartaallstates)
                hadata <- cbind.data.frame(outcomeallstates, haallstates)
                outcomeallstate <- hadata 
        }
        if (outcome == "heart failure"){
                heartfallstates <- outcomedt[, 17] 
                hfallstates <- as.data.frame(heartfallstates)
                hfdata <- cbind.data.frame(outcomeallstates, hfallstates)
                outcomeallstate <- hfdata
        }
        if (outcome == "pneumonia"){
                pneuallstates <- outcomedt [, 23] 
                pnallstates <- as.data.frame(pneuallstates)
                pndata <- cbind.data.frame(outcomeallstates, pnallstates)
                outcomeallstate <- pndata
        }
        
        colnames(outcomeallstate) <- c("state", "hospital name", "data")
        statedlist <- outcomeallstate[outcomeallstate$"state" == state,]
        almostdonelist <- statedlist[statedlist$"data" != "Not Available",]
        almostdonelist$"data" <- as.numeric(as.character(almostdonelist$"data"))
        orderedlist <- almostdonelist[order(almostdonelist$"data"),]
        
        if (num == "worst") {
                len <- nrow(orderedlist)
                dataval <- orderedlist[[len, 3]]
                topmaybetied <- orderedlist[orderedlist$"data" == dataval,]
                inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                print(paste(inaorder[nrow(inaorder), 2]))
        }
        if (class(num) == "numeric") {
                if (nrow(statedlist) < num) {
                        return(NA)
                }
                dataval <- orderedlist[[num, 3]]
                testtosee <- orderedlist[orderedlist$"data" == dataval,]
                
                if (nrow(testtosee) != 1){
                        numberlessthan <- orderedlist[orderedlist$"data" < dataval,]
                        below <- nrow(numberlessthan)
                        endnum <- num - below
                        topmaybetied <- orderedlist[orderedlist$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        print(paste(inaorder[endnum, 2]))
                }  else {
                        dataval <- orderedlist[[num, 3]]   
                        topmaybetied <- orderedlist[orderedlist$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        print(paste(inaorder[1, 2]))
                }     
        }
}

rankall <- function(outcome, num = "best") {
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        outcomecheck <- intersect(outcome, outcomelist)
        if (length(outcomecheck) == 0) {
                stop("invalid outcome")
        }
        
        outcomedt <- read.csv("~/R And GITHUB AND EVERYTHING/R/Projects/R programming/Hospital/outcome-of-care-measures.csv")
        outcomealls <- outcomedt[, 7]
        hospitalnames <- outcomedt[, 2]
        hospitalnamedt <- as.data.frame(hospitalnames)
        outcomeallstated <- as.data.frame(outcomealls)
        outcomeallstates <- cbind.data.frame(outcomeallstated, hospitalnamedt)
        outcomeallstate <- data.frame(Date=as.Date(character()),
                                      File=character(), 
                                      User=character(), 
                                      stringsAsFactors=FALSE) 
        if (outcome == "heart attack"){
                heartaallstates <- outcomedt[, 11]
                haallstates <- as.data.frame(heartaallstates)
                hadata <- cbind.data.frame(outcomeallstates, haallstates)
                hadata <- hadata[hadata$"heartaallstates" != "Not Available",]
                hadata$"heartaallstates" <- as.numeric(as.character(hadata$"heartaallstates"))
                outcomeallstate <- hadata 
                
        }
        if (outcome == "heart failure"){
                heartfallstates <- outcomedt[, 17] 
                hfallstates <- as.data.frame(heartfallstates)
                hfdata <- cbind.data.frame(outcomeallstates, hfallstates)
                hfdata <- hfdata[hfdata$"heartfallstates" != "Not Available",]
                hfdata$"heartfallstates" <- as.numeric(as.character(hfdata$"heartfallstates"))
                outcomeallstate <- hfdata
                
        }
        if (outcome == "pneumonia"){
                pneuallstates <- outcomedt [, 23] 
                pnallstates <- as.data.frame(pneuallstates)
                pndata <- cbind.data.frame(outcomeallstates, pnallstates)
                pndata <- pndata[pndata$"pneuallstates" != "Not Available",]
               pndata$"pneuallstates" <- as.numeric(as.character(pndata$"pneuallstates"))
                
                outcomeallstate <- pndata
                
        }
        colnames(outcomeallstate) <- c("state", "hospital name", "data")
        almostdonelist <- outcomeallstate[outcomeallstate$"data" != "Not Available",]
        q <- almostdonelist[order(almostdonelist[,1], almostdonelist[,3]),]
        maybedone <- split.data.frame(q, q$"state")
        
        
        one <- function(x){
               
                if (nrow(x) < num) {
                return(NA)
                 }
                dataval <- x[num, 3]
                print(class(num))
                testtosee <- x[x$"data" == dataval,]
                if(class(num) == "numeric") {
                 if (nrow(testtosee) != 1){
                        numberlessthan <- x[x$"data" < dataval,]
                        below <- nrow(numberlessthan)
                        endnum <- num - below
                        topmaybetied <- x[x$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        hospital <- (inaorder[endnum, 2])
                        hope <- as.character(hospital)
                        return(hope)
                        
                }  else {
                        dataval <- x[num, 3]   
                        topmaybetied <- x[x$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        hospital <- (inaorder[1, 2])
                        hope <- as.character(hospital)
                        return(hope)
        }     }
        }
        two <- function(x){
                
                if (num == "best"){
                        dataval <- x[1, 3]   
                        topmaybetied <- x[x$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        hospital <- (inaorder[1, 2])
                        hope <- as.character(hospital)
                        return(hope)
                }
        }
        three <- function(x){
                
                if (num == "worst"){
                        len <- nrow(x)
                        dataval <- x[[len, 3]]
                        topmaybetied <- x[x$"data" == dataval,]
                        inaorder <- topmaybetied[order(topmaybetied$"hospital name"),]
                        hospital <- (inaorder[nrow(inaorder), 2])
                        hope <- as.character(hospital)
                        return(hope)
                }
        }
        end <- list(one = one, two = two, three = three)
        
        closertodone <- sapply(end, mapply, maybedone)
        closertodone
        
        
        
        
}    
        
        

        
        
        
        
        
        
        
        
        
        
        
        

        