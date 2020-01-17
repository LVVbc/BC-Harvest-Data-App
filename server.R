options(shiny.maxRequestSize=30*1024^2)
options(shiny.error = browser)
library(shiny)
library(DT)
library(readxl)
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggplot2)

smoothFunction <- function(x){
  t <- vector(length = length(x))
  for(s in 1:length(x)){
    if(s == 1) t[s] <- mean(x[s:(s + 1)], na.rm = T)
    if(s > 1 & s < length(x)) t[s] <-  mean(x[(s - 1):(s + 1)], na.rm = )
    if(s == length(x)) t[s] <-  mean(x[(s - 1):s], na.rm = T)
  }
  t
}

# setwd("Z:/ES/WILDLIFE 2016-2020/Harvest management/Harvest data/Data Code/Shiny App")
database <- readRDS("HarvestData2018.rds")

subrlist <- list()
westkoots <- c(407,408,409,414,415,416,417,418,419,427,428,429,430,431,432,433,438,439)
eastkoots <- c(401,402,403,404,405,406,420,421,422,423,424,425,426,434,435,436,437,440)
subrlist[[1]] <- westkoots
subrlist[[2]] <- eastkoots
subr <- c("West Kootenays", "East Kootenays")

#### SERVER ####
server <- function(input, output){
  
  inputs <- reactive({
    species <- input$speciesInput
    if(is.null(species)){
      return()
    }
    specieslist <- c("Black Bear", "Grizzly Bear", "Caribou", "Cougar", "Elk", "Mountain Goat", 
                     "Moose", "Sheep", "Wolf", "Mule Deer", "White-tailed Deer")
    if("All Species" %in% species) species <- specieslist
    if(input$unitsInput == "") units <- NULL
    
    if(input$subregionInput != ""){
      units <- subrlist[[which(subr == input$subregionInput)]]
    }
    if(input$unitsInput != ""){
      units <- input$unitsInput
    }
    
    region <- input$regionInput
    if("All" %in% region) region <- c("1", "2", "3", "4", "5", "6", "7A", "7B", "8")
    
    if(input$unitsInput != "") {
      units <- as.numeric(strsplit(units, split = ",")[[1]])
    }
    if(!is.null(units)) region <- NULL
    arealabel <- input$labelInput
    if(input$subregionInput != "" & input$unitsInput == ""){
      arealabel <- paste("the", input$subregionInput, sep = " ")
    }
    if(arealabel == "") arealabel <- NULL
    smooth <- input$smoothInput
    years <- input$yearsInput[1]:input$yearsInput[2]

    if(is.null(species)){
      return()
    }
    if(is.null(region) & is.null(units)){
      return()
    }
    
    label <- ifelse(!is.null(arealabel), arealabel,
                    {ifelse(!is.null(region),
                            ifelse("All" %in% input$regionInput, "All Regions", 
                                   paste("Region", paste(region, collapse = ", "), sep = " ")),
                            if(!is.null(units)){paste(units, collapse = ", ")})})
    specieslab <- ifelse("All Species" %in% input$speciesInput, input$speciesInput, 
                         paste(species, collapse = ", "))
    inputs <- list()
    inputs[[1]] <- species
    inputs[[2]] <- units
    inputs[[3]] <- region
    inputs[[4]] <- arealabel
    inputs[[5]] <- years
    inputs[[6]] <- label
    inputs[[7]] <- specieslab
    
    inputs
  })
  
  datatable <- reactive({
    if(is.null(inputs())){
      return()
    }
    
    params <- inputs()
    species <- params[[1]]
    units <- params[[2]]
    region <- params[[3]]
    arealabel <- params[[4]]
    years <- params[[5]]
    
    x <- database[database$SPECIES %in% species,]
    x99 <- x[x$WMU %in% c("199", "299", "399", "499", "599", "699", "799.1", "799.2", "899", 
                          "999"),]
    x99 <- x99[!(x99$WMU =="999" & x99$`PROV FLAG` == 0),]
    x <- x[!(x$WMU %in% c("199", "299", "399", "499", "599", "699", "799", "799.1", "799.2",
                          "899", "999")),]
    x <- x[!is.na(x$WMU),]
    x <- x[x$Year %in% years,]
    if(!is.null(units)) x <- x[x$WMU %in% units,]
    if(!is.null(region)) x <- x[x$REGION %in% region,]
    if(is.null(region)) xeffort <- x
    if(!is.null(region)) {
      xeffort <- x99[x99$REGION %in% region & x99$SPECIES %in% species & x99$Year %in% years,]
    }
    if("All" %in% input$regionInput & !is.null(region)){
      xeffort <- x99[x99$WMU == "999" & x99$SPECIES %in% species & x99$Year %in% years,]
    }
    
    #### Calculate Stats ####
    tHarvest <- vector(length = length(years))
    mHarvest <- vector(length = length(years))
    fHarvest <- vector(length = length(years))
    uHarvest <- vector(length = length(years))
    jHarvest <- vector(length = length(years))
    rtHarvest <- vector(length = length(years))
    nrtHarvest <- vector(length = length(years))
    rDaysPer <- vector(length = length(years))
    nrDaysPer <- vector(length = length(years))
    rSuccess <- vector(length = length(years))
    nrSuccess <- vector(length = length(years))
    rEffort <- vector(length = length(years))
    nrEffort <- vector(length = length(years))
    for(i in 1:length(years)){
      mHarvest[i] <- round(sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]] *
                          x$`NON-RESIDENT MALE RATIO`[x$Year == years[i]],
                        x$`RESIDENT KILLS`[x$Year == years[i]] *
                          x$`RESIDENT MALE RATIO`[x$Year == years[i]], na.rm = T))
      fHarvest[i] <- round(sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]] *
                          x$`NON-RESIDENT FEMALE RATIO`[x$Year == years[i]],
                        x$`RESIDENT KILLS`[x$Year == years[i]] *
                          x$`RESIDENT FEMALE RATIO`[x$Year == years[i]], na.rm = T))
      uHarvest[i] <- round(sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]] *
                          x$`NON-RESIDENT UNKNOWN RATIO`[x$Year == years[i]],
                        x$`RESIDENT KILLS`[x$Year == years[i]] *
                          x$`RESIDENT UNKNOWN RATIO`[x$Year == years[i]], na.rm = T))
      jHarvest[i] <- round(sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]] *
                                 x$`NON-RESIDENT JUVENILE RATIO`[x$Year == years[i]],
                               x$`RESIDENT KILLS`[x$Year == years[i]] *
                                 x$`RESIDENT JUVENILE RATIO`[x$Year == years[i]], na.rm = T))
      rtHarvest[i] <- round(sum(x$`RESIDENT KILLS`[x$Year == years[i]], na.rm = T))
      nrtHarvest[i] <- round(sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]], na.rm = T))
      rDaysPer[i] <- (sum(x$`RESIDENT DAYS`[x$Year == years[i]], na.rm = T))/
                        (sum(x$`RESIDENT KILLS`[x$Year == years[i]], na.rm = T))
      nrDaysPer[i] <- (sum(x$`NON-RESIDENT DAYS`[x$Year == years[i]], na.rm = T))/
                          (sum(x$`NON-RESIDENT KILLS`[x$Year == years[i]], na.rm = T))
      rSuccess[i] <- rtHarvest[i]/
                       (sum(xeffort$`RESIDENT HUNTERS`[xeffort$Year == years[i]], na.rm = T))
      nrSuccess[i] <- nrtHarvest[i]/
                        (sum(xeffort$`NON-RESIDENT HUNTERS`[xeffort$Year == years[i]], na.rm = T))
      rEffort[i] <- round(sum(xeffort$`RESIDENT HUNTERS`[xeffort$Year == years[i]], na.rm = T))
      nrEffort[i] <- round(sum(xeffort$`NON-RESIDENT HUNTERS`[xeffort$Year == years[i]], na.rm = T))
    }
    tHarvest <- rtHarvest + nrtHarvest
    oHarvest <- uHarvest + jHarvest

    rDaysPer[rDaysPer == Inf] <- NA
    nrDaysPer[nrDaysPer == Inf] <- NA
    rSuccess[rSuccess == Inf] <- NA
    nrSuccess[nrSuccess == Inf] <- NA
    
    dt <- data.frame(cbind(years, tHarvest, rtHarvest, nrtHarvest, mHarvest, 
                           fHarvest, oHarvest, rDaysPer, nrDaysPer, rEffort, 
                           nrEffort, rSuccess, nrSuccess))
    dt
  })
  
  
   output$coolplot <- renderPlot({
     if(is.null(datatable())){
       return()
     }
     data <- datatable()

     if(is.null(inputs())){
       return()
     }
     params <- inputs()
     species <- params[[1]]
     units <- params[[2]]
     region <- params[[3]]
     arealabel <- params[[4]]
     years <- params[[5]]

     smooth <- input$smoothInput
     if(smooth){
       for(c in 2:ncol(data)){
        data[,c] <- smoothFunction(data[,c])
       }
     }

     label <- params[[6]]
     specieslab <- params[[7]]
     
     #### Harvest Plot ####
       main <- paste(specieslab, "Total Harvest in", label, sep = " ")
       ylab <- ifelse(smooth, "Total Harvest (Smoothed)", "Total Harvest")
       ymax <- max(c(data$tHarvest, data$rtHarvest, data$nrtHarvest), na.rm = T)*1.2
       ymin <- 0
       
       
       ##Trying ggplot
       datalong <- gather(data, "Type", "Value", tHarvest:nrSuccess)
       xscale <- scale_x_continuous(limit = c(min(years), max(years)), 
                                    breaks = seq(min(years), max(years), 5))
       textsize <- ifelse(input$plotInput == "Main Plot (4 Panels)", 16, 22)
       HPlot <- datalong %>%
         filter(Type == "tHarvest" | Type == "rtHarvest" | Type == "nrtHarvest") %>%
         ggplot(aes(x = years, y = Value, group = Type, color = factor(Type)))+
         geom_line(size = 2)+
         labs(x = "Year", y = ylab, title = main) + 
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Non Residents", "Residents", "Total")) + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = c(0.5, 0.95), legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))
       
       HPlot <- HPlot + xscale

    ##### Sex specific plot ####
         main <- paste(specieslab, "Harvest Composition", label, sep = " ")
         ylab <- ifelse(smooth, "Total Harvest (Smoothed)", "Total Harvest")
         ymax <- max(data$mHarvest, data$fHarvest, data$jHarvest, data$uHarvest, na.rm = T)*1.2
         ymin <- 0
         
         CompPlot <- datalong %>%
           filter(Type == "mHarvest" | Type == "fHarvest" | Type == "oHarvest") %>%
           ggplot(aes(x = years, y = Value, group = Type, color = factor(Type)))+
           geom_line(size = 2)+
           labs(x = "Year", y = ylab, title = main) +
           scale_y_continuous(limit = c(ymin, ymax)) +
           scale_colour_hue(labels = c("Females", "Males", "Other")) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = c(0.5, 0.95), legend.title = element_blank(),
                 legend.direction = "horizontal", legend.background = element_blank(),
                 text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))
         
         CompPlot <- CompPlot + xscale
      
       

       fr <- data$fHarvest/data$tHarvest*100
       or <- data$oHarvest/data$tHarvest*100
       d.temp <- data.frame(cbind(years, fr, or))
       datalong.temp <- gather(d.temp, "Type", "Value", fr:or)
       main <- paste("Female Ratio of", specieslab, "Harvest in", label, sep = " ")
       ylab <- ifelse(smooth, "% of Females in Harvest (Smoothed)", "% of Females in Harvest")
       ymax <- 100
       ymin <- 0
       
       RatioPlot <- ggplot(datalong.temp, aes(x = years, y = Value, group = Type,
                                              color = factor(Type)))+
         geom_line(size = 2)+
         labs(x = "Year", y = ylab, title = main) + 
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Female Ratio", "Other Ratio")) + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = c(0.5, 0.95), legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))
       
       RatioPlot <- RatioPlot + xscale

       
    #### Hunter Success ####
       if(input$effortInput == "Days Per Harvest") {
         r <- data$rDaysPer
         nr <- data$nrDaysPer
       }
       if(input$effortInput == "Hunter Success Rate"){
         r <- data$rSuccess*100
         nr <- data$nrSuccess*100
       }
       main <- ifelse(input$effortInput == "Days Per Harvest",
                      paste("Days per", specieslab, "Harvest in", label, sep = " "),
                      paste(specieslab, "Hunter Success Rate (%) in", label, sep = " "))
       ylab <- ifelse(input$effortInput == "Days Per Harvest",
                      ifelse(smooth, "Days per Kill (Smoothed)", "Days per Kill"),
                      ifelse(smooth, "Hunter Success Rate (Smoothed)", "Hunter Success Rate"))

       ymax <- ifelse(input$effortInput == "Days Per Harvest", (max(c(r, nr), na.rm = T)*1.2),
                      110)
       ymin <- 0
       d.temp <- data.frame(cbind(years, r, nr))
       datalong.temp <- gather(d.temp, "Type", "Value", r:nr)
       
       SuccessPlot <- ggplot(datalong.temp, aes(x = years, y = Value, group = Type,
                                                color = factor(Type)))+
         geom_line(size = 2)+
         labs(x = "Year", y = ylab, title = main) + 
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Non Residents", "Residents")) + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = c(0.5, 0.95), legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.92)))
       SuccessPlot <- SuccessPlot + xscale

     #### Total Effort ####
       main <- paste(specieslab, "Hunters in", label, sep = " ")
       ylab <- ifelse(smooth, "Number of Hunters (Smoothed)", "Number of Hunters")
       ymax <- max(c(data$rEffort, data$nrEffort), na.rm = T)*1.2
       ymin <- 0
       
       
       EffortPlot <- datalong %>%
         filter(Type == "rEffort" | Type == "nrEffort") %>%
         ggplot(aes(x = years, y = Value, group = Type, color = factor(Type)))+
         geom_line(size = 2)+
         labs(x = "Year", y = ylab, title = main) + 
         scale_y_continuous(limit = c(ymin, ymax)) +
         scale_colour_hue(labels = c("Non Residents", "Residents")) + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1),
               legend.position = c(0.5, 0.95), legend.title = element_blank(),
               legend.direction = "horizontal", legend.background = element_blank(),
               text = element_text(size = textsize), plot.title = element_text(size = rel(0.93)))
       EffortPlot <- EffortPlot + xscale

       if(input$plotInput == "Main Plot (4 Panels)"){
         if(input$sexDisplay == "Female Ratio"){
         grid.arrange(HPlot, RatioPlot, SuccessPlot, EffortPlot)}
         if(input$sexDisplay == "Harvest Composition"){
           grid.arrange(HPlot, CompPlot, SuccessPlot, EffortPlot)}
       }

       if(input$plotInput == "Harvest") grid.arrange(HPlot)
       if(input$plotInput == "Harvest Composition") grid.arrange(CompPlot)
       if(input$plotInput == "Female Ratio") grid.arrange(RatioPlot)
       if(input$plotInput == "Hunter Success/Days Per Harvest") grid.arrange(SuccessPlot)
       if(input$plotInput == "Number of Hunters") grid.arrange(EffortPlot)

  })
   
   
   tableOut <- reactive({
     if(is.null(datatable())){
       return()
     }
     dt <- datatable()
     dt$rDaysPer <- round(dt$rDaysPer, digits = 1)
     dt$nrDaysPer <- round(dt$nrDaysPer, digits = 1)
     dt$rSuccess <- round(dt$rSuccess, digits = 2)
     dt$nrSuccess <- round(dt$nrSuccess, digits = 2)
     colnames(dt) <- c("Year", "Total Harvest", "Resident Harvest", "Non-Resident Harvest",
                       "Male Harvest", "Female Harvest", "Other Harvest", 
                       "Resident Days Per Harvest", "Non-Resident Days Per Harvest",
                       "Resident Hunters", "Non-Resident Hunters", "Resident Success",
                       "Non-Resident Succeess")
     dt <- dt[order(dt$Year, decreasing = T),]
     dt
   })
 
   
   output$table <- DT:: renderDataTable(tableOut())
   
   output$downloadData <- downloadHandler(
     filename = function(){
       specieslab <- gsub(pattern = ",", x = inputs()[[7]], replacement = "")
       specieslab <- paste(unlist(strsplit(specieslab, split = " ")), collapse = "")
       
       arealab <- gsub(pattern = ",", x = inputs()[[6]], replacement = "")
       arealab <- paste(unlist(strsplit(arealab, split = " ")), collapse = "")
       
       paste0(paste(specieslab, "StatsIn", arealab, sep = "_"), ".csv")
     },
        content = function(file){
                    write.csv(tableOut(), file, row.names = F)
                    })
}
