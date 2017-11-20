
shinyServer(function(input, output, clientData, session) {
  
  # By default, Shiny limits file uploads to 5MB per file. You can modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R would increase the limit to 30MB.
  options(shiny.maxRequestSize=30*1024^2) 
  
  #######################################
  ####      Onglet :   Import CSV   #####
  
  dataInput <- reactive({
    file1 <- input$file11
    if(is.null(file1)){return()}
    
    if(input$iterationColumn1=="NULL"){ 
      itCol = NULL
    }else{
      itCol = as.numeric(input$iterationColumn1)
    }
    if(input$referenceYear1=="NULL"){ 
      refY = NULL
    }else{
      refY = as.numeric(input$referenceYear1)
    }
    if(input$rowToWithdraw1=="NULL"){ 
      rowW = NULL
    }else{
      rowW = as.numeric(input$rowToWithdraw1)
    }    
    ImportCSV(file=file1$datapath, sep=input$sep11, dec=input$dec11, header=TRUE, comment.char="#", iterationColumn = itCol, referenceYear = refY, rowToWithdraw = rowW)
  })
  
  output$filedf11 <- renderTable({
    if(is.null(dataInput())){return()}
    input$file11
  })
  
  output$table11 <- DT::renderDataTable({
    if(is.null(dataInput())){return()}
    DT::datatable(dataInput(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  output$AfficheTableLue11 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("About file", tableOutput("filedf11")), tabPanel("Data", DT::dataTableOutput("table11"))) 
  })
  
  

  
  ##################################################
  ####       MCMC des MinMax des groupes         ###
  
  namesG <- reactive({
    names = colnames(dataInput())
    return(names)
  })
  
  # Initialize reactive values
  valuesG <- reactiveValues()
  
  
  dataInput12 <- reactive({
    file2 <- input$file12
    if(is.null(file2)){return()}
    
    if(input$iterationColumn2=="NULL"){ 
      itCol = NULL
    }else{
      itCol = as.numeric(input$iterationColumn2)
    }
    if(input$referenceYear2=="NULL"){ 
      refY = NULL
    }else{
      refY = as.numeric(input$referenceYear2)
    }
    if(input$rowToWithdraw2=="NULL"){ 
      rowW = NULL
    }else{
      rowW = as.numeric(input$rowToWithdraw2)
    }    
    ImportCSV(file=file2$datapath, sep=input$sep12, dec=input$dec12, header=TRUE, comment.char="#", iterationColumn = itCol, referenceYear = refY, rowToWithdraw = rowW)
    
  })  
  
  output$filedf12 <- renderTable({
    if(is.null(dataInput12())){return()}
    input$file12
  })
  
  output$table12 <- DT::renderDataTable({
    if(is.null(dataInput12())){return()}
    DT::datatable(dataInput12(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  output$AfficheTableLue12 <- renderUI({
    if(is.null(dataInput12()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("About file", tableOutput("filedf12")), tabPanel("Data", DT::dataTableOutput("table12"))) 
  })
  
  observeEvent(input$StockageFile2, {
    valuesG$file2 <- dataInput12()
  })
 
  ##################################################
  ####     Creation des groupes de dates     ####
  ####      pour calcul des MinMax          ###
  
  output$ChainsSelectionG <- renderUI({
    themesG <- namesG()
    valuesG$namesG <- themesG
    checkboxGroupInput('ChainsSelectionG', 'Select a series of dates:', themesG)
  })

  # Add observer on select-all button
  observeEvent(input$selectAllG, {
    valuesG$namesG <- namesG()
    updateCheckboxGroupInput(session, 'ChainsSelectionG', selected =  valuesG$namesG)
  })

  # Add observer on clear-all button
  observeEvent(input$clearAllG, {
    valuesG$namesG <- c()
    updateCheckboxGroupInput(session, 'ChainsSelectionG', selected =  "none")
  })

  # data selectionnees
  selectDataG <- reactive({
    dataInput()[, input$ChainsSelectionG, drop = FALSE]
  })
  
  # affichage table de donnees
  output$DatasetG <- DT::renderDataTable({
    if(is.null(selectDataG())){return( )}
    DT::datatable(selectDataG(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  ## CreateMinMaxGroup
  createGroup1 <- eventReactive(input$goButton, {
    position = seq(1, length(input$ChainsSelectionG))
    dataGroup = CreateMinMaxGroup(selectDataG(), position=position, name =input$name, add=NULL) 
  })

  observeEvent(input$goButton, {
    valuesG$dataGroup <- createGroup1()
  })
  observeEvent(input$addButton, {
    valuesG$dataGroup <- addGroup()
  })
  
  addGroup <- eventReactive(input$addButton, {
    position = seq(1, length(input$ChainsSelectionG))
    addGroup = CreateMinMaxGroup(selectDataG(), position=position, name =input$name, add=valuesG$dataGroup)#, exportFile=export)
    return(addGroup)
  })

  observeEvent(input$clearButton, {
    valuesG$dataGroup <- NULL
  })
  
  output$tableGroup <- DT::renderDataTable({
    if(is.null(valuesG$dataGroup)){return()}
    DT::datatable(valuesG$dataGroup, options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })

  output$result13 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else
      tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetG")), tabPanel("Groups", DT::dataTableOutput("tableGroup")) )
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("MinMaxGroup", '.csv', sep='') },
    content = function(file) {
      write.csv(valuesG$dataGroup, file)
    }
  )
  
  observeEvent(input$StockageFile22, {
    valuesG$file2 <- valuesG$dataGroup
  })
  
  #######################################
  ####     Onglet : Convergence        ###
  ##    Checking the Markov chains    ##
  
  # Affichage des colonnes du dataframe
  namesCV <- reactive({
    names = colnames(dataInput())
    return(names)
  })
  
  # Initialize reactive values
  valuesCV <- reactiveValues()
  
  output$ChainsSelectionCV <- renderUI({
    themesCV <- namesCV()
    valuesCV$namesCV <- themesCV
    checkboxGroupInput('ChainsSelectionCV', 'Select a series of dates (at least two):', choices =themesCV, selected = valuesCV$namesCV[1:2])
  })
  
  # Add observer on select-all button
  observeEvent(input$selectAllCV, {
    valuesCV$namesCV <- namesCV()
    updateCheckboxGroupInput(session, 'ChainsSelectionCV', selected =  valuesCV$namesCV)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAllCV, {
    valuesCV$namesCV <- c()
    updateCheckboxGroupInput(session, 'ChainsSelectionCV', selected =  "none")
  })
  
  # data selectionnees
  selectDataCV <- reactive({
    dataInput()[, input$ChainsSelectionCV, drop = FALSE]
  })
  
  mcmc_List <- reactive({
    if(is.null(selectDataCV)){return()}
    coda.mcmc(selectDataCV(), numberChains = input$NbChains)#, iterationColumn = itC)
  })

  output$MCMCplot <- renderPlot({
    plot(mcmc_List())
  })

  GelmanDiag <- reactive({
    gelman.diag(mcmc_List())
  })
  
  output$GelmanDiagTable <- renderTable({
    if(is.null(GelmanDiag())) {return()}
    else {
        res = GelmanDiag()$psrf
        dim = dim(res)
        namesRes = rownames(res)
        PointEst = NULL
        UpperCI = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, namesRes[i])
          PointEst= c(PointEst, res[i,1])
          UpperCI = c(UpperCI, res[i,2])
        }
        data.frame("names"=names,"Point estimate"=PointEst, "Upper Credible Interval" = UpperCI)
      
    }
  })
  
  output$Gelmanplot <- renderPlot({
    gelman.plot(mcmc_List())
  })
  

  output$Diagnostics <- renderUI({
    if(is.null(selectDataCV()))
      h5("No data imported")
    else
      tabsetPanel(tabPanel("History Plots", plotOutput("MCMCplot")), tabPanel("Gelman Plots", plotOutput("Gelmanplot")), tabPanel("Gelman Diagnostic", tableOutput("GelmanDiagTable")) )
  })
  
  #######################################
  ####        Onglet : Dates        ####
  ####    Selection d une chaine      ##
  
  names <- reactive({
    names = colnames(dataInput())
    return(names)
  })
  
  observe({
    updateSelectInput(session, inputId='variables', 'Select a MCMC chain', choices = names() )
  })
  
  selectChain <- reactive({ 
    dataInput()[[ input$variables ]]
    })
  
  output$MarginalPlot <- renderPlot({
    MarginalPlot(selectChain(), level = input$level, title = input$titlePlot, newWindow=FALSE)
  })
  

  MarginalStatisticsText <- reactive({ 
    MarginalStatistics(selectChain(), level = input$level) 
  })
  
  output$MarginalStatisticsUI <- renderUI({ 
  tags$div(
    tags$p("Mean = ", MarginalStatisticsText()[1,1]),
    tags$p("MAP = ", MarginalStatisticsText()[2,1]),
    tags$p("sd = ", MarginalStatisticsText()[3,1]), 
    tags$p("Q1 = ", MarginalStatisticsText()[4,1]),
    tags$p("Median = ", MarginalStatisticsText()[5,1]),
    tags$p("Q2 = ", MarginalStatisticsText()[6,1]), 
    tags$p("For a level of confidence at ", MarginalStatisticsText()[7,1]*100, "%"),
    tags$p("Credible Interval = [", MarginalStatisticsText()[8,1], "," , MarginalStatisticsText()[9,1], "]"),
    tags$p("HPD region = [", MarginalStatisticsText()[10,1], "," , MarginalStatisticsText()[11,1], "]") 
    ) 
    })
  
  output$result2 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
    tabsetPanel(tabPanel("Marginal plot", plotOutput("MarginalPlot")), tabPanel("Marginal statistics", uiOutput("MarginalStatisticsUI")))
    })

  output$downloadPlotDates <- downloadHandler(
    filename = function() { paste("MarginalPlot", '.png', sep='') },
    content = function(file) {
      png(file)
      MarginalPlot(selectChain(), level = input$level, title = input$titlePlot, colors=input$color )
      dev.off()
    }
  )
  

  #####  Onglet : Dates -   Selection plusieurs chaines    ####
  
  # Initialize reactive values
  values <- reactiveValues()
  
  output$ChainsSelection <- renderUI({
    themes <- names()
    values$names <- themes
    checkboxGroupInput('multiChainsCI', 'Select numbers:', themes)
  })
  
  
  # Add observer on select-all button
  observeEvent(input$selectAll, {
    values$names <- names()
    updateCheckboxGroupInput(session, 'multiChainsCI', selected =  values$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll, {
    values$names <- c() 
    updateCheckboxGroupInput(session, 'multiChainsCI', selected =  "none")
  })
    
  # data selectionnees 
  selectData <- reactive({ 
    dataInput()[, input$multiChainsCI, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetCI <- DT::renderDataTable({
    if(is.null(selectData())){return( )}
    DT::datatable(selectData(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  # calcul des IC
  MultiCredibleIntervalText <- reactive({   
    if(is.null( input$multiChainsCI )) { return()}
    position = seq(1, length(input$multiChainsCI))
    MultiCredibleInterval(selectData(), position, level = input$level22) 
  })
  
  # affichage des resultats des IC
    output$resultTableMCI <- renderTable({
      if(is.null(MultiCredibleIntervalText())) {return()}
      else {
        dim = dim(MultiCredibleIntervalText())
        names_CI = rownames(MultiCredibleIntervalText())
        CIInf = NULL
        CISup = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_CI[i])
          CIInf= c(CIInf, MultiCredibleIntervalText()[i,2])
          CISup = c(CISup, MultiCredibleIntervalText()[i,3])
        }
        data.frame("names"=name,  "Credible Interval Inf"=CIInf, "Credible Interval Sup" = CISup)
      }
     })  
    
    ### calcul des HPD
    MultiHPDText <- reactive({   
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      MultiHPD(selectData(), position, level = input$level22) 
    })
    
    # affichage des resultats des IC
    output$resultTableMHPD <- renderTable({
      if(is.null(MultiHPDText())) {return()}
      else {
        dim = dim(MultiHPDText())
        names_HPD = rownames(MultiHPDText())
        HPDInf = NULL
        HPDSup = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_HPD[i])
          HPDInf= c(HPDInf, MultiHPDText()[i,2])
          HPDSup = c(HPDSup, MultiHPDText()[i,3])
        }
        data.frame("names"=name, "HPD Inf"=HPDInf, "HPD Sup" = HPDSup)
      }
    })     
    
    
    output$MultiDatesPlot <- renderPlot({
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      #if(input$exportFile22IT == "TRUE") { outFile = "IntervalsPlot"} else{ outFile = NULL}
      MultiDatesPlot(selectData(), position, intervals =input$intervals, order = input$order, level = input$level, title = input$titleIntervalsplot, newWindow=FALSE, print.data.result = FALSE)
    })#, height = 600, width = 800)
    
    output$downloadIntervalPlot <- downloadHandler(
      filename = function() { paste("downloadIntervalPlot", '.png', sep='') },
      content = function(file) {
        position = seq(1, length(input$multiChainsCI))
        png(file)
        MultiDatesPlot(selectData(), position, intervals =input$intervals, level = input$level, title = input$titleIntervalsplot, print.data.result = FALSE)
        dev.off()
      }
    )
    
    output$ui<- renderUI({
      switch(input$count,
             "TRUE" = textInput(inputId='ylabel', label="y-label", "Cumulative events" ),
             "FALSE" = textInput(inputId='ylabel', label="y-label", "Probability" )
      )
    })

    
    
    output$TempoPlot <- renderPlot({
        if(is.null( input$multiChainsCI )) { return()}
        position = seq(1, length(input$multiChainsCI))
        #if(input$exportFile22 == "TRUE") { outFile = "TempoPlot.png"} else{ outFile = NULL}
        TempoPlot(selectData(), position, level = input$level, title = input$titleTempoplot, Gauss=input$GaussCI, count=input$count, x.label=input$xlabel, y.label=input$ylabel, colors = input$colors, newWindow=FALSE, print.data.result = FALSE)
    })#, height = 600, width = 800)
    
    output$TempoPlotUI <- renderUI({
      if(is.null( input$multiChainsCI )) {h5(" Nothing to display ")}
      else{
        plotOutput("TempoPlot", width="80%")
      }
    })
    output$downloadTempoPlot <- downloadHandler(
      filename = function() { paste("downloadTempoPlot", '.png', sep='') },
      content = function(file) {
        position = seq(1, length(input$multiChainsCI))
        png(file)
        TempoPlot(selectData(), position, level = input$level, title = input$titleTempoplot, Gauss=input$GaussCI, count=input$count, x.label=input$xlabel, y.label=input$ylabel, colors = input$colors, print.data.result = FALSE)#, out.file=outFile)
        dev.off()
      }
    )
    
    output$TempoActivityPlot <- renderPlot({
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      TempoActivityPlot(selectData(), position, level = input$level, count=input$count, newWindow=FALSE, print.data.result = FALSE)
    })#, height = 600, width = 800)

    output$TempoActivityPlotUI <- renderUI({
      if(is.null( input$multiChainsCI )) {h5(" Nothing to display ")}
      else{
        plotOutput("TempoActivityPlot", width="80%")
      }
    })
    output$downloadActivityPlot <- downloadHandler(
      filename = function() { paste("downloadActivityPlot", '.png', sep='') },
      content = function(file) {
        position = seq(1, length(input$multiChainsCI))
        png(file)
        TempoActivityPlot(selectData(), position, level = input$level, count=input$count, print.data.result = FALSE)
        dev.off()
      }
    )
    
    output$OccurrencePlot <- renderPlot({
      if(is.null( input$multiChainsCI )) { return()}
      position = seq(1, length(input$multiChainsCI))
      OccurrencePlot(selectData(), position, level = input$level, count=input$count, newWindow=FALSE, print.data.result = FALSE)
    })
    
    output$OccurrencePlotUI <- renderUI({
      if(is.null( input$multiChainsCI )) {h5(" Nothing to display ")}
      else{
        plotOutput("OccurrencePlot", width="80%")
      }
    })
    output$downloadOccurrencePlot <- downloadHandler(
      filename = function() { paste("downloadOccurrencePlot", '.png', sep='') },
      content = function(file) {
        position = seq(1, length(input$multiChainsCI))
        png(file)
        OccurrencePlot(selectData(), position, level = input$level, count=input$count, print.data.result = FALSE)
        dev.off()
      }
    )
    
  output$result22 <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else 
    tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetCI")), 
                tabPanel("Credible intervals", uiOutput("resultTableMCI")), 
                tabPanel("HPD regions", uiOutput("resultTableMHPD")), 
                tabPanel("Intervals Plot", plotOutput("MultiDatesPlot")), 
                tabPanel("Tempo Plot", plotOutput("TempoPlot"), br(), plotOutput("TempoActivityPlot")), 
                tabPanel("Occurrence Plot", plotOutput("OccurrencePlot"))
                ) 
    
  })
  
  
  
  #######################################
  ####        Onglet : Tests         ###
  ##    Selection plusieurs chaines    ##
  
  # Initialize reactive values
#  valuesTests <- reactiveValues()
  
  namesTests <- reactive({
    names = colnames(dataInput())
    return(names)
  })
  
  observe({
    updateSelectInput(session, inputId='variableTest1a', 'Select date a', choices = namesTests())
    updateSelectInput(session, inputId='variableTest1b', 'Select date b', choices = namesTests())
  })
  
  selectChainTests <- reactive({ 
    dataInput()[,c(input$variableTest1a, input$variableTest1b), drop = FALSE]
  })
  
  output$DataSelectedTests <- DT::renderDataTable({
    if(is.null(selectChainTests())) { return( h5("")) }
    else
      DT::datatable(selectChainTests(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  MarginalProbaText <- renderText({
    MarginalProba(dataInput()[,input$variableTest1a, drop=TRUE], dataInput()[,input$variableTest1b, drop=TRUE])
  })
  
  output$MarginalProbaUI <- renderUI({
      tags$div(
        tags$p("The posterior probability that 'date a' is earlier than 'date b' is "),
        tags$p(MarginalProbaText()) 
      ) 
    })
  
  DatesHiatusText <- reactive({
    DatesHiatus(dataInput()[,input$variableTest1a, drop=TRUE], dataInput()[,input$variableTest1b, drop=TRUE], level = input$levelTests)
  })
  
  output$DatesHiatusUI <- renderUI({
    tags$div(
      tags$p("The testing procedure to check the presence of a gap between 'date a' and 'date b'"),
      tags$p("It returns the endpoints of the longest hiatus between two parameters. The result is given in calendar year (in format BC/AD)."),
      br(),
      tags$p("If 'NA', there is no hiatus at this level of confidence between 'date a' and 'date b'."),
      br(),
      tags$p("The inferior endpoint of the interval is ",DatesHiatusText()[2]),
      tags$p("The superior endpoint of the interval is ",DatesHiatusText()[3])
    ) 
  })
  
  
  
  output$resultTests <- renderUI({
    if(is.null(dataInput()))
      h5("No data imported")
    else
      tabsetPanel(tabPanel("Data selected", DT::dataTableOutput("DataSelectedTests")), tabPanel("Anteriority / Posteriority test", uiOutput("MarginalProbaUI")), tabPanel("Hiatus between dates", uiOutput("DatesHiatusUI")))
  })
  
  
  ##############################################
  ####         Onglet : Group of dates      ####
  ####    Selection d un group      ###
  
  dataGroup2 <- reactive({
    as.data.frame(valuesG$file2)
  })
  
  namesGroups <- reactive({
    names12 = colnames(dataGroup2())
    return(names12)
  })
  
  observe({
    updateSelectInput(session, inputId='variablesMin', 'Select the minimum of the group', choices = namesGroups())
    updateSelectInput(session, inputId='variablesMax', 'Select the maximum of the group', choices = namesGroups())
  })

  selectChain2 <- reactive({ 
    dataGroup2()[,c(input$variablesMin, input$variablesMax), drop = FALSE]
  })
  
  TestPhaseSelected <- reactive({
    if( sum(ifelse(dataGroup2()[,1] < dataGroup2()[,2], 1, 0)) == length(dataGroup2()[,1])) {return(1)}
  })

  output$selectedTable2 <- DT::renderDataTable({
    if(is.null(selectChain2())) { return( h5("")) }
    else
      DT::datatable(selectChain2(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })

  PhaseStatisticsText <- reactive({
    PhaseStatistics(dataGroup2()[,input$variablesMin, drop=TRUE], dataGroup2()[,input$variablesMax, drop=TRUE], level = input$level2)
  })

  output$PhaseStatisticsUI <- renderTable({
      res = PhaseStatisticsText()
      if(is.null(res)) {return()}
      else {
        dim = dim(res)
        names_PS = rownames(res)
        Minimum = NULL
        Maximum = NULL
        Duration = NULL
        name = NULL
        for (i in 1:dim[1]){
          name = c(name, names_PS[i])
          Minimum= c(Minimum, res[i,2])
          Maximum = c(Maximum, res[i,3])
          Duration = c(Duration, res[i,3])
        }
        data.frame("names"=name, "Minimum"=Minimum, "Maximum" = Maximum, "Duration" = Duration)
      }
  })

  PhaseTimeRangeText <- reactive({
    PhaseTimeRange(dataGroup2()[,input$variablesMin, drop=TRUE],dataGroup2()[,input$variablesMax, drop=TRUE], level = input$level2)
  })

  output$PhaseTimeRangeUI <- renderUI({
    res = PhaseTimeRangeText()
    tags$div(
      tags$p("For a level of confidence at ", res[1]*100, "%"),
      tags$p("Time Range = [", res[2], "," ,res[3], "]")
    )
  })


  output$PhasePlotFunction <- renderPlot({
    PhasePlot(dataGroup2()[,input$variablesMin, drop=TRUE], dataGroup2()[,input$variablesMax, drop=TRUE], level = input$level2, title = input$titlePlot2, colors=input$color2 )
  })
  output$downloadGroupPlot <- downloadHandler(
    filename = function() { paste("downloadGroupPlot", '.png', sep='') },
    content = function(file) {
      png(file)
      PhasePlot(dataGroup2()[,input$variablesMin, drop=TRUE], dataGroup2()[,input$variablesMax, drop=TRUE], level = input$level2, title = input$titlePlot2, colors=input$color2 )
      dev.off()
    }
  )
  
  output$PhaseDurationPlotFunction <- renderUI({
    PhaseDurationPlot(dataGroup2()[,input$variablesMin, drop=TRUE], dataGroup2()[,input$variablesMax, drop=TRUE], level = input$level2, title = "Duration of the phase", colors=input$color2 )
   })

  output$PhasePlotUI <- renderUI({
    if(is.null(dataGroup2()))
      {h5(" Nothing to display ")}
    else{
      plotOutput("PhasePlotFunction")
    }
  })

  output$PhaseDurationPlotUI <- renderUI({
    if(is.null(dataGroup2()))
    {h5(" Nothing to display ")}
    else{
      plotOutput("PhaseDurationPlotFunction")
    }
  })

  output$result3 <- renderUI({
    if(is.null(dataGroup2()))
      h5("No data imported")
    else
      tabsetPanel(tabPanel("Data selected", DT::dataTableOutput("selectedTable2")), tabPanel("Plot of the characteristics", fluidRow( uiOutput("PhasePlotUI")) ), tabPanel("Time range", uiOutput("PhaseTimeRangeUI")), tabPanel("Marginal Statistics", uiOutput("PhaseStatisticsUI")))
  })


  
  #####################################
  ####  Onglet : Several groups   ####
  
  # Initialize reactive values
  phases <- reactiveValues()
  
  output$PhasesSelection32 <- renderUI({
    themes <- namesGroups()
    succession$names <- themes
    checkboxGroupInput('multiPhasesSelection32', 'Select the minimum and the maximum of each group:', themes)
  })
  
  # Add observer on select-all button
  observeEvent(input$selectAll32, {
    succession$names <- namesGroups()
    updateCheckboxGroupInput(session, 'multiPhasesSelection32', selected =  succession$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll32, {
    succession$names <- c() 
    updateCheckboxGroupInput(session, 'multiPhasesSelection32', selected =  "none")
  })
  
  # data selectionnees 
  selectData32 <- reactive({ 
    dataGroup2()[, input$multiPhasesSelection32, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetPhases32 <- DT::renderDataTable({
    if(is.null(selectData32())){return()}
    DT::datatable(selectData32(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })
  
  Position_beginning32 <- reactive({
    dim = dim(selectData32())[2]
    pos = seq(1, dim, by = 2)
    return(pos)
  })

  MultiPhaseTimeRangeFunction <- reactive({
    MultiPhaseTimeRange(selectData32(), position_minimum = Position_beginning32(), level = input$levelMultiPhases)
  })

  output$MultiPhaseTimeRangeUI <- renderTable({
    res = MultiPhaseTimeRangeFunction()
    if(is.null(res)) {h5(" Nothing to display ")}
    else {
      dim = dim(res)
      names_MTR = rownames(res)
      PTInf = NULL
      PTSup = NULL
      names = NULL
      for (i in 1:dim[1]){
        names = c(names, names_MTR[i])
        PTInf= c(PTInf, res[i,2])
        PTSup = c(PTSup, res[i,3])
      }
      data.frame("names"=names,"Time Range Inf"=PTInf, "Time Range Sup" = PTSup)
    }
  })

  output$MultiPhasePlotFunction <- renderPlot({
    MultiPhasePlot(selectData32(), position_minimum = Position_beginning32(), title = input$titleMultiPhases, level = input$levelMultiPhases)
  })

  output$MultiPhasePlotUI <- renderUI({
    if(is.null(selectData32()))
      {h5(" Nothing to display ")}
    else
      plotOutput("MultiPhasePlotFunction")
  })
  
  output$result32 <- renderUI({
    if(is.null(dataGroup2()))
      h5("No data imported")
    else 
      tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetPhases32")), tabPanel("Time range", uiOutput("MultiPhaseTimeRangeUI")), tabPanel("Plot of the characteristics", "Marginal posterior densities of the minimum (oldest curve) and the maximum (youngest curve of the same color) of the selected groups and their time range interval (segment above the curves) at the desired level." ,uiOutput("MultiPhasePlotUI")))
  })
  
  output$downloadMultiPhasesPlot <- downloadHandler(
    filename = function() { paste("downloadGroupsPlot", '.png', sep='') },
    content = function(file) {
      png(file)
      MultiPhasePlot(selectData32(), position_minimum = Position_beginning32(), title = input$titleMultiPhases, level = input$levelMultiPhases)
      dev.off()
    }
  )
  
  ##########################################
  ####  Onglet : Succession de phases   ####
   
  # Initialize reactive values
  succession <- reactiveValues()
  
  output$PhasesSelection <- renderUI({
    themes <- namesGroups()
    succession$names <- themes
    checkboxGroupInput('multiPhasesSelection', 'Select the minimum and the maximum of each group:', themes)
  })
  
  # Add observer on select-all button
  observeEvent(input$selectAll4, {
    succession$names <- namesGroups()
    updateCheckboxGroupInput(session, 'multiPhasesSelection', selected =  succession$names)
  })
  
  # Add observer on clear-all button
  observeEvent(input$clearAll4, {
    succession$names <- c() 
    updateCheckboxGroupInput(session, 'multiPhasesSelection', selected =  "none")
  })
  
  # data selectionnees 
  selectData4 <- reactive({ 
    dataGroup2()[, input$multiPhasesSelection, drop = FALSE]
  })
  
  # affichage table de donnees  
  output$DatasetPhases <- DT::renderDataTable({
    if(is.null(selectData4())){return()}
    DT::datatable(selectData4(), options = list(pageLength = 5, dom = 'tip'), rownames=FALSE)
  })

  
    ## Ordering 
     Position_beginning <- reactive({
      ordre <- order(selectData4()[1,])
      pos = seq(1,length(ordre), by = 2)
      return(ordre[pos])
    })
     
     output$AffichagePositions <- renderUI({
           tags$div(
            tags$p("Positions of the beginnings 1", as.character(Position_beginning()[1]), ""),
            tags$p("Positions of the beginnings 2", as.character(Position_beginning()[2])),
            tags$p("Positions of the beginnings 3", as.character(Position_beginning()[3]), ""),
            tags$p("Positions of the beginnings 4", as.character(Position_beginning()[4]))
            )
     })
    
    ## Succession plot
    output$MultiSuccessionFunction <- renderPlot({
      MultiSuccessionPlot(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession, title = input$titleSuccessionPlot)
    }, height = 600, width = 800 )

    output$MultiSuccessionUI <- renderUI({
      if( length(Position_beginning() ) < 2)
        h5(" Nothing to display ")
      else
        plotOutput("MultiSuccessionFunction", width="100%")
    })
    

    output$downloadSuccessionPlot <- downloadHandler(
      filename = function() { paste("downloadSuccessionPlot", '.png', sep='') },
      content = function(file) {
        png(file)
        MultiSuccessionPlot(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession, title = input$titleSuccessionPlot)
        dev.off()
      }
    )
    
    
    ## Succession Transitions
    MultiPhasesTransitionFunction <- reactive({
      MultiPhasesTransition(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })

    output$MultiPhasesTransitionResults <- renderTable({
      if( length(Position_beginning() ) < 2){ return()}
      else {
        res = MultiPhasesTransitionFunction()
        dim = dim(res)
        names_MTR = rownames(res)
        TRInf = NULL
        TRSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MTR[i])
          TRInf= c(TRInf, res[i,2])
          TRSup = c(TRSup, res[i,3])
        }
        data.frame("names"=names,"Transition range Inf"=TRInf, "Transition range Sup" = TRSup)
      }
    })  

    ## Succession Gaps

    MultiPhasesGapFunction <- reactive({
      MultiPhasesGap(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })

    output$MultiPhasesGapResults <- renderTable({
      if( length(Position_beginning() ) < 2) { return()}
      else {
        res = MultiPhasesGapFunction()
        dim = dim(res)
        names_MPG = rownames(res)
        GapInf = NULL
        GapSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MPG[i])
          GapInf= c(GapInf, res[i,2])
          GapSup = c(GapSup, res[i,3])
          }
       data.frame("names"=names,"Gap range Inf"= GapInf, "Gap range Sup" = GapSup) 
      }
    })
    
    ## Succession Time range
    
    MultiPhaseTimeRangeFunction4 <- reactive({
      MultiPhaseTimeRange(selectData4(), position_minimum = Position_beginning(), level = input$levelSuccession)
    })
    
    output$MultiPhaseTimeRange4UI <- renderTable({
      if( length(Position_beginning() ) < 2){ return()}
      else {
        res = MultiPhaseTimeRangeFunction4()
        dim = dim(res)
        names_MTR = rownames(res)
        PTInf = NULL
        PTSup = NULL
        names = NULL
        for (i in 1:dim[1]){
          names = c(names, names_MTR[i])
          PTInf= c(PTInf, res[i,2])
          PTSup = c(PTSup, res[i,3])
        }
        data.frame("names"=names,"Time Range Inf"=PTInf, "Time Range Sup" = PTSup)
      }
    })   
    
   ## Output 
   output$Inputs4 <- renderUI({
     if(is.null(dataGroup2()))
       h5("No data imported")
     else 
       tabsetPanel(tabPanel("Data", DT::dataTableOutput("DatasetPhases")), tabPanel("Time ranges", uiOutput("MultiPhaseTimeRange4UI")), tabPanel("Transition ranges", uiOutput("MultiPhasesTransitionResults")), tabPanel("Gap ranges", uiOutput("MultiPhasesGapResults")), tabPanel("Succession plot", fluidRow("Curves represent the marginal posterior densities of the minimum and maximum of each group. Segments correspond to time range of the group of the same color,  two-coloured segments correspond to transition interval or to the gap range. A cross instead of a two-coloured segment means that there is no gap range at the desired level of confidence."),fluidRow(uiOutput("MultiSuccessionUI"))  )) 
   })
   
   
  ##########################################
  
})