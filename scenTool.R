library(ggplot2)
#library(reshape)
library(reshape2)
#library(plotly)
#using data tables is much faster than data frames
library(data.table)

#limit for file upload set to 300 MB
options(shiny.maxRequestSize = 300*1024^2)

file <- NULL

#server function
server <- function(input,output,session,extFile=file) {
  #function for reading data files
  read.mif <- function(inFile) {
    if (is.null(inFile)) {
      return(NULL)
    } else {
      s <- fread(inFile,sep=";",header=FALSE,nrows=1) 
      if (all(names(s) == "V1")) sep <- "," else sep <- ";"
      #fread is much faster than read.table
      wide <- fread(inFile,sep=sep,header=TRUE,stringsAsFactors = TRUE,na.strings = "N/A",check.names = FALSE)
      #clean the data table
      del <- which(names(wide) == "")
      if (length(del) > 0) wide <- wide[,-del]
      #convert data from wide to long format
      long <- melt(wide, id.vars=c(1:5),variable.name = "Year",value.name = "Value",na.rm = FALSE)
      #reorder columns
      setcolorder(long,c(1,2,3,6,4,5,7))
      #set column names
      names(long) <- c("Model","Scenario","Region","Year","Variable","Unit","Value")
      #merge variable and unit column
      long$Variable <- with(long,paste0(Variable," (",Unit,")"))
      long$Variable <- as.factor(long$Variable)
      long$Unit <- NULL
      #order
      long$Model <- factor(long$Model,levels=unique(long$Model))
      long$Scenario <- factor(long$Scenario,levels=unique(long$Scenario))
      long$Region <- factor(long$Region,levels=unique(long$Region))
      long$Variable <- factor(long$Variable,levels=unique(long$Variable))
      #remove NAs
      long <- na.omit(long)
      #convert years to numeric
      long$Year <- as.numeric(as.character(long$Year))
      #keep only complete cases
      long <- long[complete.cases(long),]
      return(long)
    }
  }

  #initialize reactive value
  val <- reactiveValues(a = NULL,sel=NULL,title=NULL,ylab=NULL)
  
  if(is.null(extFile)) {
    #create dummy data for testing the tool
    model <- factor(c("Model1","Model2","Model3"))
    scenario <- factor(c("Scen1","Scen2","Scen3"))
    region <- factor(c("Region1","Region2","Region3"))
    year <- c(2005,2050,2100)
    variable <- factor(c("Variable1 (Unit1)","Variable2 (Unit2)","Variable3 (Unit3)"))
    long <- expand.grid(model,scenario,region,year,variable,1,KEEP.OUT.ATTRS = FALSE,stringsAsFactors = TRUE)
    names(long) <- c("Model","Scenario","Region","Year","Variable","Value")
    long$Value <- 1:length(long$Value)
    val$a <- as.data.table(long)
  } else val$a <- read.mif(extFile)

  
  
  #Upload and read in data file if there is a change in input$datafile
  observeEvent(input$datafile, {
    print("read data")
    #assing to reactive value
    val$a <- read.mif(input$datafile$datapath)
    
  })

  # observeEvent(c(input$model,input$scenario,input$region,input$year,input$variable),{
  #   print("subset data")
  #   val$sel <- val$a[which(val$a$Model %in% input$model & val$a$Scenario %in% input$scenario & val$a$Region %in% input$region & val$a$Year %in% input$year & val$a$Variable %in% input$variable), ]
  # })
  
  #subsetting the data stepwise is faster than all at once
  observeEvent(c(input$model,input$scenario,input$region,input$year,input$variable),{
    print("subset data")
    val$sel <- val$a[Model %in% input$model, ]
    val$sel <- val$sel[Scenario %in% input$scenario, ]
    val$sel <- val$sel[Region %in% input$region, ]
    val$sel <- val$sel[Year %in% input$year, ]
    val$sel <- val$sel[Variable %in% input$variable, ]
  })
  
  observeEvent(c(input$variable,input$stackshare,input$plottype,input$switchaxis),{
    if(length(input$variable) == 1) {
      val$title <- strsplit(input$variable[1]," \\(")[[1]][1]
      val$ylab <- strsplit(input$variable[1]," \\(")[[1]][2]
      val$ylab <- substr(val$ylab, 1, nchar(val$ylab)-1)
      if (input$stackshare) val$ylab <- "Share"
    } else if (input$plottype == "scatter") {
      val$title <- ""
      if(input$switchaxis) val$ylab <- input$variable[2] else val$ylab <- input$variable[1]
    } else {
      val$title <- "Variable(s)"
      val$ylab <- "Unit"
    }
    updateTextInput(session, "plottitle", value = val$title)
    updateTextInput(session, "ylab", value = val$ylab)
  })
  
  observeEvent(c(input$plottitle,input$ylab),{
    val$title <- input$plottitle
    val$ylab <- input$ylab
  })
  
  observe({
    print("update choices")
    updateSelectInput(session, "model", choices = levels(val$a$Model),selected = if (length(levels(val$a$Model)) > 5) levels(val$a$Model)[1:5] else levels(val$a$Model))
    updateSelectInput(session, "scenario", choices = levels(val$a$Scenario),selected = if (length(levels(val$a$Scenario)) > 3) levels(val$a$Scenario)[1:3] else levels(val$a$Scenario))
    updateSelectInput(session, "region", choices = levels(val$a$Region),selected = if ("World" %in% levels(val$a$Region)) "World" else levels(val$a$Region)[1])
    updateSelectInput(session, "year", choices = unique(val$a$Year),selected = unique(val$a$Year))
    updateSelectInput(session, "variable", choices = levels(val$a$Variable),selected = levels(val$a$Variable)[1])
    #    updateSelectInput(session, "normalizeYear", choices = unique(val$a$Year),selected = unique(val$a$Year)[1])
  })
  
  plot <- reactive({
    myBreaks <- function(x){
      if(length(unique(x)) <= 3) {
        breaks <- unique(x)
      } else {
        breaks <- c(min(x),round(mean(range(x)),digits = -1),max(x))
      }
      names(breaks) <- attr(breaks,"labels")
      breaks
    }
    
    ggname <- function(x) {
      if (class(x) != "character") {
        return(x)
      }
      y <- sapply(x, function(s) {
        if (!grepl("^`", s)) {
          s <- paste("`", s, sep="", collapse="")
        }
        if (!grepl("`$", s)) {
          s <- paste(s, "`", sep="", collapse="")
        }
      }
      )
      y 
    }
    
    
    color <- input$color
      fill <- input$fill
      if(input$plottype == 'line') fill <- NULL
      if(input$plottype == 'bar' || input$plottype == 'area') color <- NULL
#      if(input$normalize) norm <- input$normalizeYear else norm <- NULL
      sel <- val$sel
      if(input$plottype == 'scatter') {
        sel <- reshape(sel, timevar = "Variable", idvar = names(sel)[!(names(sel) %in% c("Value", "Variable"))], direction = "wide")
        names(sel) <- gsub("Value.", "", names(sel))
        if(input$switchaxis) {
          x_var <- input$variable[1]
          y_var <- input$variable[2]
        } else{
          x_var <- input$variable[2]
          y_var <- input$variable[1]
        }
        p <- ggplot(data=sel, aes_string(x=ggname(x_var), y=ggname(y_var))) + theme_minimal()
      } else {
        p <- ggplot(data=sel, aes(x=Year, y=Value)) + theme_minimal()
      }
      if(input$plottype == "scatter") {
        p <- p + geom_line(aes_string(color=input$color_scatter,linetype=input$linetype_scatter)) + geom_point(aes_string(color=input$color_scatter))
      } else if(input$plottype == "line") {
        p <- p + geom_line(aes_string(color=color,linetype=input$linetype)) + geom_point(aes_string(color=color))
        p <- p + scale_x_continuous(breaks = myBreaks(sel$Year))
      } else if(input$plottype == "bar") {
        sel$Year <- as.factor(sel$Year)
        data_pos <- sel
        data_neg <- sel
        data_pos$Value[data_pos$Value<0] <- 0
        data_neg$Value[data_neg$Value>=0] <- 0
        if (input$stack) {
          if (input$stackshare) {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='fill',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='fill',stat='identity',aes_string(fill=fill))
          } else {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='stack',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='stack',stat='identity',aes_string(fill=fill))
          }
        } else p <- p + geom_bar(data=sel,position='dodge',stat='identity',aes_string(fill=fill))
      } else if (input$plottype == "area") {
        data_pos <- sel
        data_neg <- sel
        data_pos$Value[data_pos$Value<0] <- 0
        data_neg$Value[data_neg$Value>=0] <- 0
        if (input$stack) {
          if (input$stackshare) {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='fill',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='fill',stat='identity',aes_string(fill=fill))
          } else {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='stack',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='stack',stat='identity',aes_string(fill=fill))
          }
          
        } else p <- p + geom_area(data=sel,position='dodge',stat='identity',aes_string(fill=fill))
        p <- p + scale_x_continuous(breaks = myBreaks(sel$Year))
      }
      
      if (!is.null(input$facet_y)) {
        p <- p + facet_grid(as.formula(paste(paste(input$facet_y,collapse = '+'), "~",if(is.null(input$facet_x)) "." else paste(input$facet_x,collapse = '+'))))
      } else if (!is.null(input$facet_x)) p <- p + facet_wrap(as.formula(paste("~", paste(input$facet_x,collapse = '+'))), ncol=input$ncol)
      
      p <- p + ylab(val$ylab) + ggtitle(val$title)
      p <- p + theme(axis.text.x = element_text(angle=90, vjust=0.5))
#      p <- p + theme(axis.text = element_text(size = 20))
      
      return(p)
  })
  
  # output$plot <- renderPlotly({
  #   layout(p = ggplotly(plot(),tooltip=c("y",if(input$plottype == "line") {"colour"} else {"fill"})),margin = list(b = 120,l = 180))
  # })
  
  output$plot <- renderPlot({
    plot()},res = 120)#height = 400, width = 500

  output$summary <- renderPrint({
    summary(val$sel$Value)
  })
  output$info <- renderPrint({
    cat(paste(length(levels(val$a$Model)),"Model(s)"),
        paste(length(levels(val$a$Scenario)),"Scenario(s)"),
        paste(length(levels(val$a$Region)),"Region(s)"),
        paste(length(unique(val$a$Year)),"Year(s)"),
        paste(length(levels(val$a$Variable)),"Variable(s)"),sep="\n")
  })
  output$data <- renderDataTable({
    val$sel
    }, options = list(pageLength = 10))
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("export", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plot(), device = "pdf",scale=1,width=20,height=13,unit="cm",dpi=150)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() { paste("export", '.csv', sep='') },
    content = function(file) {
      out <- val$sel
      out$Unit = as.character(lapply(strsplit(as.character(out$Variable), split=" \\("), "[", 2))
      out$Unit <- as.factor(substr(out$Unit,1,nchar(out$Unit)-1))
      out$Variable = as.factor(as.character(lapply(strsplit(as.character(out$Variable), split=" \\("), "[", 1)))
      setcolorder(out,c(1,2,3,4,5,7,6))
      out <- dcast(out, Model + Scenario + Region + Variable + Unit ~ Year, value.var="Value")
      write.csv(out, file ,row.names = FALSE,quote = FALSE)
     }
  )
}

#client-sided function
ui <- fluidPage(
  #titlePanel("Scenario Analysis Tool"),
  navbarPage("Scenario Analysis Tool 0.1",
             tabPanel("Panel 1",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('datafile', 'Choose CSV/MIF File', accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.mif')),
                          tags$hr(),
                          selectInput('model', 'Model', "Pending upload",multiple = TRUE),
                          selectInput('scenario', 'Scenario', "Pending upload",multiple = TRUE),
                          selectInput('region', 'Region', "Pending upload",multiple = TRUE),
                          selectInput('year', 'Year', "Pending upload",multiple = TRUE),
                          selectInput('variable', 'Variable', "Pending upload",multiple = TRUE)
                          #      sliderInput("year", "years", min = min(dims$year), max = max(dims$year), value = range(dims$year))
                        ,width=3),
                        # sidebarPanel(
                        #   selectInput('color', 'Color', c("Model","Scenario","Region","Variable"),multiple = FALSE,selected = "Scenario")
                        # ),
                        mainPanel(
                          tabsetPanel(id = "main",type = "tabs",
                                      tabPanel("Plot", 
                                               # plotlyOutput("plot",height = "550px",width = "100%"),
#                                               plotOutput("plot",height = "550px",width = "100%"),
                                               plotOutput("plot",width = "100%"),
                                               wellPanel(
                                                 fluidRow(
                                                   column(2,
                                                          radioButtons("plottype", "Plot Type", c("line","bar","area","scatter"), selected = "line", inline = F),
                                                          #                                                      checkboxInput('normalize', 'Normalize', value = FALSE, width = NULL),
                                                          #                                                      conditionalPanel(condition = "input.normalize == true", selectInput('normalizeYear', 'Year', "Pending upload",multiple = FALSE)),
                                                          conditionalPanel(condition = "input.plottype == 'bar' || input.plottype == 'area'", checkboxInput('stack', 'Stack', value = FALSE, width = NULL)),
                                                          conditionalPanel(condition = "input.stack == true", checkboxInput('stackshare', 'Share', value = FALSE, width = NULL)),
                                                          conditionalPanel(condition = "input.plottype == 'scatter'", checkboxInput('switchaxis', 'Switch Axis', value = FALSE, width = NULL))
                                                   ),
                                                   column(2,
                                                          conditionalPanel(condition = "input.plottype == 'line'",
                                                                           radioButtons('color', 'Color', c("Model","Scenario","Region","Variable"),selected = "Model",inline = F)),
                                                          conditionalPanel(condition = "input.plottype == 'bar' || input.plottype == 'area'",
                                                                           radioButtons('fill', 'Fill', c("Model","Scenario","Region","Variable"),selected = "Model",inline = F)),
                                                          conditionalPanel(condition = "input.plottype == 'scatter'",
                                                                           radioButtons('color_scatter', 'Color', c("Model","Scenario","Region","Year"),selected = "Model",inline = F))
                                                          
                                                   ),
                                                   column(2,
                                                          conditionalPanel(condition = "input.plottype == 'line'", radioButtons('linetype', 'Line Type', c("Model","Scenario","Region","Variable"),selected = "Scenario",inline = F)),
                                                          conditionalPanel(condition = "input.plottype == 'scatter'", radioButtons('linetype_scatter', 'Line Type', c("Model","Scenario","Region","Year"),selected = "Scenario",inline = F))
                                                   ),
                                                   column(2,
                                                          checkboxGroupInput('facet_x', 'Horizontal', c("Model","Scenario","Region","Variable"),selected = NULL),
                                                          conditionalPanel(condition = 'input.facet_x[0] != null & input.facet_y[0] == null', numericInput('ncol', 'Columns', value = 5, min = 1, max = 10,step = 1, width = NULL))
                                                   ),
                                                   column(2,
                                                          checkboxGroupInput('facet_y', 'Vertical', c("Model","Scenario","Region","Variable"),selected = NULL)
                                                   ),
                                                   column(2,
                                                          textInput('plottitle', 'Plot Title', "Variable(s)"),
                                                          textInput('ylab', 'Y Axis Label', "Unit")
                                                   )
                                                 )
                                               ),
                                               wellPanel(downloadButton('downloadPlot', 'Download Plot'))
                                      ),
                                      tabPanel("Table", 
                                               dataTableOutput("data"),
                                               wellPanel(downloadButton('downloadData', 'Download Data'))
                                               ),
                                      tabPanel("Info", 
                                               h2("Summary"),
                                               verbatimTextOutput("summary"),
                                               h2("General information about the dataset"),
                                               verbatimTextOutput("info")
                                      )
                          )
                        )
                      )
             )
  )
)

#start the app
shinyApp(ui = ui, server = server)

