#Data Analysis Tools version 1.0 Beta
#A Shiny app to help facilitate everyday tasks performed by data scientists working with categorical data
#Minimap active when appropriate zip code data is present in data set; will default to logo image file if available
#Date subsetting possible when data set contains a date column


library(tidyverse)
library(viridis)
library(zipcode)
library(ggmap)
library(lubridate)
library(shiny)
library(ggimage)
library(shinyjs)


data(zipcode)

ui <- 	fluidPage(

	titlePanel('Data Analysis Tools v. 1.0 Beta'),

	sidebarLayout(

		

    		sidebarPanel(
    			fileInput(inputId='whatfile',label='Choose Data Set'),
			actionButton("UpdateButton", "Update Values"),
			dateRangeInput(inputId='DateRange',label='Intake Date Range',start='2015-01-01'),
			selectInput(inputId='sumvariable',label='Summary Variable',choices=c("")),
			selectInput(inputId='ColorScheme',label='Viridis Color Scheme',choices=c('viridis','magma','cividis','plasma','inferno')),
			selectInput(inputId='CorrelVar1',label='Variable 1',choices=c("")),
			selectInput(inputId='CorrelVar2',label='Variable 2',choices=c("")),			
			plotOutput(outputId='minimap')   
   		),
    		
		mainPanel(
			textOutput(outputId='datatitle',container=h2),
			textOutput(outputId='datelabel',container=h3),
			dataTableOutput(outputId='summarytable'),
			textOutput(outputId='daterangetotal'),
			plotOutput(outputId='summaryplot'),
			tableOutput(outputId='CoTable'),
			textOutput(outputId='CramersV')
		)

	)

)






server <- function(input,output,session) {

options(shiny.maxRequestSize=9*1024^2)
###############Categorical Summaries#########################
	output$datatitle <- renderText({req(input$whatfile); paste("Data Summary For ", input$whatfile[1,'name'], sep="")})



	output$datelabel <- renderText({req(input$whatfile);
		tempdata <- read_csv(input$whatfile[1,'datapath']);
		if ('IntakeDate' %in% names(tempdata)) {
			paste("Date Range: ",as.character(input$DateRange[[1]]), " to ",as.character(input$DateRange[[2]]),sep="")
		}
	})

themap <- reactive({
	dataformap <- read_csv(input$whatfile[1,'datapath'])
		if ('zip' %in% names(dataformap)) {
			backmap <- get_stamenmap(bbox=c(top=42,bottom=38.5,left=-85,right=-80.5),maptype='watercolor',zoom=8)
			if ('IntakeDate' %in% names(dataformap)) {
				dataformap <- select(dataformap,zip,IntakeDate) %>% filter(nchar(as.character(zip))==5)
				mapsum <- filter(dataformap,input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]]) %>% group_by(zip) %>% summarize(Count=n()) %>% as.data.frame(); 
			}
			else {
				dataformap <- select(dataformap,zip) %>% filter(nchar(as.character(zip))==5)
				mapsum <- group_by(dataformap,zip) %>% summarize(Count=n()) %>% as.data.frame();
			}	
			zips <- full_join(mapsum,zipcode,by='zip'); themap <- ggmap(backmap) + geom_point(data=zips,aes(x=longitude,y=latitude,color=Count,size=Count,alpha=.4)) + scale_color_viridis() + theme(legend.position='none') + labs(x='Longitude',y='Latitude',title='ZIP Code Distribution');
			themap
		}
		else {emptyplot <- ggplot();
			backthing <-'genericlogo.jpg';
			themap <-ggbackground(emptyplot,backthing)
			themap
		}
})
	output$minimap <- renderPlot({req(input$whatfile); themap()})


	possiblechoices <- reactive({
		possiblechoices <- read_csv(input$whatfile[1,'datapath']) %>% names();
		possiblechoices
	})


	observeEvent(input$UpdateButton,
		{
			updateSelectInput(session,'sumvariable', choices = possiblechoices());
			updateSelectInput(session,'CorrelVar1', choices = possiblechoices());
			updateSelectInput(session,'CorrelVar2', choices = possiblechoices());
   		}
	)

someframe <- reactive({req(input$whatfile)
		somedata <- read_csv(input$whatfile[1,'datapath'])

		someframe <- filter(somedata,!is.na(get(input$sumvariable))) %>% filter(!str_detect(get(input$sumvariable),"\\|") & !str_detect(get(input$sumvariable),"\\)")) %>% as.data.frame()
		# get(input$sumvariable)
			
		
		if ('IntakeDate' %in% names(someframe)) {
			someframe <- filter(someframe,input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]])
			someframe <- as.data.frame(someframe)	
			}
		else{}
		someframe <- group_by(someframe,get(input$sumvariable)) %>% summarize(Count=n()); names(someframe) <- gsub('get\\(input\\$sumvariable\\)',input$sumvariable,names(someframe))
			
		as.data.frame(someframe)
})

	output$summarytable <- renderDataTable(options = list(pageLength = 5),{req(input$whatfile);someframe()})

somedata3 <- reactive({req(input$whatfile);
		somedata3 <- read_csv(input$whatfile[1,'datapath'])
		if ('IntakeDate' %in% names(somedata3)) {
			somedata3 <- filter(somedata3,!is.na(get(input$sumvariable))) %>% filter(!str_detect(get(input$sumvariable),"\\|") & !str_detect(get(input$sumvariable),"\\)"))%>%filter(input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]]);paste("Total For Given Date Range: ", nrow(somedata3),sep="")
		}
		else {
			somedata3 <- filter(somedata3,!is.na(get(input$sumvariable))) %>% filter(!str_detect(get(input$sumvariable),"\\|") & !str_detect(get(input$sumvariable),"\\)"));paste("Total For Data Set: ", nrow(somedata3),sep="")
		}
})


	output$daterangetotal <- renderText({req(input$whatfile);somedata3()})
	
Summaryplot <- reactive({req(input$whatfile);req(input$ColorScheme);
		somedata2 <- read_csv(input$whatfile[1,'datapath']);
		someframe2 <- filter(somedata2,!is.na(get(input$sumvariable))) %>% filter(!str_detect(get(input$sumvariable),"\\|") & !str_detect(get(input$sumvariable),"\\)")) 
		if ('IntakeDate' %in% names(somedata2)) {
			someframe2 <- filter(someframe2,input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]])%>% group_by(get(input$sumvariable)) %>% summarize(Count=n()) %>% as.data.frame()
		} 
		else {
			someframe2 <- group_by(someframe2,get(input$sumvariable)) %>% summarize(Count=n()) %>% as.data.frame()
		}
		itemslength=nchar(as.character(someframe2[1,1]));
		for (i in 2:nrow(someframe2)) {
			if (nchar(as.character(someframe2[i,1])) > itemslength) {
				itemslength <- nchar(as.character(someframe2[i,1]))
			}
		}

		if ('IntakeDate' %in% names(somedata2)) {
			if ((nrow(someframe2)*itemslength) > 128) {
				ggplot(data=someframe2,aes(x=factor(someframe2[,1]),y=Count,fill=factor(someframe2[,1]))) + geom_col() + scale_fill_viridis(option=input$ColorScheme,discrete=TRUE) + labs(title=paste(input$sumvariable," Summary From ", as.character(input$DateRange[[1]]), " to ", as.character(input$DateRange[[2]]), sep=""),subtitle=paste("From ",input$whatfile[1,'name']," dataset",sep=""),x=input$sumvariable, fill=input$sumvariable) + theme(axis.text.x = element_text(angle=90, hjust=1)) + theme(legend.position='none')
			}
			else {
				ggplot(data=someframe2,aes(x=factor(someframe2[,1]),y=Count,fill=factor(someframe2[,1]))) + geom_col() + scale_fill_viridis(option=input$ColorScheme,discrete=TRUE) + labs(title=paste(input$sumvariable," Summary From ", as.character(input$DateRange[[1]]), " to ", as.character(input$DateRange[[2]]), sep=""),subtitle=paste("From ",input$whatfile[1,'name']," dataset",sep=""),x=input$sumvariable, fill=input$sumvariable) + theme(legend.position='none')
			}
		}

		else {
			if ((nrow(someframe2)*itemslength) > 128) {
				ggplot(data=someframe2,aes(x=factor(someframe2[,1]),y=Count,fill=factor(someframe2[,1]))) + geom_col() + scale_fill_viridis(option=input$ColorScheme,discrete=TRUE) + labs(title=paste(input$sumvariable," Summary",sep=""),subtitle=paste("From ",input$whatfile[1,'name']," dataset",sep=""),x=input$sumvariable, fill=input$sumvariable) + theme(axis.text.x = element_text(angle=90, hjust=1)) + theme(legend.position='none')
			}
			else {
				ggplot(data=someframe2,aes(x=factor(someframe2[,1]),y=Count,fill=factor(someframe2[,1]))) + geom_col() + scale_fill_viridis(option=input$ColorScheme,discrete=TRUE) + labs(title=paste(input$sumvariable," Summary",sep=""),subtitle=paste("From ",input$whatfile[1,'name']," dataset",sep=""),x=input$sumvariable, fill=input$sumvariable) + theme(legend.position='none')
			}
		}
})

	output$summaryplot <- renderPlot({req(input$whatfile);Summaryplot()})




crosstabstable <- reactive({
		req(input$whatfile);
		catcordata <- read_csv(input$whatfile[1,'datapath']) %>% filter(!is.na(get(input$CorrelVar1))) %>% filter(!str_detect(get(input$CorrelVar1),"\\|") & !str_detect(get(input$CorrelVar1),"\\)")) %>%
		filter(!is.na(get(input$CorrelVar2))) %>% filter(!str_detect(get(input$CorrelVar2),"\\|") & !str_detect(get(input$CorrelVar2),"\\)")) %>% as.data.frame();
		if ('IntakeDate' %in% names(catcordata)) {
			catcordata <- filter(catcordata,input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]])
		}
		corvar1 <- input$CorrelVar1;
		corvar2 <- input$CorrelVar2;
		contingency <- xtabs(data=catcordata,~ get(corvar1) + get(corvar2));
		contingency <- as.data.frame(contingency);
		names(contingency) = c(corvar1,corvar2,"Count");
		arrange(contingency,get(corvar1)) %>% as.data.frame()
})

	output$CoTable <- renderTable({req(input$whatfile);crosstabstable()})


sumcramervalue <- reactive({
		catcordata2 <- read_csv(input$whatfile[1,'datapath']) %>% filter(!is.na(get(input$CorrelVar1))) %>% filter(!str_detect(get(input$CorrelVar1),"\\|") & !str_detect(get(input$CorrelVar1),"\\)")) %>%
			filter(!is.na(get(input$CorrelVar2))) %>% filter(!str_detect(get(input$CorrelVar2),"\\|") & !str_detect(get(input$CorrelVar2),"\\)")) %>% as.data.frame();
		if ('IntakeDate' %in% names(catcordata2)) {
			catcordata2 <- filter(catcordata2,input$DateRange[[1]] < mdy(IntakeDate) & mdy(IntakeDate) < input$DateRange[[2]])
			}
			corvar1 <- input$CorrelVar1;
			corvar2 <- input$CorrelVar2;
			contingency <- xtabs(data=catcordata2,~ get(corvar1) + get(corvar2));
#Uncomment the following to test an unrelated file: contingency <- read_csv("testdata.csv") %>% as.data.frame()
			numcells <- nrow(contingency) * ncol(contingency)
			numlessthan5 <- 0
			lessthan1 <- FALSE
			lessthan5 <- FALSE
			expectvals <- contingency
			chisquare <- contingency
			for (x in 1:nrow(contingency)) {
				for (y in 1:ncol(contingency)) {
					if (contingency[x,y] < 1) {
						lessthan1 <- TRUE
					}
					if (contingency[x,y] < 5) {
						numlessthan5 <- numlessthan5 + 1;
					}
					else {}
				}
			}
			percentlessthan5 <- numlessthan5/numcells
			if (percentlessthan5 > .2) {
				lessthan5 <- TRUE
			}
			else {}
		#This line was used to debug   return(paste(numlessthan5,",",numcells,",",percentlessthan5,",",lessthan5))
			if (lessthan1 && lessthan5) {
				return("Chi-Square statistic is not valid due to a sparse data set.  At least one cell with value less than 1 and greater than 20% of cells have a value less than 5.")
			}
			else if (lessthan1) {
					return("Chi-Square statistic is not valid due to a sparse data set.  At least one cell with value less than 1.")
				}
			else if (lessthan5) {
					return("Chi-Square statistic is not valid due to a sparse data set.  Greater than 20% of cells have a value less than 5.")
				}
			
			else {
				
				for (a in 1:nrow(contingency)) {
					for (b in 1:ncol(contingency)) {
						expectvals[a,b] <- (sum(contingency[a,])*sum(contingency[,b]))/(sum(contingency))
						chisquare[a,b] <- ((contingency[a,b]-expectvals[a,b])^2)/expectvals[a,b]
					}
				}
				chival <- sum(chisquare)
				maxcontrib <- max(chisquare)
				dof <- 	(nrow(contingency)-1)*(ncol(contingency)-1)		
				cramer <- sqrt(chival/(sum(contingency)*min((nrow(contingency)-1),(ncol(contingency)-1)  ) ))
				chiprobabilities <- read_csv("ChiSquareProbabilities.csv") %>% as.data.frame()
				#For right now, use alpha of .25 which is the 9th column
				chiprob <- chiprobabilities[dof,9]
				if (chival > chiprob) {
				return(paste("Degrees of Freedom: ",dof," Chi-squared value: ", chival, ".  Null hypothesis can be rejected!","Cramer V value is: ", cramer, sep=""))
				}
				else {
				return(paste("Chi-squared value: ", chival, ".  Null hypothesis can not be rejected!","Cramer V value is: ", cramer, sep=""))
				}
			}
})


output$CramersV <- renderText({req(input$whatfile);sumcramervalue()})

}


	
shinyApp(ui = ui, server = server)






