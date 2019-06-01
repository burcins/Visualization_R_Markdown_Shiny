setwd("E:/dersler/3.Spring/practicum_2/Assignment_1")
library(shiny)
library(tidyverse)

#####READ & CLEAN DATASETS####
#### Excel Data
suicide1 <- readxl::read_xls("suicide1.xls", skip=2)
colnames(suicide1)[1] <- "GEO"
suicide1 <- reshape2::melt(suicide1, id.vars=c("GEO"), variable.name = "Year", 
               value.name = "Total")

suicide1_2 <- group_by(suicide1, GEO)
suicide1_2 <- suicide1_2[ ! suicide1_2$GEO %in% "European Union (current composition)", ]


###### Txt data
suicide2 <- read.csv(text=gsub("(^\\|\\$)", "", 
                               readLines("suicide II OECD.csv.txt", 
                                         skipNul = T)))
colnames(suicide2) <- "LOCATION,INDICATOR,SUBJECT,MEASURE,FREQUENCY,TIME,Value,Flag.Codes"

suicide2 <- separate(suicide2, colnames(suicide2), 
                     into = c("LOCATION","INDICATOR","SUBJECT",
                              "MEASURE","FREQUENCY","TIME","Value",
                              "Flag.Codes"), sep=",")
suicide2 <- suicide2[-c(2:5,8)]

suicide2$TIME <- as.numeric(gsub("(^\"|\"$)", "", suicide2$TIME))
suicide2$Value <- as.numeric(suicide2$Value)


###### VISUALISATION ######

## Years total by countries
a <- aggregate(suicide1_2$Total, by=list(suicide1_2$GEO), sum)
ggplot(a, aes(x,Group.1)) + geom_jitter()+
  labs(title="Total Nr. by Countries(2011-2015)", x="Total Suicide Nr.")
  

## Average and variation by Countries
a <- suicide1_2 %>% group_by(GEO) %>%
  summarise(mean=mean(Total), sd=sd(Total))

ggplot(a, aes(log(mean), log(sd))) + geom_jitter(aes(col=GEO))+
  labs(title="Average vs Variation btw Countries(2011-2015)", y="Variation btw 2011-2015", x="Log of Average Suicide Nr.")+
  ggrepel::geom_text_repel(aes(label=GEO), size=2, data=a) + 
  theme(legend.position = "None") 

## By year1
ggplot(suicide1_2, aes(Total, GEO, col=GEO)) + geom_jitter()+
  labs(title="Total Nr. by Year & Countries", x="Total Suicide Nr.")

## By year2
ggplot(suicide1_2, aes(Total, GEO, col=Total)) + geom_jitter()+
  labs(title="Total Nr. by Year & Countries", x="Total Suicide Nr.")+
  facet_grid(suicide1_2$Year)+ theme(axis.text.x=element_text(size=8, angle = 90))+
  ggrepel::geom_text_repel(aes(label=GEO), size=2, data=suicide1_2) + 
  theme(legend.position = "None") +
  xlim(c(0, max(suicide1_2$Total)))+scale_color_gradient(low="blue", high="red")


#### Counties total btw 2004-2014
a <- aggregate(suicide2$Value, by=list(suicide2$LOCATION), mean)
ggplot(a, aes(x,Group.1)) + geom_jitter()+
  labs(title="Total Nr. by Countries(2004-2014)", x="Total Suicide Nr.per 100.000 people")


## Average and variation by Countries in Long Run
a <- suicide2 %>% group_by(LOCATION) %>%
  summarise(mean=mean(Value), sd=sd(Value))

ggplot(a, aes(mean, sd)) + geom_jitter(aes(col=LOCATION))+
  labs(title="Average vs Variation btw Countries(2004-2014)", 
       y="Variation btw 2004-2014", x="Average Suicide Nr.per 100.000 people")+
  ggrepel::geom_text_repel(aes(label=LOCATION), size=2, data=a) + 
  theme(legend.position = "None") 

## By year
ggplot(suicide2, aes(Value, LOCATION, col=Value)) + geom_jitter()+
  labs(title="Total Nr. by Year & Countries", x="Total Suicide Nr.")+
  facet_grid(suicide2$TIME)+  theme(axis.text.x=element_text(size=8, angle = 90))+
  ggrepel::geom_text_repel(aes(label=LOCATION), size=2, data=suicide2) + 
  theme(legend.position = "None")+ scale_color_gradientn(colours = rainbow(5))

####Only Greece

a <- suicide1 %>% filter(suicide1$GEO=="Greece")
ggplot(a, aes(Year, Total)) + geom_jitter()

a <- suicide2 %>% filter(suicide2$LOCATION=="GRC")
ggplot(a, aes(TIME, Value)) + geom_jitter()+ 
geom_smooth(method="loess", se=F) + scale_x_discrete(limits = c(2004,2006,2008,2010,2012,2014))

#### Dynamic plot provides country selection ## Shiny package

ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Suicide Frequency by County & Year"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput(inputId = "type", label = strong("Trend by Country"),
                  choices = unique(suicide2$LOCATION),
                  selected = "GRC")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("Plot")  
    )
    
  )
)

server <- function(input, output) {
  
  # Fill in the spot created for a plot
  # Render a barplot
  output$Plot <- renderPlot({
    a <- suicide2 %>% filter(suicide2$LOCATION==input$type)
    
    ggplot(a, aes(TIME, Value)) + geom_jitter()+ 
      labs(title=paste("Total Nr. by Year",input$type), x="Year",y= "Suicide frequency per 100.000 people")+
      geom_smooth(method="loess", se=F) + scale_x_discrete(limits = c(2004,2006,2008,2010,2012,2014))
  
  })
}

shinyApp(ui = ui, server = server)





