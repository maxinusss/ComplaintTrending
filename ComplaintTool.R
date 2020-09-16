library('shiny')
library('ggplot2')
library('tidyverse')
library('rsconnect')
library('readr')
library('ggQC')
library('ggrepel')
library('openxlsx')



complaint <- read.xlsx("COMPLAINT_LOG/Complaintlogtrialrun.xlsx", detectDates = TRUE)
  comp <- complaint %>% rename(product = Product.Family, f_or_nf = 'Failure.(yes/no)', date = Date.of.Incident ) 
  comp$year <- format(as.Date(comp$date), "%Y")
  comp$month <- format(as.Date(comp$date), "%m")
comps = select(comp, year, month, product, f_or_nf, date, in_circ)

a <- comps %>% group_by(month, product, year) %>% count(month) %>% mutate(countn = n) %>% group_by(product, countn, month, year) %>% summarise
mergee <- merge(a, comps, by = c("month", "year", "product"))


 ## load in main data and take out duplicates ##
merge<- read_csv("COMPLAINT_LOG/mmerge.csv") %>% mutate(point = ((countn/in_circ)))
mm <- merge[!duplicated(merge), ]


## quick temp data for pie chart with proportions ##
m1 <- merge %>% group_by(product, year, f_or_nf) %>% count(f_or_nf) %>% group_by(product, year, f_or_nf, n) %>% summarise() %>% mutate(countF = n)
m2 <- merge %>% group_by(product, year) %>% count(product) %>% group_by(product, year, n) %>% summarise %>% mutate(totalcount = n)
merge3 <- merge(m1, m2, by = c("product", "year")) %>% mutate(prop = (countF/totalcount))


####Pareto Stuff ####
pareto <- merge %>% group_by(product, year, f_reason) %>% count(f_reason) %>%  group_by(product, year, f_reason, n) %>% summarise %>% mutate(count_reason = n)


############# User Interface ##################
ui <- fluidPage(
    headerPanel(img(src = "archerlogo.png", align = "right", height = 60, width = 150)),
    titlePanel("ArcherDx Product Complaints"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    mainPanel(
        tabsetPanel(
             tabPanel("One Year Ubar",
                     sidebarLayout(
                         sidebarPanel(
                             uiOutput("product"),
                             uiOutput("year")
                         ),
                         fluidPage(splitLayout(
                                   verticalLayout(
                             a(plotOutput(outputId = "ubarplot")),
                             a(tableOutput("CAPA"))))
                        )
                     )
                  ),
              tabPanel("Month-by-Month",
                     sidebarLayout(
                       sidebarPanel(
                         uiOutput("archerprod"),
                         uiOutput("yearmon"),
                         checkboxGroupInput(inputId = "mon", 
                                            label = "Choose Months (This grpah will only appear with data if more than one month is chosen)",
                                            c("January" = 'jan',
                                              "Februrary" = 'feb',
                                              "March" = "mar",
                                              "April" = 'apr',
                                              "May" = 'may',
                                              "June" = 'jun',
                                              "July" = 'jul',
                                              "August" = 'aug',
                                              "September" = 'sep',
                                              "October" = 'oct',
                                              "November" = 'nov',
                                                      "December" = 'dec'))
                       ),
                       fluidPage(splitLayout(
                                  verticalLayout(
                                    a(plotOutput(outputId = "monthly", width = "100%")),
                                    a(tableOutput("CAPA_month"))
                              )
                            )
                         )
                       )
                    ),
              tabPanel("Year-by-Year",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("archerp"),
                           uiOutput("years")
                         ),
                         fluidPage(splitLayout(
                           verticalLayout(
                             a(plotOutput(outputId = "yearly", width = "100%")),
                             a(tableOutput("CAPA_month_year"))
                        )
                      )
                    )
                  )
                ),
              tabPanel("Pie Chart",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("pieprod"),
                           uiOutput("pieyear")
                         ),
                         mainPanel("Pie Chart Displaying Percentage of Failure vs. Nonfailure Reasons for Complaints",
                                   plotOutput(outputId = "piechart")
                          )
                       )
                    ),
              tabPanel("Failures Pareto", 
                       sidebarLayout(
                         sidebarPanel(
                         uiOutput("paretoprod"),
                         uiOutput("paretoyear")
                          ),
                         mainPanel(
                           plotOutput(outputId = "pareto")
                         )
                       )
                    ),
              tabPanel("Date to/From",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("archerr"),
                           dateRangeInput("dates", label = "Date range"),
                           hr(),
                           fluidRow(column(4, verbatimTextOutput("value")))
                           ),
                         mainPanel( plotOutput(outputId = "calendar"))
                      ) 
                  )
              )),
    tags$footer("Contains Confidential and Proprietary Information", align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: dimgray;
              padding: 10px;
              background-color: white;
              z-index: 1000;"))

############### Backend ###############
server <- function(input, output) {
    
    ############ Reactive UI Output Option Creation (Goes into the dataset and pulls unique options from data for sidepanel bars in app#############
    output$product <- renderUI ({
        selectInput(inputId = "product",
                    label = "Choose Archer Product",
                    sort(unique(mm$product)))
    })
    output$year <- renderUI({
        selectInput(inputId = "year",
                    label = "Choose Year",
                    sort(unique(mm$year)))
    
    })
    output$yearmon <- renderUI({
      selectInput(inputId = "yearmon",
                  label = "Choose Year",
                  sort(unique(mm$year)))
    })
    output$archerprod<- renderUI ({
      selectInput(inputId = "archerprod",
                  label = "Choose Archer Product",
                  sort(unique(mm$product)))
    })
    output$archerp <- renderUI ({
      selectInput(inputId = "archerp",
                  label = "Choose Archer Product",
                  sort(unique(mm$product)))
    })
    output$years <- renderUI({
      checkboxGroupInput(inputId = "years",
                         label = "Choose year",
                         sort(unique(mm$year)))
    })
    output$pieprod <- renderUI ({
      selectInput(inputId = "pieprod",
                  label = "Choose Archer Product",
                  sort(unique(mm$product)))
    })
    output$pieyear <- renderUI({
      selectInput(inputId = "pieyear",
                  label = "Choose year",
                  sort(unique(mm$year)))
    })
    output$archerr<- renderUI ({
      selectInput(inputId = "archerr",
                  label = "Choose Archer Product",
                  sort(unique(mm$product)))
    })
    output$paretoprod<- renderUI ({
      selectInput(inputId = "paretoprod",
                  label = "Choose Archer Product",
                  sort(unique(mm$product)))
    })
    output$paretoyear <- renderUI({
      selectInput(inputId = "paretoyear",
                         label = "Choose year",
                         sort(unique(mm$year)))
    })
    
    ############### Plot/Table Creation #################
  
   output$ubarplot <- renderPlot({
        ggplot(mm %>%
                  filter(product == input$product) %>%
                  filter(year %in% input$year) %>% 
                  mutate(point = (countn/in_circ)) %>%
                  group_by(product, year) %>% 
                  mutate(ubar = ((sum(countn))/(sum(in_circ)))) %>%
                  group_by(product, year, month, countn, in_circ, point) %>% 
                  mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%
                  mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))))+
          aes(x=month, y=point, group = product)+
          geom_line(aes (y=ucl, x=month), color = 'blue')+
          geom_line(aes(y=ucl2, x=month), color = 'blue', linetype = 'dashed')+
          geom_line(aes(y=ubar, x=month), color = "red")+
          geom_point()+
          geom_line()+
          geom_label_repel(data = (. %>% filter(point > ucl2) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "orange", nudge_x=.5, nudge_y = .2, size = 5)+
          geom_label_repel(data = (. %>% filter(point > ucl) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "red", nudge_x=.5, nudge_y = .2, size = 5)+       
          geom_label(data = (. %>% filter(month=="dec") %>% group_by(month, point, product, year, in_circ, countn, ucl) %>% summarise), aes(x="dec", y=ucl, label = "UCL"), color = "blue", nudge_x = .3)+
          geom_line(aes(y=ubar, x=month), color = 'red')+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=point), color = "orange", size = 3)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=point), color = "red", size = 3)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.04)), fill = "orange", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.05)), fill = "orange", color = "white", shape = 33, size = 5)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.04)), fill = "red", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.05)), fill = "red", color = "white", shape = 33, size = 5)+theme_bw()+
          scale_x_discrete(limits = c("jan", 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))+
          labs(x = 'Month', y='Percentage of Complaints') 
   })
    
    output$CAPA <- renderTable(mm %>%
            filter(product == input$product) %>%
            filter(year %in% input$year) %>% 
            mutate(point = (countn/in_circ)) %>%
            group_by(product, year) %>% 
            mutate(ubar = ((sum(countn))/(sum(in_circ)))) %>%
            mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%
            mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))) %>%
            filter(point > ucl2) %>% 
            mutate(CAPA = "Consider CAPA") %>% 
            within(CAPA[point > ucl] <-"Initiate CAPA") %>% 
            group_by(product, year, month, countn, point, ucl, CAPA) %>% 
            mutate(point = paste0(round(point*100), "%")) %>% 
            mutate(ucl = paste0(round(ucl*100), "%")) %>% 
            mutate(year = paste0(round(year))) %>% 
            mutate(countn = paste0(round(countn))) %>% 
            summarise()
            )
    
    
    output$monthly <- renderPlot({
        ggplot(mm %>% 
                filter(product == input$archerprod) %>% 
                filter(year %in% input$yearmon) %>% 
                filter(month %in% input$mon)%>% 
                mutate(point = (countn/in_circ)) %>%
                group_by(product, year) %>% 
                mutate(ubar = ((sum(countn))/(sum(in_circ)))) %>%
                group_by(product, year, month, countn, in_circ, point) %>% 
                mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%
                mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))))+
          aes(x=month, y=point, group = product)+
          geom_line(aes (y=ucl, x=month), color = 'blue')+
          geom_line(aes(y=ucl2, x=month), color = 'blue', linetype = 'dashed')+
          geom_line(aes(y=ubar, x=month), color = "red")+
          geom_point()+
          geom_line()+
          geom_label_repel(data = (. %>% filter(point > ucl2) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "orange", nudge_x=.5, nudge_y = .2, size = 5)+
          geom_label_repel(data = (. %>% filter(point > ucl) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "red", nudge_x=.5, nudge_y = .2, size = 5)+       
          geom_line(aes(y=ubar, x=month), color = 'red')+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=point), color = "orange", size = 3)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=point), color = "red", size = 3)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.04)), fill = "orange", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.05)), fill = "orange", color = "white", shape = 33, size = 5)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.04)), fill = "red", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.05)), fill = "red", color = "white", shape = 33, size = 5)+theme_bw()+
          scale_x_discrete(limits = c("jan", 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))+
          labs(x = 'Month', y='Percentage of Complaints') 
    })
    
    output$CAPA_month <- renderTable(mm %>%
            filter(product == input$archerprod) %>% 
            filter(year %in% input$yearmon) %>% 
            filter(month %in% input$mon)%>% 
            mutate(point = (countn/in_circ)) %>%
            group_by(product, year) %>% 
            mutate(ubar = ((sum(countn))/(sum(in_circ)))) %>%
            mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%
            mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))) %>%
            filter(point > ucl2) %>% 
            mutate(CAPA = "Consider CAPA") %>% 
            within(CAPA[point > ucl] <-"Initiate CAPA") %>% 
            group_by(product, year, month,countn, point,ucl, CAPA) %>% 
            mutate(point = paste0(round(point*100), "%")) %>% 
            mutate(ucl = paste0(round(ucl*100), "%")) %>% 
            mutate(year = paste0(round(year))) %>% 
            mutate(countn = paste0(round(countn))) %>% 
            summarise()
            )
    
    output$yearly <- renderPlot({
      ggplot(mm %>% 
               filter(product == input$archerp) %>% 
               filter(year %in% input$years) %>% 
               group_by(product, year) %>% mutate(point = (countn/in_circ)) %>%  group_by(product, year) %>% 
               mutate(ubar = ((sum(countn))/(sum(in_circ))))  %>% group_by(product, year, month, countn, in_circ, point) %>% 
               mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%   mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))))+
          aes(x=month, y=point, group = product)+
          geom_line(aes (y=ucl, x=month), color = 'blue')+
          geom_line(aes(y=ucl2, x=month), color = 'blue', linetype = 'dashed')+
          geom_line(aes(y=ubar, x=month), color = "red")+
          geom_point()+
          geom_line()+
          geom_label_repel(data = (. %>% filter(point > ucl2) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "orange", nudge_x=.5, nudge_y = .2, size = 5)+
          geom_label_repel(data = (. %>% filter(point > ucl) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "red", nudge_x=.5, nudge_y = .2, size = 5)+       
          geom_line(aes(y=ubar, x=month), color = 'red')+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=point), color = "orange", size = 3)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=point), color = "red", size = 3)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.04)), fill = "orange", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl2)), aes(x=month, y=(point+.05)), fill = "orange", color = "white", shape = 33, size = 5)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.04)), fill = "red", shape = 24, size = 7)+
          geom_point(data = (. %>% filter(point > ucl)), aes(x=month, y=(point+.05)), fill = "red", color = "white", shape = 33, size = 5)+
          scale_x_discrete(limits = c("jan", 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))+
          facet_wrap(~year, ncol = 2)+
          theme_bw()
    })
    
    output$CAPA_month_year <- renderTable(mm %>%
           filter(product == input$archerp) %>% 
           filter(year %in% input$years) %>% 
           group_by(product, year) %>% mutate(point = (countn/in_circ)) %>%  
           group_by(product, year) %>% 
           mutate(ubar = ((sum(countn))/(sum(in_circ))))  %>% 
           group_by(product, year, month, countn, in_circ, point) %>% 
           mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%   
           mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))) %>% 
           filter(point > ucl2) %>% 
           mutate(CAPA = "Consider CAPA") %>% 
           within(CAPA[point > ucl] <-"Initiate CAPA") %>% 
           group_by(product, year, month,countn, point,ucl, CAPA) %>% 
           mutate(point = paste0(round(point*100), "%")) %>% 
           mutate(ucl = paste0(round(ucl*100), "%")) %>% 
           mutate(year = paste0(round(year))) %>% 
           mutate(countn = paste0(round(countn))) %>% 
           summarise()
           )
    
    output$pareto <- renderPlot({
      ggplot(pareto %>% 
          filter(product == input$paretoprod) %>% 
          filter(year %in% input$paretoyear)) +
          aes(x=f_reason, y= count_reason) +
          stat_pareto(bars.fill = c("red", "blue"))+
          theme_bw()+
          theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5, size =15))+
          labs(x = "Reason", y = "Count")
    }, height = 800, width =900, res = 80)
    
    output$piechart <- renderPlot({
      ggplot(merge3 %>% filter(product == input$pieprod) %>% filter(year == input$pieyear))+
        aes(x = "", y=prop,  fill = f_or_nf,)+
        geom_bar(width = 1, stat = 'identity', color = 'black')+
        coord_polar("y", start = 0)+
        geom_text(aes(label = paste0(round(prop*100), "%","\n       Count:", paste0(countF))), position = position_stack(vjust = 0.5))+
        labs(x= "", y="")+
        scale_fill_discrete(name = "", labels = c("Failure", "Not Failure"))+
        theme_void()+
        theme(legend.key.size = unit(2, "cm"),
              legend.key.width = unit(1,"cm"))
    }, height = 600, width = 800, res = 80)
    
    output$calendar <- renderPlot ({
      ggplot(mergee %>%
               filter(product == input$archerr) %>% 
               filter(date > input$dates[1]) %>% 
               filter(date < input$dates[2]) %>% 
               mutate(point = (countn/in_circ)) %>%
               group_by(product, year) %>% 
               mutate(ubar = ((sum(countn))/(sum(in_circ)))) %>%
               group_by(product, year, month, countn, in_circ, point) %>% 
               mutate(ucl = (ubar+(3*(sqrt(ubar/in_circ))))) %>%
               mutate(ucl2 = (ubar+(2*(sqrt(ubar/in_circ))))))+
          aes(x=date, y=point, group = product)+
        geom_line(aes (y=ucl, x=date), color = 'blue')+
        geom_line(aes(y=ucl2, x=date), color = 'blue', linetype = 'dashed')+
        geom_line(aes(y=ubar, x=date), color = "red")+
        geom_point()+
        geom_line()+
        geom_label_repel(data = (. %>% filter(point > ucl2) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "orange", nudge_x=.5, nudge_y = .2, size = 5)+
        geom_label_repel(data = (. %>% filter(point > ucl) %>%  group_by(month, point, product, year, in_circ, countn) %>% summarise), aes(label = countn),fill = "red", nudge_x=.5, nudge_y = .2, size = 5)+       
        geom_line(aes(y=ubar, x=month), color = 'red')+
        geom_point(data = (. %>% filter(point > ucl2)), aes(x=date, y=point), color = "orange", size = 3)+
        geom_point(data = (. %>% filter(point > ucl)), aes(x=date, y=point), color = "red", size = 3)+
        geom_point(data = (. %>% filter(point > ucl2)), aes(x=date, y=(point+.04)), fill = "orange", shape = 24, size = 7)+
        geom_point(data = (. %>% filter(point > ucl2)), aes(x=date, y=(point+.05)), fill = "orange", color = "white", shape = 33, size = 5)+
        geom_point(data = (. %>% filter(point > ucl)), aes(x=date, y=(point+.04)), fill = "red", shape = 24, size = 7)+
        geom_point(data = (. %>% filter(point > ucl)), aes(x=date, y=(point+.05)), fill = "red", color = "white", shape = 33, size = 5)+ 
        theme_bw()
    })
}
########## Puts app together and tells Shiny what is the UI and what is the Server #############
shinyApp(ui = ui, server = server)


