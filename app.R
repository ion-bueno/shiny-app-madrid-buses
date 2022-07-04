library(shiny)
library(readxl)
library(dplyr)
library(tmap)
library(ggplot2)
library(sf)
library(rgdal)
library(forcats)


data = readxl::read_xlsx('data/NodesLines.xlsx')
coords = read.csv('data/coords.csv')

data1 = read.csv('data/stops.csv', sep = ',')
data2 = read.csv('data/nstops_by_line.csv', sep = ',')
data3 = read.csv('data/distances.csv', sep = ',')
data4 = data.frame(distance=data3$distance_km, n_lines=data1$n_lines)

data$lat = coords$latitude
data$long = coords$longitude

data$N_Lines <- sapply(data$Lines, function(x) length(unlist(strsplit(as.character(x), " "))))

tmap_info = read.csv("data/Ai_tmap.csv")
tmap_info$Or = as.character(tmap_info$Or)

tmap_info = tmap_info %>%
  mutate(K.FreeFlow = FreeFlow*1000)

tmap_info = tmap_info %>% select(1, 6)


stops = data %>%
  st_as_sf(coords=c('long', 'lat'))

shp = readOGR("data/Madrid_TAZ.shp",
              layer = "Madrid_TAZ", GDAL1_integer64_policy = TRUE)


Free.Flow <- merge(
  
  shp,
  
  # read csv file add a new column to be displayed in popup
  tmap_info,
  
  # matching columns
  by.x = "TAZ_Madr_1", by.y = "Or")



#tmap_last() %>% 
#  tmap_save("data/bus_stops.html")







ui <- fluidPage(
  
  # App title ----
  titlePanel("Final Assignment"),
  sidebarLayout(
    position = 'left',
    sidebarPanel = sidebarPanel(
      img(src = "uc3m.jpg", height = 40, width = 400),
      h3('Network analysis and data visualization'),
      h4('Master in Big Data Anaytics'),
      h5('Ion Bueno Ulacia, NIA: 100364530'),
      h5('Daniel Martín Cruz, NIA: 100384121')
    ),
    
    mainPanel = mainPanel(
      div(style = "width: 90%; margin-bottom: 30px;",
        
        h1("Madrid EMT Bus Stops and Lines"),
        
        p(paste('The goal of the shiny app is getting some visual insights about',
                'information related with bus stops and lines from the city of Madrid. The datasets',
                'that contain this information are provided by the EMT in the following '),
          a("link", href="https://opendata.emtmadrid.es/Datos-estaticos/Datos-generales"),
          '.',
          'The data was published in 2015 and is divided in 4 different topics:',
          tags$ul(tags$li(strong("Calendar: "), 'contains information about the week days and seasons when each line is available. It will not be used for any visualization.'), 
                  tags$li(strong("Groups: "), 'here we can find sets of lines that belong to the same group and a brief description of the purpose of each of them.'),
                  tags$li(strong("Lines: "), 'in this file there is information about all the lines that are a part of the Madrid buses system indicating the first and last stop as well as its start and end date.'),
                  tags$li(strong("Stops: "), 'this datafile contains the position, name and lines that go through each of the bus stops in the city of Madrid.')),
          'Let us focus now with the visualization of the information.'),
        
        
        
        # Map
        h2("1. Map of the city"),
        
        p('We will start with a map showing all the bus stops of the city of Madrid, as bubbles of ',
          'size proportional to the number of lines going through them. Apart from this, it ',
          'is also possible to see the name of the stop and which lines are connected with it by ',
          'clicking in the corresponding bubble. The map is divided in neighborhoods and by clicking ',
          'each one you can also see, among other things, the population living in that ',
          'area in 2017.'),
        p('By default, the map shows the complete set of bus stops existing in the city, ',
          'but it is also possible to filter the huge network stops in order to see just one line. ',
          'To do this you only have to type the line you want to see and click the filter checkbox. ',
          'There are 209 bus lines and the one that has a larger identifier number is labeled as ',
          'the ', em('line 799.'), 'Mention that stops plotted per line belong to a round trip, ',
          'for example, in line ', em('161,'), 'the corresponding stops could belong to both directions, ',
          em('161/1'), ' or ', em('161/2.'), 'For the rest of plots, the direction is considered in order ',
          'to be more precise in the study.'),
        

        
        
        checkboxInput('filter', 'Filter line'),
        numericInput('line', 
                     'Select line',
                     1,
                     min=1,
                     max=799,
                     step=1),
        tmapOutput("map"),
        
        
        p('This map is really helpful in order to be aware of the large number of bus stops ',
          'there are in Madrid. Apart from that, it can be also useful if you want ',
          'to know the route followed by each of the bus lines in the city.'),
        
        p('When we first saw this map, we had the sensation that the bus stops situated in the ',
          'city center were represented with bubbles of a bigger size, or what is the same, ',
          'that the there are more lines passing in each stop in the city center than in the ',
          'periphery of Madrid. This idea motivated the next plot of the report.'),
        
        
        # Scatter plot
        h2("2. Do the bus stops of the city center have more lines going through them?"),
        
        p('In order to answer that question, we decided to produce a scatter plot with all the available stops. The ',
          'x-axis corresponds with the amount of lines going through the stop and the y-axis with the ', 
          'distance in kilometers to the city center, which corresponds with the Sol square in this case.',
          'We have also added the mean using the ', code('geom_smooth()'), ' function. ',
          'The confidence interval can be selected as input of the plot. Mention how it is wider when less samples are present.'),
        
        sliderInput(inputId = "conf_level",
                    label = "Confidence level:",
                    min = 0,
                    max = 1,
                    value = 0.95
        ),
        plotOutput(outputId = "scatter"),
        
        
        p('We can observe a wide range of different distances when the number of lines is low ',
          '(between 0 and 7 approximately). When the amount of lines per stop reaches around the number of ',
          '10, we see that those stops are generally close. When there are more than 15 lines, ',
          'all those stops are around 1km with respect the Sol square. Consequently, we can state',
          'that a few number of EMT lines connect bus stops that are far from the center, limiting mobility possibilities.'),
        
        
        p('Mention that this plot is a bit confused since the amount of lines is a discrete variable',
          'and most of samples are between 0 and 5 lines. In order to produce a clearer view, we summarise',
          'useful information of this visualization in next section.'),
        
        
        
        # Barplot 
        h2("3. Summary amount of lines with respect to the distance to Madrid center"),
        
        p('Now we will show a barplot in a similar way as the previous case. You can decide ',
          'the number of sets (lines per stop) you want to plot. In addition, the',
          strong('Statistic'), 'input is used to ',
          'select how you want to aggregate the stops in that set (average, median, maximum or minimum).'),
        
        
        sliderInput(inputId = "number_of_nlines",
                    label = "Number of sets:",
                    min = 1,
                    max = 19,
                    value = 10
        ),
        selectInput("stat", "Statistic:",
                    c("Average" = "avg",
                      "Median" = "med",
                      "Maximum" = "max",
                      "Minimum" = "min"
                    )
        ),
        plotOutput(outputId = "barplot"),
        
        
        p('In this plot we can get a similar conclusion as the one we had with the previous section. ',
          'The only exception is when you show the minimum of all the stops, which is very irregular because ',
          'the correlation of the distance with the amount of lines is not so clear in this case, ',
          'as it can be seen in the scatter plot of section 2.'),
  
        
        # Histogram
        h2("4. Distribution of number of lines per stop"),
        
        p('In this section we will not take into account the influence of distance and ',
          'we will just focus on the on the distribution of the amount of lines per stop. We ',
          'plot a histogram of the number of lines versus the rate of stops included in that ',
          'bin. The number of bins can be selected by the user. Mention we overlap the estimated density function.'),
        
        
        
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 10
        ),
        plotOutput(outputId = "histogram"),
        
        
        p('We can see an exponential distribution with a clear dominance of stops that present',
          ' less than 3 lines. When the x-axis reaches around 5 lines in the stop the ',
          'rate drops to almost 0.'),
        
        p('Consequently, we can state that most of stops are not very well connected to a lot of lines ',
          'and the main purpose of a bus stop is being used for a few lines or even only one.'),
        
        
        
        # Boxplot
        h2("5. Distribution of number of stops per group line"),
        
        p('In the last section we are going to study the impact of the group of the line with ',
          'respect to the length of that line. We will estimate that length in terms of the number ',
          'of stops of the line. The way to represent this information in a visual way will be ',
          'by means of a boxplot per group of lines. The possible groups are coded as numbers of three digits,',
          'where each line belongs to a subgroup. In this case we merge lines belonging to the same group ',
          'according to their subgroups. The possible groups defined in the ', strong('Groups'),' dataset are:',
          
          tags$ul(tags$li(strong("100: "), 'conventional lines, lines of work centres, etc.'), 
                  tags$li(strong("300: "), 'night lines.'),
                  tags$li(strong("200: "), 'university lines.'),
                  tags$li(strong("Others: "), 'special lines.')),
          
          'The order followed to plot the groups is according to the number of lines belonging to them.'
          ),
        
        
        plotOutput(outputId = "boxplot"),
        
        
        p('At a first sight, we can see a remarkable difference in the range of the first two boxplots ',
          'with respect to the other two. This is because there is a considerably higher number of lines ',
          'in these two groups. Moreover, there is still a big difference in terms of length, ',
          'having longer lines in groups 100 and 300. The reason of this is that lines that belong ',
          'to groups 200 and ', em('other'), ' are dedicated to universities and exceptional uses, while group 100, which ',
          'corresponds to conventional lines, and group 300, which are night bus routes, are ',
          'generally very long to cover a big part of the city with a reduced service. '),
        
          p('Mention also the difference between group 100 and 300, where in general there are more stops in the ',
          'night lines. The reason is that these buses are often used to cover very long distances ',
          'since the service is very reduced due to the small amount of people who make use of it.'),
        

      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$map <- renderTmap({
    
    
    if(input$filter){
      
      str1 = paste('^', input$line, '/', sep='')
      str2 = paste(' ', input$line, '/', sep='')
      
      data_filtered = data[with(data, grepl(str1, Lines)) | with(data, grepl(str2, Lines)),]
      
      t = paste("Bus stops of line", input$line)
      
      if(length(data_filtered$Node) == 0){
        data_filtered = data
        t = "Showing all bus stops because input line does not exist"
      }
      
    }else{
      data_filtered = data
      t = "All bus stops"
    }
    
    stops = data_filtered %>%
      st_as_sf(coords=c('long', 'lat'))
    
    tm_shape(Free.Flow) +
      tm_fill(alpha = 0.3, col='white') +
      tm_borders(alpha= 0.5) +
      tm_shape(stops) +
      tm_bubbles( size='N_Lines', scale=.3, id='Name', popup.vars=c('Lines'), col='red') +
      tm_layout(title = t)
  })
  
  output$histogram <- renderPlot({
    
    data.frame(x=data1$n_lines) %>%
      ggplot(aes(x=x)) +
      geom_histogram(aes(y=..density..),
                     bins=input$bins, fill="indianred3", color="black", alpha=0.9) +
      geom_density(adjust=5,
                   alpha=.5, fill="skyblue3") +
      theme(plot.title = element_text(size=15)) +
      theme_bw() + guides(fill="none") +
      labs(x='Amount of lines',
           y='Rate of stops',
           title='Number of lines presented per stop'
      )
    
  })
  
  output$boxplot <- renderPlot({
    
    data.frame(n_stops=data2$n_stops, group=data2$group) %>%
      mutate(group = fct_reorder(group, n_stops, .fun='length', .desc=TRUE)) %>%
      ggplot(aes(x=group, y=n_stops, fill=group)) + 
      geom_boxplot() +
      scale_fill_hue(c = 70) +
      #scale_fill_manual(values = c("brown2", "chartreuse2", "deepskyblue2", "gold2")) +
      theme_bw() + guides(fill="none") +
      labs(x='Lines groups',
           y='Amount of stops',
           title='Number of stops per line and group',
           subtitle='Ordered by number of lines, from larger to smaller')
    
  })
  
  
  output$scatter<- renderPlot({
    
    data4 %>%
      ggplot(aes(y=distance, x=n_lines)) + 
      geom_point(colour='red3') +
      geom_smooth(color='black', fill='indianred3',
                  level=input$conf_level) +
      theme_bw() +
      #ylim(0,15) +
      labs(x='Amount of lines',
           y='Distance (km)',
           title='Distance to Madrid center from each stop')
    
  })
  
  output$barplot<- renderPlot({
    
    metric_toplot = input$stat
    
    df_filtered = data4 %>% 
      group_by(n_lines) %>% 
      filter(n_lines <= input$number_of_nlines)
    
    if(metric_toplot=='avg'){
      df = df_filtered %>% 
        summarise(metric = mean(distance))
      title = 'Average distance to Madrid center from each stop'
      
    }else if(metric_toplot=='med'){
      df = df_filtered %>% 
        summarise(metric = median(distance))
      title = 'Median distance to Madrid center from each stop'
      
    }else if(metric_toplot=='max'){
      df = df_filtered %>% 
        summarise(metric = max(distance))
      title = 'Maximum distance to Madrid center from each stop'
    }else{
      df = df_filtered %>% 
        summarise(metric = min(distance))
      title = 'Minimum distance to Madrid center from each stop'
    }
    
    df %>%
      ggplot(aes(x=as.factor(n_lines), 
                 y=metric,
                 fill=as.factor(n_lines))) + 
      geom_bar(stat="identity") +
      scale_fill_hue(c = 50) +
      theme_bw() + guides(fill="none") +
      geom_text(aes(label=round(metric, 2)), 
                vjust=1.6, color="white",
                position = position_dodge(0.9), 
                size=3.5) +
      labs(x='Amount of lines',
           y='Distance (km)',
           title=title,
           subtitle='Ordered by number of lines')
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
