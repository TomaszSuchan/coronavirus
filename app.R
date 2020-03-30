library(shiny)
library(curl)
library(data.table)
library(ggplot2)
library(shinythemes)
library(wbstats)
library(countrycode)
library(R0)

covdat <- fread('https://covid.ourworldindata.org/data/ecdc/full_data.csv')
covdat[is.na(covdat)] <- 0
covdat$date <- as.Date(covdat$date, format = "%Y-%m-%d")
#add iso2 countrycodes
covdat$iso2c <- countrycode(covdat$location, origin = 'country.name', destination ='iso2c')
#get continents
covdat$continent <- countrycode(covdat$location, origin = 'country.name', destination ='continent')
#get population_2018
pop_data <- wb(country = unique(covdat$iso2c), indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)
covdat <- merge(covdat, pop_data[,c('iso2c', 'value')], by='iso2c')
names(covdat)[names(covdat) == "value"] <- "population"

#get values corrected for population
covdat$new_cases_percapita <- covdat$new_cases / covdat$population
covdat$new_deaths_percapita <- covdat$new_deaths / covdat$population
covdat$total_cases_percapita <- covdat$total_cases / covdat$population
covdat$total_deaths_percapita <- covdat$total_deaths / covdat$population

countries_max_cases <- aggregate(covdat$total_cases, by=list(Category=covdat$location), FUN=max)
# select only countries with 100 or more cases
countries <- countries_max_cases[countries_max_cases$x>=1,]$Category

modifdate <- max(covdat$date)

mindate <- min(covdat$date)
maxdate <- max(covdat$date)


# Define UI for app that draws a histogram ----
ui <- fluidPage(#theme = shinytheme("flatly"),
  fluidRow(
    column(12,
      h1("Coronavirus cases by country", align="center"),
      p("Data from",
              a("Our World in Data",
                href="https://ourworldindata.org/coronavirus"),
              "| Link to the dataset (last updated ",
              modifdate,
              "):",
              a("https://covid.ourworldindata.org/data/ecdc/full_data.csv",
                href = "https://covid.ourworldindata.org/data/ecdc/full_data.csv"),
              "| Shiny app by Tomasz Suchan",
              a("@tomaszsuchan",
                href="https://twitter.com/tomaszsuchan"),
              align = "center")
    )
  ),
  fluidRow(
    sidebarLayout(
      sidebarPanel(width = 2,
                   radioButtons(inputId = "data_column",
                   label = "Data to show:",
                   choices = c("Total cases" = "total_cases",
                                "New cases" = "new_cases",
                                "Total deaths" = "total_deaths",
                                "New deaths" = "new_deaths"
                                ),
                   selected = "total_cases"
                   ),
                   selectInput(inputId = "countries_sel",
                               label = "Countries (with at least 1 case):",
                               list('Europe' = unique(covdat[covdat$continent == 'Europe',]$location),
                                    'Africa' = unique(covdat[covdat$continent == 'Africa',]$location),
                                    'Americas' = unique(covdat[covdat$continent == 'Americas',]$location),
                                    'Asia' = unique(covdat[covdat$continent == 'Asia',]$location),
                                    'Oceania' = unique(covdat[covdat$continent == 'Oceania',]$location)
                                    ),
                               selected = c("France", "Italy", "Germany", "Spain", "Poland", "South Korea"
                               ),
                               multiple = TRUE
                   ),
                   strong("Plot options:"),
                   checkboxInput(inputId="log",
                                 label = "Plot y axis on log scale", value = FALSE),
                   checkboxInput(inputId="percapita",
                                 label = "Correct for population size", value = FALSE),
                   checkboxInput(inputId="dailyscale",
                                 label = "Plot daily breaks on x axis", value = FALSE),
                   checkboxInput(inputId="sync",
                                 label = "Synchronize national epidemics", value = FALSE),
                   checkboxInput(inputId="R0",
                                 label = "Sliding R0 computation", value = FALSE)
                  ),
      mainPanel(width = 9,
                fluidRow(
                  plotOutput(outputId = "distPlot", width="100%", height=750)
                ),
                fluidRow(
                  sliderInput(inputId="dates",
                               label="Dates:",
                               min = mindate,
                               max = maxdate,
                               value = c(as.Date("2020-02-15", format = "%Y-%m-%d"),maxdate),
                               timeFormat = "%F",
                               width="100%")
                )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$countries) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
###############################
    if(input$R0){
    DAT.0 = covdat[covdat$location %in% input$countries_sel, c("date", "location", "new_deaths")]
    mGT<-generation.time("gamma", c(3, 1.5))
    RES <- list()
    #
    for(c in unique(DAT.0$location)){
      DAT.1 <- DAT.0[DAT.0$location == c, ]
      DAT.1 <- DAT.1[-c(1:(which(DAT.1$new_deaths > 0)[1]-1)), ]
      DATES <- DAT.1$date
      DAT.1 <- DAT.1$new_deaths
      names(DAT.1) <- format(as.Date(as.character(DATES)), "%m/%d/%Y")
      ##
      L = length(DAT.0)
      window = 3
      #
      steps <- data.frame(
      #										"BEGIN" = c(n.steps*step - 4, n.steps[length(n.steps)]*step + 1 -1)
      #										, "END" = c(n.steps*step, L - 1)
                          J = 1:(length(DAT.1) - 1)
                          , "BEGIN" = as.numeric(NA)
                          , "END" = as.numeric(NA)
                          , "R0_point" = as.numeric(NA)
                          , "R0_low" = as.numeric(NA)
                          , "R0_high" = as.numeric(NA)
                          )
      #
    steps$BEGIN <- unlist(apply(steps, 1, function(x){
            if((x["J"] - window/2) < 0){
            return(0)
            } else {
            return(x["J"] - floor(window/2))
            }
            }
            )
            )
    #
    steps$END <- 		unlist(apply(steps, 1, function(x){
            if((x["J"] - window/2) < 0){
            return(window)
            } else {
            return(floor(x["J"] + window/2))
            }
            }
            )
            )
    #
          RES[[c]] <- steps
          for (s in (1:(dim(steps)[1]))){
              print(s)
              if(mean(DAT.1[(RES[[c]][s, "BEGIN"]):(RES[[c]][s, "END"])]) <= 10){
              RES[[c]][s, "R0_point"] <- 0
              RES[[c]][s, "R0_low"] <- 0
              RES[[c]][s, "R0_high"] <- 0
              } else {
              estR0 <- estimate.R(DAT.1, mGT
                    , begin = steps[s, "BEGIN"], end = steps[s, "END"]
                    , methods=c(
                    "EG"
                    #"ML"
                    #"TD"
                     #"AR", "SB"
                    )
                    , pop.size=100000, nsim=10000)
                    print(estR0)
                    RES[[c]][s, "R0_point"] <- estR0$estimates$EG$R
                    RES[[c]][s, "R0_low"] <- estR0$estimates$EG$conf.int[1]
                    RES[[c]][s, "R0_high"] <- estR0$estimates$EG$conf.int[2]
                    }
                  }

    }
    for(c in names(RES)){
      RES[[c]]$location <- c
    }
    RES <- do.call("rbind", RES)
  }
###############################

    if(input$sync){
          before <- which(covdat$total_cases == 0)
          covdat.sync <- covdat[-before, ]
          covdat.sync$J <- 0
          for (c in unique(covdat.sync$location)){
          				L <- dim(covdat.sync[covdat.sync$location == c, ])[1]
          				covdat.sync[covdat.sync$location == c, "J"] <- seq(length = L)
                  }
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- covdat.sync[(covdat.sync$location %in% input$countries_sel) & (covdat.sync$date %in% dates_range),]
          } else {
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- covdat[(covdat$location %in% input$countries_sel),]
          }

    ######
    myplot <- ggplot(covdat_selected) +
          #scale_color_brewer(palette="Paired", name = "Country")
          scale_color_discrete(name = "Countries:") +
          theme_linedraw(base_size = 15)
    ######
    #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
    #    covdat_selected <- covdat[(covdat$location %in% input$countries_sel) & (covdat$date %in% dates_range),]



    if(input$sync){
    if(input$percapita){
    myplot <- myplot + labs(x = "Date", y = "Number of cases per capita")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases_percapita, colour = location), size=1)}
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases_percapita, colour = location), size=1)}
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths_percapita, colour = location), size=1)}
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths_percapita, colour = location), size=1)}
    }  else {
    myplot <- myplot + labs(x = "Date", y = "Number of cases")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases, colour = location), size=1)}
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases, colour = location), size=1)}
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths, colour = location), size=1)}
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths, colour = location), size=1)}
    }
    } else if(input$R0){
        myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location)) +
        geom_point() +
        geom_line()+
        geom_ribbon(aes(ymin=R0_low, ymax=R0_high, colour = location), linetype=2, alpha=0.5)+
        theme_minimal()
      } else {
    if(input$percapita){
      myplot <- myplot + labs(x = "Date", y = "Number of cases per capita")
      if(input$data_column == "total_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases_percapita, colour = location), size=1)}
      else if(input$data_column == "new_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases_percapita, colour = location), size=1)}
      else if(input$data_column == "total_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths_percapita, colour = location), size=1)}
      else if(input$data_column == "new_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths_percapita, colour = location), size=1)}
        }
        else{
          myplot <- myplot + labs(x = "Date", y = "Number of cases")
          if(input$data_column == "total_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases, colour = location), size=1)}
          else if(input$data_column == "new_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases, colour = location), size=1)}
          else if(input$data_column == "total_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths, colour = location), size=1)}
          else if(input$data_column == "new_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths, colour = location), size=1)}
        }
      }


    if(input$log)
      myplot <- myplot + scale_y_log10()
    if(input$dailyscale)
      myplot <- myplot + scale_x_date(date_minor_breaks = "1 day")
    return(myplot)
  })
}

shinyApp(ui, server)
