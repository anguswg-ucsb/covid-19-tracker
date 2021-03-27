# Fonts and label for plotly
font = list(
  family = 'Arial',
  size = 15,
  color = 'white')
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font)

### ------------- FUNCTIONS ------------- ###

### ------------- LOAD COUNTY & POP DATA  ------------- ###
counties = readRDS("counties.rds")
pop = readxl::read_excel('PopulationEstimates.xls', skip = 2) %>%
  select(state = State, pop_19 = POP_ESTIMATE_2019, fips = FIPStxt)
pop$fips <- as.numeric(pop$fips)

### ------------ READ IN COVID 19 TIMESERIES URL ------------ ###
read_covid19 = function(){
  url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  cc = read.csv(url, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date),
           fips = as.numeric(fips),
           name = paste0(county, " County, ", state))
}

### ------------ COUNTY CENTROIDS ------------ ###
# Join County Data with
today_centroids = function(counties, covid_data){
  filter(covid_data, date == max(date)) %>%
    left_join(st_centroid(counties), by = 'fips') %>%
    na.omit() %>%
    mutate(size = abs(cases - mean(cases)) / sd(cases), size2 = abs(deaths - mean(deaths)) / sd(deaths)) %>%
    st_as_sf()
}
today_centroids_2 <- function(counties, pop, covid19) {
  rollmean_map <- covid19 %>%
    left_join(st_centroid(counties), by = 'fips') %>%
    na.omit()
  rollmean_map <- rollmean_map %>%
    group_by(county, state) %>%
    filter(date > max(date) - 8) %>%
    arrange(date) %>%
    mutate(new_cases = cases - lag(cases))
  rollmean_map = inner_join(rollmean_map, select(pop, pop_19, fips), by = 'fips')
  rollmean_map <- rollmean_map %>%
    mutate(cases_per_100k = (new_cases /pop_19)*100000) %>%
    na.omit() %>%
    mutate(rmean_new = rollmean(new_cases, 7, fill = NA, align = 'right'),
           rmean_percap = rollmean(cases_per_100k, 7, fill = NA, align = 'right')) %>%
    na.omit()
  rollmean_map <- rollmean_map %>%
    filter(rmean_new >= 0.0,
           rmean_percap >= 0.0) %>%
    ungroup() %>%
    mutate(size2 = abs(rmean_new - mean(rmean_new)) / sd(rmean_new),
           size3 = abs(rmean_percap - mean(rmean_percap)) / sd(rmean_percap)) %>%
    filter(state != "Alaska") %>%
    st_as_sf()
}

### ------------ MAP FUNCTIONS ------------ ###
basemap = function(today){
  pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
  pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)
  leaflet(data = today) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar("bottomleft") %>%
    addCircleMarkers(
      fillColor = ~pal(size),
      color = 'black',
      weight = 0.2,
      fillOpacity = 0.5,
      radius = ~size*2,
      layerId = ~fips,
      label   = ~name) %>%
    addLegend("bottomright",
              pal = pal2,
              values = ~cases,
              title = paste("COVID Cases\n", max(today$date)),
              opacity = 1) %>%
    setView(lng = -98, lat= 38, zoom = 4)
}
roll_mean_map <- function(today_2) {
  pal3 = colorNumeric("viridis", reverse= TRUE, domain = today_2$size3)
  pal4 <- colorNumeric("viridis", reverse = TRUE, domain = today_2$rmean_percap)
  leaflet(data = today_2) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar("bottomleft") %>%
    addCircleMarkers(
      fillColor = ~pal3(size3),
      color = 'black',
      weight = 0.2,
      fillOpacity = 0.5,
      radius = ~size3*2,
      layerId = ~fips,
      label   = ~name) %>%
    addLegend("bottomright",
              pal = pal4,
              values = ~rmean_percap,
              title = paste("New cases/100k \n", max(today_2$date)),
              opacity = 1) %>%
    setView(lng = -98, lat= 38, zoom = 4)
}

# zoom to selected county
zoom_to_county = function(map, counties, FIP){
  # Filter the counties to the input FIP code
  shp = filter(counties, fips == FIP)
  # Build a buffered bounding box to center the map on:
  bounds = shp %>%
    # make bounding box
    st_bbox() %>%
    # Make spatial
    st_as_sfc() %>%
    # Buffer to .1 degree
    st_buffer(.1) %>%
    # make new bounding box
    st_bbox() %>%
    # extract coordinates as vector
    as.vector()
  # Clear all current shapes (remember county centroids are currently markers!)
  clearShapes(map) %>%
    # Add the county shape making the outline color red and the fill an opaque white
    addPolygons(data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
    # Fly the leaflet map to the buffered boundary
    flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}

### ------------ TABLES & GRAPHS ------------ ###
make_table = function(today, FIP){
  myfip = filter(today, fips == FIP)
  url = paste0('https://en.wikipedia.org/wiki/',  gsub(" ", "_", myfip$name)) %>%
    read_html() %>%
    html_nodes("table.infobox") %>%
    html_table(fill= TRUE)
  l = url[[1]]
  # remove rows where the columns are identical
  ll = l[!l[,1] == l[,2],]

  ## Make a datatable from the resulting data.frame and turn off paging
  datatable(ll,
            caption = paste('Wikipedia Information:', myfip$name),
            # Turn off some of the interactive components for this table.
            options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
            colnames = rep("", ncol(ll)))
}
## Build State Ranking table
make_table2 = function(today, FIP){
  myfips = filter(today, fips == FIP)
  # Filter todays data to the state of the input FIP
  mydata = filter(today, state == myfips$state) %>%
    arrange(desc(cases)) %>%
    st_drop_geometry() %>%
    select(County = county, Cases = cases, Deaths = deaths) %>%
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) %>%
    head(10)

  formattable(mydata, align = c("l", rep("r", NCOL(mydata) - 1)),  list(`County` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold")), `Cases` = color_tile("cornsilk", "darkgoldenrod1"), `Deaths` = color_tile("lightpink", "tomato"), `DeathRate` = formatter("span", style = ~ style(color = "azure1",font.weight = "bold"))))

  # Make an interactive Table! with a caption
  # datatable(mydata, caption = paste('COVID-19 Statistics', myfips$state, myfips$date),
  #           options = list(paging = FALSE, searching = FALSE))
  # datatable(mydata, options = list(paging = FALSE, searching = FALSE))
}
make_table_2 = function(today, FIP){
  myfip  = filter(today, fips == FIP)

  # Filter todays data to the state of myfip
  mydata = filter(today, state == myfip$state) %>%
    arrange(desc(cases)) %>% # Arrange the cases from largest to smallest
    st_drop_geometry() %>%
    select(County = county, Cases = cases, Deaths = deaths) %>% # Keep only the County name, cases and deaths
    mutate(DeathRate = paste0(100* round(Deaths/Cases,2), "%")) # Create a new variable called Death Rate and save it as a string for printing
  datatable(mydata,
            caption = paste('COVID-19 Statistics', myfip$state, myfip$date))
}

# The graph requires you COVID data and a FIP code as input
make_graph = function(covid19, FIP){
  FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
  subset = filter(covid19, fips == FIP)
  rownames(subset) <- subset$date

  subset <- subset %>%
    rename(Cases = cases, Deaths = deaths)
  # Fit and exponetial model for fun
  exponential.model <- lm(log(Cases)~ date, data = subset)
  # use the model to predict a what a expoential curve would look like
  subset$Exponential = ceiling(exp(predict(exponential.model, list(date = subset$date))))

  dygraph(data = select(subset, Cases, Deaths, Exponential),
          main = paste0(" ", subset$name[1]),
          ylab = '',
          xlab = '') %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = .3,
                highlightSeriesOpts = list(strokeWidth = 3.5)) %>%
    dyAxis("y", axisLabelFormatter=JS(FUNC_JSFormatNumber), valueFormatter=JS(FUNC_JSFormatNumber)) %>%
    dyOptions(colors = c("darkcyan", "darkred", 'black'),
              drawGrid = FALSE,
              fillGraph = TRUE,
              strokeWidth = 3.5,
              stackedGraph = TRUE)
}

### ------------ COUNTRY DAILY GRAPHS ------------ ###
daily_cases_graph = function(covid19, FIP){
  subset2 <- covid19 %>%
    filter(fips == FIP)
  subset2 <- subset2 %>%
    group_by(state, date) %>%
    summarise(county = county, fips = fips, cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(rolling_mean = rollmean(new_cases, 7, fill = NA, align = 'right')) %>%
    arrange(desc(date)) %>%
    slice(n = 1:240)
  subset2 <- subset2 %>% rename(Date = date)
  font = list(
    family = 'Arial',
    size = 15,
    color = 'white',
    font = 2)
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font)
  gg_1 = ggplot(subset2) +
    geom_col(aes(x = Date, y = new_cases), fill = 'cadetblue') +
    geom_line(aes(Date, y = rolling_mean), col = "dodgerblue4", size = 0.6) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
  ggplotly(gg_1, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    layout(font = font) %>%
    config(displayModeBar = FALSE)
}
daily_deaths_graph = function(covid19, FIP){
  # NEW DEATHS --- COUNTY
  subset3 <- covid19 %>% filter(fips == FIP)

  subset3 <- subset3 %>%
    group_by(state, date) %>%
    summarise(county = county, fips = fips, deaths = sum(deaths, na.rm = TRUE)) %>%
    mutate(new_deaths = deaths - lag(deaths)) %>%
    mutate(rolling_mean = rollmean(new_deaths, 7, fill = NA, align = 'right')) %>%
    filter(new_deaths >= 0, rolling_mean >= 0) %>%
    arrange(desc(date)) %>%
    slice(n = 1:240)
  # subset3 <- subset3 %>% rename("Daily deaths" = new_deaths, "Rolling mean" = rolling_mean, Date = date)
  subset3 <- subset3 %>% rename(Date = date)
  font = list(
    family = 'Arial',
    size = 15,
    color = 'white',
    font = 2)
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font)
  gg_1 = ggplot(subset3) +
    geom_col(aes(Date, new_deaths),fill = 'tomato3', alpha = 0.5) +
    geom_line(aes(Date, y = rolling_mean), col = "darkred", size = 0.6) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
  ggplotly(gg_1, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    layout(font = font) %>%
    config(displayModeBar = FALSE)
}
total_cases_graph = function(covid19, FIP){
  subset4 <- covid19 %>% filter(fips == FIP)
  gg_3 <- ggplot(subset4, aes(date, cases)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    labs(x = 'DATE',
         y = 'CASES') +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
          legend.title.align = 0.5,
          legend.text = element_text(face = "bold", size = 12))
  ggplotly(gg_3)

}
total_deaths_graph = function(covid19, FIP){
  subset4 <- covid19 %>% filter(fips == FIP)
  gg_3 <- ggplot(subset4, aes(date, deaths)) +
    geom_col(col = 'aquamarine4', fill = 'aquamarine3') +
    labs(x = 'DATE',
         y = 'DEATHS') +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, hjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
          legend.title.align = 0.5,
          legend.text = element_text(face = "bold", size = 12))
  ggplotly(gg_3)
}


### ------------ COUNTRY LEVEL GRAPHS ------------ ###
usa_total_cases = function(covid19){
  total_cases <- covid19 %>%
    group_by(date) %>%
    summarize(cases = sum(cases, na.rm = TRUE)) %>%
    arrange(desc(date)) %>%
    slice(n = 1:320) %>%
    rename(Date = date)
  font = list(
    family = 'Arial',
    size = 15,
    color = 'white',
    font = 2)
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font)
  usa_cases = ggplot(total_cases) +
    geom_col(aes(x = Date, y = cases), fill = 'skyblue3',
             # col = 'black',
             # size = 0.1,
             alpha = 0.6) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
  ggplotly(usa_cases, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    layout(font = font) %>%
    config(displayModeBar = FALSE)
}
usa_total_deaths = function(covid19){
  total_deaths <- covid19 %>%
    group_by(date) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE)) %>%
    arrange(desc(date)) %>%
    slice(n = 1:320) %>%
    rename(Date = date)
  font = list(
    family = 'Arial',
    size = 15,
    color = 'white',
    font = 2)
  label = list(
    bgcolor = '#232F34',
    bordercolor = 'transparent',
    font = font)
  usa_deaths = ggplot(total_deaths) +
    geom_col(aes(x = Date, y = deaths), fill = 'tomato3',
             # col = "darkred",
             # size = 0.1,
             alpha = 0.5) +
    labs(x = '',
         y = '') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = comma) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
  ggplotly(usa_deaths, tooltip = c("x", "y")) %>%
    style(hoverlabel = label) %>%
    layout(font = font) %>%
    config(displayModeBar = FALSE)
}

### ------------ VALUE BOXES ------------ ###
cases_info = function(covid19){
  subset3 <- covid19 %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE), cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    arrange(desc(date)) %>%
    slice_max(1) %>%
    select(2:4) %>%
    formatC(format="d", big.mark=",")
}
death_info = function(covid19){
  subset4 <- covid19 %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE), cases = sum(cases, na.rm = TRUE)) %>%
    mutate(new_cases = cases - lag(cases)) %>%
    mutate(death_rate = 100*round(deaths/cases, 3)) %>%
    arrange(desc(date)) %>%
    slice_max(1)
}
