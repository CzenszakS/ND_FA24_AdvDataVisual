# ---
#   title: "Project App"
# author: "Stephen Czenszak"
# date: last-modified 
# subtitle: "Project App"
# format: 
#   html: 
#   embed-resources: true
# df-print: paged
# editor: visual
#
# --- 
# 
# link to app on shinyapps.io site: 
# 
#
#
#
# ---
  

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)
library(leaflet)
library(bslib)
library(rsconnect)
library(sf)
library(dplyr)
library(tidyr)
library(ggvis)
library(ggplot2)


###
### BEGIN:: Read in the datasets for Steve's tab 
### 

# School boundaries (spatial) 
school_boundaries <- st_read("../Data/School_Boundaries/School_Boundaries.shp", 
                             stringsAsFactors = FALSE, 
                             quiet = TRUE)  

# Abandoned Property Data (spatial) 
data_abandoned <- st_read("../Data/Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", 
                          stringsAsFactors = FALSE, 
                          quiet = TRUE) 

# Abandoned Property Data (spatial)
council_districts <- st_read("../Data/City_Council_Districts/City_Council_Districts.shp", 
                             stringsAsFactors = FALSE, 
                             quiet = TRUE) 

###
### END:: Read in the datasets for Steve's tab 
### 



###
### BEGIN:: Read in data sets for Chuck's tab and Initial Processing for Chuck's tab 
###

# Park locations and features (csv)
parksAndFeatures <- read.csv("../Data/Parks_Locations_and_Features.csv")

# Park locations and features prep for spatial
parksAndFeatures.spatial <- parksAndFeatures %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326) 

# Add info for popup marker
parksAndFeatures.spatial$popup <- paste("<b>",parksAndFeatures.spatial$Park_Name,"</b><br>",
                                        "Type: ",parksAndFeatures.spatial$Park_Type,"<br>",
                                        "Address: ",parksAndFeatures.spatial$Address,sep ="")

Public_Facilities <- read.csv("../Data/Public_Facilities.csv")

Public_Facilities <- Public_Facilities[Public_Facilities$POPL_TYPE != "LIBRARY", ]

Public_Facilities.spatial <- Public_Facilities %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

Public_Facilities.spatial$popup <- paste("<b>",Public_Facilities.spatial$POPL_NAME,"</b><br>",
                                         "Type: ",Public_Facilities.spatial$POPL_TYPE,"<br>",
                                         "Address: ",Public_Facilities.spatial$POPL_ADDR1,sep ="")

Public_Facilities <- read.csv("../Data/Public_Facilities.csv")

Public_Facilities <- Public_Facilities[Public_Facilities$POPL_TYPE != "LIBRARY", ]

Public_Facilities.spatial <- Public_Facilities %>% 
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326)

Public_Facilities.spatial$popup <- paste("<b>",Public_Facilities.spatial$POPL_NAME,"</b><br>",
                                         "Type: ",Public_Facilities.spatial$POPL_TYPE,"<br>",
                                         "Address: ",Public_Facilities.spatial$POPL_ADDR1,sep ="")

###
### END:: Read in data sets for Chuck's tab and Initial Processing for Chuck's tab 
###

###
### BEGIN:: Read in and preprocessing for Madi's tab
###

calls <- read.csv("./Data/311_Phone_Call_Log_Mod.csv")
set.seed(1842)
calls$date <- as.Date(calls$Call_Date, format = "%Y-%m-%d")
calls$day_of_week <- weekdays(calls$date)
calls$month <- month(calls$date)
### ***DISCLAIMER*** District data is randomly created below. It is NOT accurate call location data
calls$District <- sample(1:6, 61075, replace=TRUE, prob=c(0.05,0.3,0.25,0.15,0.15,0.1))
topics <- unique(calls$Called_About)
safety_topics <- topics[c(31, 33, 36, 38, 39, 41, 42, 45, 52, 56, 59, 60, 64, 65, 67, 68, 72, 74, 76, 81, 82, 83, 85, 86, 93, 94, 97, 106, 108, 109, 116, 117, 118, 119, 120, 121, 124, 125, 127, 128, 130, 132, 136, 137, 138, 140, 143, 146, 150, 151, 152, 155, 159, 160, 162, 163, 164, 165, 167, 169, 170, 171, 176, 177, 178, 179)]
calls$safety <- rep("No",length(calls$Called_About))
for(i in 1:length(calls$Called_About)){
  if(calls$Called_About[i] %in% safety_topics)
    #print(calls$Called_About[i])
    calls$safety[i] <- "Yes"
  #print(calls$safety[i])
}

###
### END:: Read in and Preprocessing for Madi's tab
###

### 
### BEGIN:: Processing for Steve's tab 
###

# Determine which schools reside in which districts 
dist1_schools = st_contains(council_districts[1,] %>% st_make_valid(), school_boundaries)
dist2_schools = st_contains(council_districts[2,]$geometry, school_boundaries)
dist3_schools = st_contains(council_districts[3,]$geometry, school_boundaries)
dist4_schools = st_contains(council_districts[4,]$geometry, school_boundaries)
dist5_schools = st_contains(council_districts[5,]$geometry, school_boundaries)
dist6_schools = st_contains(council_districts[6,]$geometry, school_boundaries)
# dist6_schools[[1]]

# By default, set district to 0 for all schools
school_boundaries$District = 0 

# Set values for schools based on results above 
school_boundaries[dist1_schools[[1]],]$District = 1
school_boundaries[dist2_schools[[1]],]$District = 2
school_boundaries[dist3_schools[[1]],]$District = 3
school_boundaries[dist4_schools[[1]],]$District = 4
school_boundaries[dist5_schools[[1]],]$District = 5
school_boundaries[dist6_schools[[1]],]$District = 6

# 54 of the 57 districts mapped correctly using st_contains. 
# The following 3 needed to be checked manually to determine the correct district.
school_boundaries[school_boundaries$OBJECTID == 76,]$District  = 2
school_boundaries[school_boundaries$OBJECTID == 103,]$District = 5
school_boundaries[school_boundaries$OBJECTID == 189,]$District = 4

# Make column integer
school_boundaries$District <- as.integer(school_boundaries$District)


# Determine which properties are in each district
dist1_aband = st_contains(council_districts[1,] %>% st_make_valid(), data_abandoned)
dist2_aband = st_contains(council_districts[2,]$geometry, data_abandoned)
dist3_aband = st_contains(council_districts[3,]$geometry, data_abandoned)
dist4_aband = st_contains(council_districts[4,]$geometry, data_abandoned)
dist5_aband = st_contains(council_districts[5,]$geometry, data_abandoned)
dist6_aband = st_contains(council_districts[6,]$geometry, data_abandoned)
# dist6_schools[[1]]

# By default, set district to 0 for all properties
data_abandoned$District = 0 

# Set values for properties based on results above 
data_abandoned[dist1_aband[[1]],]$District = 1
data_abandoned[dist2_aband[[1]],]$District = 2
data_abandoned[dist3_aband[[1]],]$District = 3
data_abandoned[dist4_aband[[1]],]$District = 4
data_abandoned[dist5_aband[[1]],]$District = 5
data_abandoned[dist6_aband[[1]],]$District = 6

# 1510 out of 1511 properties mapped correctly using st_contains. 
# The following property was checked manually. 
data_abandoned[data_abandoned$OBJECTID == 1408,]$District = 5

# Make column integer
data_abandoned$District <- as.integer(data_abandoned$District)


# Create labels for schools and menus by district 
# generate name school labels 
school_boundaries$Label = paste(school_boundaries$OBJECTID, school_boundaries$School, sep=" - ")
#school_boundaries$Label = paste(school_boundaries$Label, " (Dist ", sep = "")
#school_boundaries$Label = paste(school_boundaries$Label, school_boundaries$District, sep="")
#school_boundaries$Label = paste(school_boundaries$Label, ")", sep = "")

# generate menu groups for each district 
schools_menu_1 <- setNames(school_boundaries[school_boundaries$District == 1,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 1,]$Label) 
schools_menu_2 <- setNames(school_boundaries[school_boundaries$District == 2,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 2,]$Label) 
schools_menu_3 <- setNames(school_boundaries[school_boundaries$District == 3,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 3,]$Label) 
schools_menu_4 <- setNames(school_boundaries[school_boundaries$District == 4,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 4,]$Label) 
schools_menu_5 <- setNames(school_boundaries[school_boundaries$District == 5,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 5,]$Label) 
schools_menu_6 <- setNames(school_boundaries[school_boundaries$District == 6,]$OBJECTID, 
                           school_boundaries[school_boundaries$District == 6,]$Label) 

school_choices_list <- list(
  "District 1" = schools_menu_1,
  "District 2" = schools_menu_2,
  "District 3" = schools_menu_3,
  "District 4" = schools_menu_4,
  "District 5" = schools_menu_5,
  "District 6" = schools_menu_6
)

school_choices_all <- c(
  unname(schools_menu_1), unname(schools_menu_2), 
  unname(schools_menu_3), unname(schools_menu_4), 
  unname(schools_menu_5), unname(schools_menu_6)
)

report1_choices <- setNames(1:2, 
                            c("Most Nearby Abandoned Properties, Top 6 Schools", 
                              "Avg. Nearby Abandoned Properties, By District")
                            )

# Rename OBJECTID in the school and property data so they are unique names 
# This makes it easier to organize the data when computing distances between all pairs of both 
data_abandoned <- data_abandoned %>% dplyr::rename(PROPERTY_ID = OBJECTID)
school_boundaries <- school_boundaries %>% dplyr::rename(SCHOOL_ID = OBJECTID)

# Calculate distances between properties and schools 
# This creates a 57 x 1511 dataframe 
abandoned_distances <- st_distance(st_centroid(st_geometry(school_boundaries)), 
                                   st_centroid(st_geometry(data_abandoned))) %>% as.data.frame

# Set the column names to the PROPERTY IDs 
colnames(abandoned_distances) <- data_abandoned$PROPERTY_ID

# Add a column at the beginning with the SCHOOL_ID 
# Now the dataframe is 57 x 1512 
abandoned_distances <- abandoned_distances %>% mutate(SCHOOL_ID = school_boundaries$SCHOOL_ID, .before=1)

# Transform the dataframe to something we can use for a joing 
# The resulting table has one row per combination of school and abandoned property 
# So total size is now 1511 x 57 = 86,127 rows of distance data 
abandoned_distances <- abandoned_distances %>% 
  pivot_longer(cols = "1":"1511", 
               names_to = "PROPERTY_ID", 
               values_to = "distance")

# Create a column with the distance values in unitless format 
abandoned_distances$distanceUnitless = as.vector(abandoned_distances$distance)

# Total abandoned properties per district 
district_abandoned_totals <- data_abandoned %>% 
  group_by(District) %>% 
  summarise("Total_Abandoned_Properties" = n())
# district_abandoned_totals  

# Total schools per district 
district_school_totals <- school_boundaries %>% 
  dplyr::select(District, SCHOOL_ID) %>%
  as.data.frame %>%
  group_by(District) %>% 
  summarise("Total_Schools_in_District" = n())
# district_school_totals

# Join both as summary table 
district_totals <- district_school_totals %>% 
  inner_join(district_abandoned_totals, by = "District") %>% 
  as.data.frame() %>% 
  dplyr::select(District, Total_Schools_in_District, Total_Abandoned_Properties) %>%
  rename("Schools" = Total_Schools_in_District) %>% 
  rename("Abandoned Properties" = Total_Abandoned_Properties)
# district_totals

### 
### END:: Processing for Steve's tab 
###



###
### BEGIN:: Added label info for mouse over info for Chuck's tab 
### 

school_boundaries$label_info  <- paste(
  school_boundaries$School
)

dist_school_to_parks <- st_distance(school_boundaries, parksAndFeatures.spatial)
dist_school_to_responder <- st_distance(school_boundaries, Public_Facilities.spatial)

###
### END:: Added label info for mouse over info for Chuck's tab 
### 



###
### Pre-processing for Alex's tab 
###

#Call datasets
boundaries <- st_read("../Data/School_Boundaries/School_Boundaries.shp")
lights = read_csv("../Data/Street_Lights.csv")
#Create geometry column for lights
lights = lights %>%  st_as_sf(coords = c("Lon","Lat")) %>% st_set_crs(value = 4326)
#Find the distance between lights and the school boundaries.
light_distance=st_distance(lights$geometry,boundaries$geometry)
lddf=as.data.frame(light_distance)
#Rename columns and rows, add OBJECTID from light, and use pivot_longer to turn columns and values into two columns.
colnames(lddf) = boundaries$School
rownames(lddf) = lights$OBJECTID
lddf <- lddf %>% mutate(OBJECTID = lights$OBJECTID, .before=1)
lddf <- lddf %>% 
  pivot_longer(cols = "Messiah Christian Academy":"Success Academy", 
               names_to = "School", 
               values_to = "distance")
#Remove meters from distance
lddf$distance <- as.vector(lddf$distance)
#Create a dataframe filtering lights down to lights less than 200 meters from a school.
lddf_200 = lddf %>% filter(distance <= 200)
#Create count of lights near schools for bar chart.
count_table=data.frame(table(lddf_200$School))
count_table$Var1 <- factor(count_table$Var1,levels = count_table$Var1[order(count_table$Freq)])
#Add geometry to lddf_200.
lights_simple = lights %>% dplyr::select("OBJECTID","geometry")
lddf_200 = merge(lights_simple,lddf_200,by="OBJECTID",all.y=TRUE)



###
### UI FUNCTION  
### 

# Define UI for application that draws a histogram
ui <- navbarPage("South Bend School Data", id = "nav", 

        # tags$style(type="text/css",
        #             ".shiny-output-error { visibility: hidden; }",
        #             ".shiny-output-error:before { visibility: hidden; }"
        #             ),
                 
  tabPanel("Nearby Abandoned Property Data", 
    div(class = "outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),        
                        
      # The main map for this tab 
      leafletOutput("mainPlot_Abandoned", height="100%", width="100%"),
      
      
      absolutePanel(id = "controls", class = "panel panel-default", 
                    fixed = TRUE, draggable = FALSE, 
                    top = 60, left = "auto", right = 10, bottom = "auto",
                    width = 500, height = "auto",
      
        h5(""),   # This just adds some blank space at the top 
        
        layout_columns(
          
          sliderInput("distance", "Search Radius (miles)",
                      min = 0, max = 2, step = 0.05, value = 0.4, width = "90%"
          ), 
        
          sliderInput("searchRadiusOpacity", "Search Radius Opacity",
                      min = 0.000, max = 0.3, step = 0.025, value = 0.075, width = "90%"
          ), 
        
        ), 
        # This was demo/sample code 
        #sliderInput("bins", "Number of bins:",
        #            min = 10, max = 200, value = 30, 
        #            width = "90%"
        #), 

        virtualSelectInput(inputId = "school_ids", label = "Schools to Display (by District):",
                           choices = school_choices_list,
                           multiple = TRUE, 
                           inline = TRUE, 
                           selected = school_choices_all
        ),   
        
        br(),
        h5("District Totals (All Schools and Abandoned Properties):"), 
        
        tableOutput("districtTotals"),
        
        selectInput("report1choice", "Summary Report for Selected Schools:",
                    choices = report1_choices, width = 370),
        
        tableOutput("report1"),
        
      ), #end: absolutePanel  

      tags$div(id="cite",
               'Group 3: Stephen Czenszak'
      )
      
    ) #end: div 
  ), 
  

tabPanel("Parks, Schools, and Responders",
         div(class = "outer",
             
             # Include custom CSS
             tags$head(
               includeCSS("styles.css"),
               tags$style(HTML("
          .outer {
            position: fixed;
            top: 0;
            left: 0;
            bottom: 0;
            right: 0;
            overflow: hidden;
          }
          #parkschoolresponder {
            height: 100%;
            width: 100%;
          }
          #controls {
            z-index: 1000;
          }
        "))
             ),
             
             # Leaflet map output
             leafletOutput("parkschoolresponder", height = "100%", width = "100%"),
             
             # Absolute panel for controls
             absolutePanel(
               id = "controls", class = "panel panel-default",
               fixed = TRUE, draggable = FALSE,
               top = 60, left = 10, right = "auto", bottom = "auto",
               width = 300, height = "auto",
               
               # Input controls
               selectInput("selected_school", "Select a School:", choices = school_boundaries$School),
               sliderInput("radius", "Radius (meters):", min = 0, max = 20000, value = 5000, step = 100),
               textOutput("count_parks"),
               textOutput("count_facilities")
             ) # End Absolute Panel
         ) # End Div
), # End TabPanel

  
  
  tabPanel("Street Lighting Around Schools", 
           
           # Application title
           titlePanel("Street Lighting Around Schools"),
           
           #Create input for list of schools.
           virtualSelectInput(inputId = "school", label = "School:",choices = unique(boundaries$School)),
           
           #Display outputs.
           mainPanel(
             leafletOutput(outputId = "mainPlot_Lighting"),
             plotOutput("distPlot")
           )           
           
  ), 
  
  tabPanel("311 Call Data", "**DISCLAIMER** District Information is randomly created. It does NOT indicate accurate location data for calls.",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css")
           ),    
           
           # Application title
           titlePanel("311 Calls"),
           fluidRow(
             column(3,
                    wellPanel(
                      h4("Filter"),
                      checkboxGroupInput("Districts","Select District(s):", c(1:6)),
                      checkboxGroupInput("Department","Select Department(s):", unique(calls$Department))
                    )
             ),
             # Show a plot of the generated plot
             column(9,   
                    ggvisOutput("plot1")
             )
           )
  )
  

)



### 
### SERVER FUNCTION 
### 

server <- function(input, output) {

  ###
  ### ABANDONED PROPERTY TAB 
  ###
  
  # Create the map
  output$mainPlot_Abandoned <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -86.200, lat = 41.670, zoom = 12)
  })
  
  # Update the list of schools if the schools selected changes  
  get_schools_in <- reactive({
    my_school_ids <- input$school_ids
      
    # Select the schools to display data for 
    return_schools_in <- school_boundaries %>% dplyr::filter(SCHOOL_ID %in% my_school_ids)
    
    # Return the result 
    return_schools_in
  })

  # Update the list of abandoned properties for the selected schools if 
  # either the set of schools has changed, or the distance range has changed 
  get_distances_in <- reactive({ 
    schools_in <- get_schools_in()      
    my_distance <- input$distance * 1609.34  # And convert from miles to meters

    # Join in ALL the distance data for the selected schools 
    schools_in_distances <- inner_join(schools_in, abandoned_distances, by = "SCHOOL_ID")
    
    # Filter the distance data for the selected schools to get only the properties 
    # within the specified distance, convert to data frame, drop school geometry data,   
    # and rename the District variable to avoid conflict when joining with 
    # abandoned property data 
    return_distances_in <- schools_in_distances %>% 
      dplyr::filter(distanceUnitless < my_distance) %>% 
      as.data.frame %>% 
      dplyr::select(!geometry) %>%    # drop the geometry data for the school 
      dplyr::rename(School_District = District) 
    
    # Return the result 
    return_distances_in          
  })

  # Update just the list of properties that are within the specified distance of
  # any of the selected schools 
  get_properties_in <- reactive({ 
    distances_in <- get_distances_in()      

    # Now select the unique properties that were within the specified distance of 
    # any of the selected schools
    return_properties_in <- distances_in 
    return_properties_in$PROPERTY_ID = as.double(return_properties_in$PROPERTY_ID)
    return_properties_in <- return_properties_in %>% 
      arrange(PROPERTY_ID) %>% 
      dplyr::distinct(PROPERTY_ID)
    
    # Now inner join with the abandoned properties to get the rest of the property info
    return_properties_in <- inner_join(data_abandoned, return_properties_in, by = "PROPERTY_ID")    
    
    # Return the result 
    return_properties_in         
  })
  
  report1_df <- reactive({
    distances_in <- get_distances_in() 
    schools_in <- get_schools_in() 
    report1_choice <- input$report1choice
    
    table1_data <- distances_in %>% 
      group_by(Label) %>% 
      summarise(num_abandoned_properties = n()) %>% 
      full_join(schools_in, by = "Label") %>% 
      replace(is.na(.), 0) %>% 
      dplyr::select(District, num_abandoned_properties, Label) %>% 
      arrange(desc(num_abandoned_properties)) %>% 
      rename(School = Label, "Nearby Abandoned Properties" = num_abandoned_properties)
    
    table2_data <- table1_data %>% 
      rename(nearby_aban_prop = "Nearby Abandoned Properties") %>% 
      group_by(District) %>% 
      summarize("Number Schools Selected" = n(),
                avg_nearby_aban_prop = mean(nearby_aban_prop)) %>% 
      arrange(desc(avg_nearby_aban_prop)) %>% 
      rename("Avg Nearby Abandoned Properties" = avg_nearby_aban_prop)
    
    # Limit table1 to first X rows 
    table1_data <- table1_data %>% slice(1:6)
    
    if (report1_choice == 1) { 
      report1_df_out <- table1_data
    } else { 
      report1_df_out <- table2_data
    }
    
    # Return the selected table 
    report1_df_out
  })
  
  # Update the graphics     
  observe( priority = 0, { 
    schools_in <- get_schools_in()      
    distances_in <- get_distances_in()  
    my_distance <- input$distance * 1609.34  # And convert from miles to meters
    properties_in <- get_properties_in() 
    my_searchRadiusOpacity <- input$searchRadiusOpacity  

    leafletProxy("mainPlot_Abandoned") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(data = council_districts, 
                  color = "#AC7299",   # purple
                  weight = 2, smoothFactor = 0.3,
                  opacity = 0.8, fillOpacity = 0.075
      ) %>% 
      addPolygons(data = properties_in, 
                  color = "#cf191a", fillColor = "#dd4636",    
                  weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.8
      ) %>% 
      addCircles(data = st_centroid(st_geometry(schools_in)), 
                 stroke = 1, weight = 1, 
                 opacity = my_searchRadiusOpacity, 
                 fillOpacity = my_searchRadiusOpacity, 
                 radius = my_distance, color = "red"
      ) %>% 
      addPolygons(data = schools_in, 
                  color = "#306C94",  fillColor = "#306C94",  # orange , #306C94
                  weight = 1, smoothFactor = 0.5,
                  opacity = .75, fillOpacity = .75 
      ) %>% 
      addCircleMarkers(data = st_centroid(st_geometry(schools_in)), 
                       stroke = 0, opacity = .75, fillOpacity = .75, 
                       radius = 5, color = "#306C94", fillColor = "#306C94"
      ) 
  })

  output$districtTotals <- renderTable(district_totals, 
                                       spacing = "xs", align = "c")
  
  output$report1 <- renderTable(report1_df(), 
                                spacing = "xs", align = "ccl")
  
  ###
  ### PARKS, SCHOOLS, RESPONDERS TAB 
  ###
  
  selected_school <- reactive({
    school_boundaries[school_boundaries$School == input$selected_school, ]
  })
  
  buffer <- reactive({
    st_buffer(selected_school(), dist = input$radius)
  })
  
  points_within_radius <- reactive({
    list(
      parks = st_intersects(buffer(), parksAndFeatures.spatial, sparse = FALSE),
      facilities = st_intersects(buffer(), Public_Facilities.spatial, sparse = FALSE)
    )
  })
  
  output$count_parks <- renderText({
    paste("Parks within radius:", sum(points_within_radius()$parks))
  })
  
  output$count_facilities <- renderText({
    paste("Facilities within radius:", sum(points_within_radius()$facilities))
  })
  
    output$parkschoolresponder <- renderLeaflet({
    leaflet(school_boundaries) %>%
      addTiles() %>%
      addPolygons(
        color = "blue",
        weight = 2,
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          color = "red",
          weight = 3,
          bringToFront = TRUE
        ),
        label = ~label_info, # Dynamic labels from the `label_info` column
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "color" = "black"),
          textsize = "13px",
          direction = "auto",
          html = TRUE, # Enable HTML rendering
          
        )
      )  %>%
      
      addMarkers(data = parksAndFeatures.spatial, 
                 popup = ~popup,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30)) %>%
      addMarkers(data = Public_Facilities.spatial, 
                 popup = ~popup,
                 )
 
    
  })
    
    observe({
      leafletProxy("parkschoolresponder") %>%
        clearShapes() %>%
        addPolygons(data = buffer(), color = "red", weight = 1, opacity = 0.5) %>%
        setView(lng = st_coordinates(selected_school())[1, 1],
                lat = st_coordinates(selected_school())[1, 2],
                zoom = 12)
    })    
    
  

    ###
    ### LIGHTING TAB 
    ### 
    
    #Render blank map.
    output$mainPlot_Lighting <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -86.200, lat = 41.670, zoom = 11)
    })
    
    #Create reactive functions for schools and lights based on input.
    get_schools_light_in <- reactive({
      my_schools <- input$school
      
      # Select the schools to display data for 
      return_schools_in <- boundaries %>% dplyr::filter(School %in% my_schools)
      
      # Return the result 
      return_schools_in
    })
    get_lights_in <- reactive({
      my_schools <- input$school
      
      # Select the schools to display data for 
      return_lights_in <- lddf_200 %>% dplyr::filter(School %in% my_schools)
      
      # Return the result 
      return_lights_in
    })
    
    #Create polygons and circle markers based on the input.
    observe( priority = 0, { 
      schools_in <- get_schools_light_in()
      lights_in <- get_lights_in()
      leafletProxy("mainPlot_Lighting")  %>%
        clearShapes()  %>%
        clearMarkers()  %>%
        addPolygons(data=schools_in) %>%
        addCircleMarkers(data = lights_in, stroke = 0, fillOpacity = 0.35, radius = 4)}) 
    #Render the barplot.
    output$distPlot <- renderPlot(ggplot(data=count_table,aes(x=Var1,y=Freq))+geom_bar(stat = "identity", width = 0.75)+coord_flip()+labs(x = "School", y = "Count", title = "Number of Street Lights Within 0.2 km of Schools"))
    
  
    ###
    ### 311 Calls Tab
    ###

      #calls2 <- calls %>% filter(District %in% input$Districts) %>% filter(Department %in% input$Department)
      calls2 <- reactive({
        c <- calls %>% filter(District %in% input$Districts) %>% filter(Department %in% input$Department)
        c
      })
      call_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$FID)) return(NULL)
        # pick out call
        all_calls <- isolate(calls2())
        call1 <- all_calls[all_calls$FID == x$FID,]
        
        paste0("<b>",call1$FID, "</b><br>",
               "Department: ", call1$Department, "</b><br>",
               "Call Date: ", call1$date, "</b><br>",
               "Called About: ", call1$Called_About, "</b><br>",
               "District: ", call1$District
        )
      }
      
      vis <- reactive({
        calls2 %>%
          ggvis(x = ~day_of_week, y = ~duration_Seconds, opacity := 1.0) %>%
          layer_points(size := 50, size.hover := 200,
                       fillOpacity := 0.2, fillOpacity.hover := 0.5,
                       stroke = ~safety, key := ~FID) %>%
          add_tooltip(call_tooltip, "hover") %>%
          add_axis("x", title = "Day of the Week") %>%
          add_axis("y", title = "Call Duration (in seconds)") %>%
          add_legend("stroke", title = "Safety Topic", values = c("Yes","No")) %>%
          scale_nominal("stroke", domain = c("Yes","No"),
                        range = c("orange", "#aaa")) %>%
          set_options(width = 500, height = 500)
      })
      
      vis %>% bind_shiny("plot1")
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)
