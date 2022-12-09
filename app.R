

library(shiny)
library(shinyjs)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(dplyr)
library(lubridate)

library(RPostgreSQL)
library(RPostgres)
library(DBI)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgis_33_sample",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

getColorsMap <- function(x){
  lapply(x, function(i) unlist(strsplit(i, ";"))[1]) %>%unlist()
}

getFillColorsMap <- function(x){
  lapply(x, function(i) unlist(strsplit(i, ";"))[2]) %>%unlist()
}

getDashLineMap <- function(x){
  lapply(x, function(i) unlist(strsplit(i, ";"))[3]) %>%unlist()
}

getOpacityFeatureMap <- function(x){
  lapply(x, function(i) unlist(strsplit(i, ";"))[4]) %>%unlist()
}

ui <- fillPage(
  includeCSS("www/css/styles.css"),
  useShinyjs(),
  div(leafletOutput("mapa",  width = "100%", height = "100%"), style="width:100%; height:100%"),
  div(fluidRow(column(12, selectInput("editingLayer",label = "Agregar elemento a la capa:", choices = c("inmuebles_urbanos", "inmuebles_rusticos", "vias_publicas_urbanas", "vias_publicas_rusticas",
                                                                         "bienes_revertibles", "patrimonio_suelo")))), 
      class="fixedPanel")
  
)

# Server functions
server <- function(input, output, session) {
  
  reactiveData <- reactiveValues()
  
  ## Non editable layers
  parcelas_rusticas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM parcelas_rusticas") %>%
    st_transform(4326)
  construcciones_urbanas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM construcciones_urbanas") %>%
    st_transform
  parcelas_urbanas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM parcelas_urbanas") %>%
    st_transform(4326)
  limite_urbana <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM limite_urbana") %>%
    st_transform(4326)
  limite_rustica <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM limite_rustica") %>%
    st_transform(4326)
  carreteras <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM carreteras") %>%
    st_transform(4326)
  ferrocarril <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM ferrocarril") %>%
    st_transform(4326)
  hidrografia <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM hidrografia") %>%
    st_transform(4326)
  vias_pecuarias <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM vias_pecuarias") %>%
    st_transform(4326)
  

  output$mapa <- renderLeaflet({
    
    
    #Editable layers
    inmuebles_urbanos <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM inmuebles_urbanos") %>%
      st_transform(4326)
    inmuebles_rusticos <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM inmuebles_rusticos") %>%
      st_transform(4326)
    vias_publicas_rusticas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM vias_publicas_rusticas") %>%
      st_transform(4326)
    vias_publicas_urbanas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM vias_publicas_urbanas") %>%
      st_transform(4326)
    patrimonio_suelo <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM patrimonio_suelo") %>%
      st_transform(4326)
    bienes_revertibles <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM bienes_revertibles") %>%
      st_transform(4326)
    
    mapa<-leaflet() %>% 
      addPolygons(data = parcelas_rusticas , color=getColorsMap(parcelas_rusticas$style), group = "parcelas_rusticas", fill=TRUE,
                  popup = popupTable(parcelas_rusticas[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(parcelas_rusticas)
                  ))
                  ), layerId = parcelas_rusticas$parcela_id) %>%
      addPolygons(
        data = construcciones_urbanas , color=getColorsMap(construcciones_urbanas$style), group = "construcciones_urbanas", fill=TRUE,
        popup = popupTable(construcciones_urbanas[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
          "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(construcciones_urbanas)
        ))
        ), layerId = construcciones_urbanas$construccion_id )%>%
      addPolygons(
        data = parcelas_urbanas , color="gray", group = "parcelas_urbanas", fill=TRUE, fillColor = "gray",
        popup = paste(
          "via_id: ", parcelas_urbanas$parcela_id, "<br>",
          "masa: ", parcelas_urbanas$DELEGACIO, "<br>",
          "parcela: ", parcelas_urbanas$MUNICIPIO, "<br>",
          "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
          "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                      ), layerId = parcelas_urbanas$parcela_id ) %>%
      addPolygons(
        data = limite_urbana , color=getColorsMap(limite_urbana$style), group = "limite_urbana", fill=FALSE, dashArray ="20,20",
        layerId = limite_urbana$limite_id ) %>%
      addPolygons(
        data = limite_rustica , color=getColorsMap(limite_rustica$style), group = "limite_rustica", fill=FALSE, dashArray ="20,20",
        layerId = limite_rustica$limite_id ) %>%
      # addPolygons(
      #   data = hidrografia , color="blue", group = "hidrografia",
      #   popup = paste(
      #     "via_id: ", hidrografia$cuerpo_id, "<br>",
      #     "masa: ", hidrografia$id_red, "<br>",
      #     "parcela: ", hidrografia$id_tramo, "<br>",
      #     "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
      #     "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
      #   ), layerId = hidrografia$cuerpo_id ) %>%
      # addPolygons(
      #   data = carreteras , color="red", group = "carreteras",
      #   popup = paste(
      #     "via_id: ", carreteras$carretera_id, "<br>",
      #     "masa: ", carreteras$id_vial, "<br>",
      #     "parcela: ", carreteras$carretera_id, "<br>",
      #     "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
      #     "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
      #   ), layerId = carreteras$carretera_id) %>%
      addPolylines(
        data = vias_pecuarias , color=getColorsMap(vias_pecuarias$style), group = "vias_pecuarias", fillColor = getColorsMap(vias_pecuarias$style),
        popup = popupTable(vias_pecuarias[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
          "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(vias_pecuarias)
        ))
        ), layerId = vias_pecuarias$via_id ) %>%
    
      addPolygons(data = inmuebles_urbanos , color=getColorsMap(inmuebles_urbanos$style), group = "inmuebles_urbanos", fill=TRUE, fillColor = getColorsMap(inmuebles_urbanos$style), opacity = 0.4,
                  popup = popupTable(inmuebles_urbanos[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(inmuebles_urbanos)
                  ))
                  ),
                  layerId = inmuebles_urbanos$inmueble_id) %>%
      addPolygons(data = inmuebles_rusticos , color=getColorsMap(inmuebles_rusticos$style), group = "inmuebles_rusticos", fill=TRUE, opacity = 0.4,
                  popup = popupTable(inmuebles_rusticos[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(inmuebles_rusticos)
                  ))
                  ),
                  layerId = inmuebles_rusticos$inmueble_id)  %>%
      addPolygons(data = vias_publicas_urbanas , color=getColorsMap(vias_publicas_urbanas$style), group = "vias_publicas_urbanas",
                  popup = popupTable(vias_publicas_urbanas[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(vias_publicas_urbanas)
                  ))
                  ),
                  layerId = vias_publicas_urbanas$via_id) %>%
      addPolygons(data = vias_publicas_rusticas , color=getColorsMap(vias_publicas_rusticas$style), group = "vias_publicas_rusticas", fillColor = getFillColorsMap(vias_publicas_rusticas$style),
                  popup = popupTable(vias_publicas_rusticas[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(vias_publicas_rusticas)
                  ))
                  ), 
                  layerId = vias_publicas_rusticas$via_id) %>%
      addPolylines(data = bienes_revertibles , color=getColorsMap(bienes_revertibles$style), group = "bienes_revertibles", fillColor = getFillColorsMap(bienes_revertibles$style),
                  popup = popupTable(bienes_revertibles[,1:3] %>% mutate(button = rep(
                                                                          "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                                                                    ), nrow(bienes_revertibles))
                                     ), 
                  layerId = bienes_revertibles$bien_id) %>%
      addPolygons(data = patrimonio_suelo , color=getColorsMap(patrimonio_suelo$style), group = "patrimonio_suelo", fill = TRUE,
                  popup = popupTable(patrimonio_suelo[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                    "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(patrimonio_suelo)
                  ))
                  ), 
                  layerId = patrimonio_suelo$patrimonio_id) %>%
      addTiles( group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addWMSTiles(
        "http://ovc.catastro.meh.es/Cartografia/WMS/ServidorWMS.aspx",
        layers = "catastro"
      ) %>%
      addWMSTiles(
        "https://mapserver.eprinsa.es/cgi-bin/eiel",
        layers = "EIEL"
      ) %>%
      addLayersControl(
        baseGroups = c("Esri.WorldImagery", "OSM","catastro", "EIEL"),
        overlayGroups = c(
          "parcelas_rusticas", "contrucciones_urbanas", "parcelas_urbanas", "limite_urbana","limite_rustica", "hidrografia","carreteras",
                          "vias_pecuarias", "ferrocarril",
                          "inmuebles_urbanos", "inmuebles_rusticos", "vias_publicas_urbanas", "vias_publicas_rusticas",
                          "bienes_revertibles", "patrimonio_suelo"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addDrawToolbar(
        targetGroup = "editing",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        position="bottomleft") %>% 
      addEasyButton(easyButton( position = "bottomleft",
        icon="fa-backward", title="Refresh map", id = "refresh-map", 
        onClick = JS("function(btn, map){Shiny.onInputChange('cancel_edit', Math.random());}")))
      
    
    if(!is.null(reactiveData$mapLocation)){
      print(reactiveData$mapLocation[["east"]])
      mapa %>% fitBounds(lng1=reactiveData$mapLocation[["east"]], lat1=reactiveData$mapLocation[["north"]], lng2=reactiveData$mapLocation[["west"]], lat2=reactiveData$mapLocation[["south"]])
    } else{
      mapa
    }
    
  })
  
  ### Mapa proxy
  map_proxy<-leaflet::leafletProxy("mapa")
  
  
  newFeature <- reactiveValues()
  observeEvent(input$mapa_draw_new_feature,{

    updatedLayer <- input$editingLayer
    
    if(editedFeature$updatedLayer == "inmuebles_rusticos" | editedFeature$updatedLayer == "vias_publicas_rusticas" |
       editedFeature$updatedLayer == "inmuebles_urbanos" | editedFeature$updatedLayer == "vias_publicas_urbanas" |
       editedFeature$updatedLayer == "bienes_revertibles" | editedFeature$updatedLayer == "patrimonio_suelo"){
      geometryInDatabase <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", updatedLayer," LIMIT 1")) %>%
        st_transform(4326) %>% st_geometry_type(by_geometry = FALSE)
      
      pol<-input$mapa_draw_new_feature
      
      
      if(pol$properties$feature_type=="circle"){
        poligono<- st_point(c(pol$geometry$coordinates[[1]], pol$geometry$coordinates[[2]]))%>%
          st_sfc()%>%st_set_crs(4326)%>%
          st_transform(3857)%>%
          st_buffer(dist = pol$properties$radius)
      }else if(pol$properties$feature_type=="polygon" | pol$properties$feature_type=="rectangle"){
        coor<-unlist(pol$geometry$coordinates)
        datap<-data.frame(
          Longitud=coor[seq(1,length(coor), 2)], 
          Latitud=coor[seq(2,length(coor), 2)]
        )
        poligono<-datap %>%
          st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON") ##%>% st_cast("MULTIPOLYGON")
        
        if(geometryInDatabase == "MULTIPOLYGON"){
          poligono <- poligono %>% st_cast("MULTIPOLYGON")
        }
      } else if(pol$properties$feature_type=="polyline"){
        coor<-unlist(pol$geometry$coordinates)
        datap<-data.frame(
          Longitud=coor[seq(1,length(coor), 2)], 
          Latitud=coor[seq(2,length(coor), 2)]
        )
        poligono<-datap %>%
          st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("MULTILINESTRING")
      }
      
      
      if(geometryInDatabase != st_geometry_type(poligono, by_geometry = FALSE)){
        return()
      } 
      
      dataExample <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", updatedLayer," LIMIT 1")) %>%
        st_transform(4326)
      
      showModal(
        modalDialog(title = "New Data",
                    fluidRow(
                      column(12,h4("Actualiza los datos"))),
                    fluidRow(class="panel-inputs-newFeature",
                             fluidRow(
                               lapply(1:(ncol(dataExample)-1), function(i) {
                                 print(class(dataExample[[i]]))
                                 print(dataExample[[i]])
                                 if(class(dataExample[[i]]) == "character"){
                                   textInput(paste0("id_", colnames(dataExample)[i]), label = colnames(dataExample)[i], value = ifelse(is.null(dataExample[[i]]), NA,dataExample[[i]] ))
                                 } else if(class(dataExample[[i]]) == "numeric" | class(dataExample[[i]]) == "integer"){
                                   numericInput(paste0("id_", colnames(dataExample)[i]), label = colnames(dataExample)[i], value = ifelse(is.na(dataExample[[i]]), 0,dataExample[[i]] ))
                                 } else if(class(dataExample[[i]]) == "Date"){
                                   dateInput(paste0("id_", colnames(dataExample)[i]), label = colnames(dataExample)[i], value = ifelse(is.na(dataExample[[i]]), as.character(Sys.Date()),dataExample[[i]] ))
                                 }
                                 
                               })
                               
                             )
                    ),
                    easyClose = FALSE,
                    size = "m",
                    footer = tagList(
                      actionButton("cancelar","Cancel"),
                      actionButton("addNewFeature","Save")
                    ) 
        )
      )
      
      newFeature$poligono <- poligono
      newFeature$attributes <- dataExample %>% st_drop_geometry()
    } else {
      showNotification(h4("No puedes editar este layer"), action = NULL, duration = 5, 
                       id = "noEdit", "error")
    }
  })
  
  observeEvent(input$addNewFeature,{
    formDataList <- lapply(2:ncol(newFeature$attributes), function(i) input[[paste0('id_', colnames(newFeature$attributes)[i])]])
    
    formDataVector <- c()
    for(i in 1:length(formDataList)){
      if(class(formDataList[[i]]) == "Date"){
        formDataVector <- c(formDataVector, as.character(ymd(formDataList[[i]])))
      } else{
        formDataVector <- c(formDataVector, formDataList[[i]])
      }
    }
    
    
    print(input$editingLayer)
    getLastID <- dbGetQuery(conn, paste0("SELECT * FROM ",input$editingLayer, " ORDER BY ", colnames(newFeature$attributes)[1]," DESC LIMIT 1"))
    newId <- getLastID[,1] + 1
    
    dataUpdated <- c(newId,formDataVector )
    geoemtryPolygon <- st_as_text(newFeature$poligono$geometry)
    
    dbGetQuery(conn, paste0("INSERT INTO ", input$editingLayer, " VALUES ('",paste(dataUpdated , collapse = "', '"), "')"))
    
    dbGetQuery(conn, paste0("UPDATE ", input$editingLayer," SET geom = ST_GeomFromText('", geoemtryPolygon,"', 4326) WHERE ",colnames(newFeature$attributes)[1]," = ",newId,";"))
    
    dbGetQuery(conn, paste0("UPDATE ", updatedLayer," SET modificado_por = ",session$clientData$url_search,", fecha_modifica = ",Sys.time()," WHERE ",columnUpdated ," = ",editedFeature,";"))
    
    newData <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", input$editingLayer)) %>%
      st_transform(4326)

    reactiveData$mapLocation <- input$mapa_bounds
    
    
    removeModal()
  
  })
  
  
  observeEvent(input$remove_feature,{
    req(editedFeature$featureId)
    
    removedFeature <- editedFeature$featureId
    updatedLayer <- editedFeature$updatedLayer
    columnUpdated <- dbGetQuery(conn, paste0("SELECT * FROM ",updatedLayer," LIMIT 1")) %>% select(1L) %>% names(.)
    
    if(updatedLayer == "inmuebles_rusticos" | updatedLayer == "vias_publicas_rusticas" |
       updatedLayer == "inmuebles_urbanos" | updatedLayer == "vias_publicas_urbanas" |
       updatedLayer == "bienes_revertibles" | updatedLayer == "patrimonio_suelo"){
      
      dbGetQuery(conn, paste0("DELETE FROM ",updatedLayer," WHERE ",columnUpdated," = ", removedFeature))
      
      #Update map
      newData <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", updatedLayer)) %>%
        st_transform(4326)
      
      map_proxy%>%clearGroup(updatedLayer)%>%
        addPolygons(data = newData , color=getColorsMap(newData$style), group = updatedLayer, fill=TRUE,
                    popup = popupTable(newData[,2:3] %>% st_drop_geometry() %>% mutate(edit = rep(
                      "<div> 
                                                                            <button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>
                                                                          <button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>
                                                                          </div>", nrow(newData)
                    ))
                    ), 
                    layerId = newData[[columnUpdated]]) 
    } else {
      showNotification(h4("No puedes editar este layer"), action = NULL, duration = 5, 
                       id = "noEdit", "error")
    }
    
  })
  
  editedFeature <- reactiveValues()
  observe({
    print(input$mapa_shape_click)
    updatedFeature <- input$mapa_shape_click[["id"]]
    editedFeature$featureId <- updatedFeature
    
    updatedLayer <- input$mapa_shape_click[["group"]]
    req(updatedLayer != "editing")
    editedFeature$updatedLayer<- updatedLayer
    
    print(editedFeature$featureId)
    print(editedFeature$updatedLayer)
    
    # Update layer accordng to the click events
    updateSelectInput(session, "editingLayer",
                      selected = editedFeature$updatedLayer
    )
    
    req(!is.null(editedFeature$updatedLayer))
    #Update buttons according with the kind of layer
    geometryFromDatabase <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", editedFeature$updatedLayer," LIMIT 1")) %>%
      st_transform(4326) %>% st_geometry_type(by_geometry = FALSE)
    
    if(geometryFromDatabase == "LINESTRING" | geometryFromDatabase == "MULTILINESTRING"){
      runjs('
          function removeStyleElem() {
        
            var styleCircleMarker = document.getElementsByClassName("leaflet-draw-draw-circlemarker")[0];
            styleCircleMarker.style.display = "none";
            
            var styleMarker = document.getElementsByClassName("leaflet-draw-draw-marker")[0];
            styleMarker.style.display = "none";
            
            var styleCircle = document.getElementsByClassName("leaflet-draw-draw-circle")[0];
            styleCircle.style.display = "none";
            
            var styleRectangle = document.getElementsByClassName("leaflet-draw-draw-rectangle")[0];
            styleRectangle.style.display = "none";
            
            var stylePolygon = document.getElementsByClassName("leaflet-draw-draw-polygon")[0];
            stylePolygon.style.display = "none";
            
            var stylePolyline = document.getElementsByClassName("leaflet-draw-draw-polyline")[0];
            stylePolyline.style.display = "block";
          }
          
          removeStyleElem()
          ')
    } else if(geometryFromDatabase == "POLYGON" | geometryFromDatabase == "MULTIPOLYGON"){
      runjs('
          function removeStyleElem2() {
            var styleCircleMarker = document.getElementsByClassName("leaflet-draw-draw-circlemarker")[0];
            styleCircleMarker.style.display = "none";
            
            var styleMarker = document.getElementsByClassName("leaflet-draw-draw-marker")[0];
            styleMarker.style.display = "none";
            
            var styleCircle = document.getElementsByClassName("leaflet-draw-draw-circle")[0];
            styleCircle.style.display = "none";
            
            var styleRectangle = document.getElementsByClassName("leaflet-draw-draw-rectangle")[0];
            styleRectangle.style.display = "block";
            
            var stylePolygon = document.getElementsByClassName("leaflet-draw-draw-polygon")[0];
            stylePolygon.style.display = "block";
            
            var stylePolyline = document.getElementsByClassName("leaflet-draw-draw-polyline")[0];
            stylePolyline.style.display = "none";
          }
          
          removeStyleElem2()
          ')
    }
    
  })
  
  
  observeEvent(input$update_feature,{
    
    editedFeature <- input$mapa_shape_click[["id"]]
    updatedLayer <- input$mapa_shape_click[["group"]]
    columnUpdated <- dbGetQuery(conn, paste0("SELECT * FROM ",updatedLayer," LIMIT 1")) %>% select(1L) %>% names(.)
    
    if(updatedLayer == "inmuebles_rusticos" | updatedLayer == "vias_publicas_rusticas" |
       updatedLayer == "inmuebles_urbanos" | updatedLayer == "vias_publicas_urbanas" |
       updatedLayer == "bienes_revertibles" | updatedLayer == "patrimonio_suelo"){
      
      ## Enable edit button
      runjs('
          function myFunction() {
            var drawEditor = document.getElementsByClassName("leaflet-draw-toolbar")[0];
            drawEditor.style.display = "none";
            
            var styleEditor = document.getElementsByClassName("leaflet-draw-edit-edit")[0];
            styleEditor.style.display = "block";
            
            
          }
          
          myFunction()
          ')
      ## get only the polygon to be updated
      newData <- st_read(dsn = conn, geometry_column = "geom", 
                         EWKB = TRUE, 
                         query = paste0("SELECT * FROM ",updatedLayer," WHERE ", columnUpdated," = ", editedFeature)) %>%
        st_transform(4326)
      
      
      map_proxy%>%clearGroup(updatedLayer)%>%
        clearShapes() %>%
        addPolygons(data = newData , color="blue", group = "editing", 
                    layerId = newData[[columnUpdated]])
      
      #Enable cance edit button
      runjs('
          function removeCancelElem() {
        
            var cancelButton = document.getElementsByClassName("easy-button-button")[0];
            cancelButton.style.display = "block";
          }
          
          removeCancelElem()
          ')
      
    } else {
      showNotification(h4("No puedes editar este layer"), action = NULL, duration = 5, 
                       id = "noEdit", "error")
    }
    
    
  })
  
  observeEvent(input$mapa_draw_edited_features,{
    req(input$mapa_draw_edited_features)
    
    editedFeature <- input$mapa_shape_click[["id"]]
    updatedLayer <- input$editingLayer
    columnUpdated <- dbGetQuery(conn, paste0("SELECT * FROM ",updatedLayer," LIMIT 1")) %>% select(1L) %>% names(.)
    geometryInDatabase <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = paste0("SELECT * FROM ", updatedLayer," LIMIT 1")) %>%
      st_transform(4326) %>% st_geometry_type(by_geometry = FALSE)
   
    pol <- input$mapa_draw_edited_features
    
    coor<-unlist(pol$features[[1]]$geometry$coordinates[[1]])
    
    datap<-data.frame(
      Longitud=coor[seq(1,length(coor), 2)], 
      Latitud=coor[seq(2,length(coor), 2)]
    )
    poligono<-datap %>%
      st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")  %>% st_transform(4326)
    print(datap)
    print(poligono$geometry)
    geoemtryPolygon <- st_as_text(poligono$geometry)
    print(geoemtryPolygon)
    
    dbGetQuery(conn, paste0("UPDATE ", updatedLayer," SET geom = ST_GeomFromText('", geoemtryPolygon,"', 4326) WHERE ",columnUpdated ," = ",editedFeature,";"))
    
    dbGetQuery(conn, paste0("UPDATE ", updatedLayer," SET modificado_por = ",session$clientData$url_search,", fecha_modifica = ",Sys.time()," WHERE ",columnUpdated ," = ",editedFeature,";"))

    #Remoe cancel edit button
    runjs('
          function removeCancelElem() {
        
            var cancelButton = document.getElementsByClassName("easy-button-button")[0];
            cancelButton.style.display = "none";
          }
          
          removeCancelElem()
          ')
    
    # Update the map 
    reactiveData$mapLocation <- input$mapa_bounds
    
    
  })
  
  # Close modals from new feature
  observeEvent(input$cancelar,{
    removeModal()
    reactiveData$mapLocation <- input$mapa_bounds
  })
  
  #Close edit mode
  observeEvent(input$cancel_edit,{
    reactiveData$mapLocation <- input$mapa_bounds
  })
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    print(query)
  })
  
  # observe({
  #   selected_groups <- req(input$mapa_groups)
  #   print(selected_groups) 
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
