library(shiny)
library(shinyFiles)
library(shinyjs)
library(fs)
library(bulletxtrctr)
library(x3ptools)
library(shinyRGL)
library(rgl)
library(tidyverse)
library(randomForest)
library(shinydashboard)
library(shinyBS)
library(DT)


reducePaths <- function(paths) {
  if (length(paths) < 2) return(paths)
  
  pathlist <- strsplit(paths, split="/")
  same <- TRUE
  for (i in 1:length(pathlist[[1]])) {
    for (j in 2:length(pathlist)) {
      same <- same & (pathlist[[j]][i] == pathlist[[1]][i])
    }
    if (!same) {
      break
    }
  }
  pathlist <- lapply(pathlist, function(x) x[-(1:i-1)])
  sapply(pathlist, paste, sep="/", collapse="/")
}

vertical_flip_transform<- function(x3p){
  x3p$surface.matrix<- x3p$surface.matrix[nrow(x3p$surface.matrix):1,] 
  x3p<- rotate_x3p(x3p, angle = 180)
}

horizontal_flip_transform<- function(x3p){
  x3p<- transpose_x3p(x3p)
  x3p<- rotate_x3p(x3p, angle = -90)
}



ifile<<- 0
idir<<- 0
file_list<<- list()
dir_list<<- list()

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

options(rgl.useNULL = TRUE)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
ui<- fluidPage(
  sidebarPanel(
    # tags$img(src = "logo.png", style = "float: left; width: 120px; margin-right: 10px; margin-top: 5px"),
    # tags$hr(),
    tags$p(),
    tags$h1("Magazine"),
    tags$hr(),
    tags$h4(" Load Explore and Diagnose (LEAD)"),
    tags$hr(),
    fluidRow(
      #   tags$p("Folder(s) to be processed:"),
      selectInput("selectID", label = "Folder(s) to be processed:", size = 4, selectize = FALSE, 
                  choices = c("None selected"), selected=NULL),
      
      column(3, shinyDirButton("directory", "Add a folder", "Select a folder containing x3p files.")),
      bsTooltip(id = "directory", title = "Select one folder at a time. You need two or more folders for batching processes.",
                placement = "bottom", trigger = "hover"),
      column(3, actionButton("new_bullet_sample",label = "Refresh view"))
    ),
    checkboxInput(inputId = "table_show", label = "Dimension & orientation conformity summary", value = FALSE),
    
    tags$hr(),
    tags$p(),
    #                     tags$p("Please select the Folders that you want to batch. \n
    #                            Please make sure that you have selected at least two folders before batching. \n
    #                            In order to select multiple folders, select the first folder, then press the button \n
    #                            'Add a folder' again and repeat the process."),
    tags$hr(),
    textInput("resolutionID", label="Resolution: ", value="NA"),
    checkboxInput(inputId = "micronscaling", label = "Scale to Microns", value = FALSE),
    conditionalPanel(condition = "input.micronscaling==1",
                     tags$p("Micron Scaling has been switched on")
    ),
    conditionalPanel(condition = "input.micronscaling==0",
                     tags$p("Micron Scaling has been switched off")
    ),
    tags$hr(),
    selectInput("interpscans", label = "Resolution Level Transformations", selectize = FALSE, 
                choices = c("No Changes", "Interpolate", "Sample"), selected=NULL),
    conditionalPanel(condition = "input.interpscans.includes('No Changes')",
                     tags$p("No resolution level changes selected")),
    conditionalPanel(condition = "input.interpscans.includes('Interpolate')",
                     textInput("resx_interp_scans", label="Interpolation Resolution", value="NA"),
                     textInput("maxgap_interp_scans", label="Set NA values to interpolate", value=1)),
    conditionalPanel(condition = "input.interpscans.includes('Sample')",
                     textInput("m_sample_scans", label="Sampling every m th number (Integer)", value=1),
                     textInput("offset_sample_scans", label="Sampling offset value (Integer between 0 and m-1)", value=0)),
    # actionButton("set_interp", "Set Interpolation")),
    tags$hr(),
    checkboxInput(inputId = "show_res_transforms", label = "Show Resolution Level Transformations", value = FALSE),
    tags$hr(),
    ##############
    
    
    fluidRow(
      column(4,checkboxInput(inputId = "sticky_transformation", label = "Cascade Transform", value = FALSE)),
      tags$header('Switch it on to get new transformations imposed on
                  previous ones, off shows them on the original image')),
    tags$hr(),
    fluidRow(
      column(4,radioButtons("rotate", "Transformations",
                            c("Off" = "off",
                              "Rotate by 90" = "rplus90",
                              "Rotate by -90" = "rminus90",
                              "Flip Vertically" = "flipvert",
                              "Flip Horizontally" = "fliphoriz"))),
      column(6, actionButton(inputId = "show", label = "Show change"),
             tags$p(),
             selectInput("transformationList", label = "Order of Changes", size = 4, selectize = FALSE, 
                         choices = c("No changes"), selected=NULL)),
      tags$hr(),
      tags$hr()
    ),# value = FALSE),
    
    tags$hr(),
    tags$hr(),
    radioButtons("batch_scripting", "Batch Scripting options",
                 c("Use current app" = "useapp",
                   "Use external script" = "dontuseapp")),
    tags$hr(),
    fluidRow(
      column(3, actionButton("batchit", "Batch It")),
      column(3, shinySaveButton("save", "Save sample file", "Save file as...", filetype = list(text = "x3p", picture = "png"))),
      column(3, shinySaveButton("savebatch", "Save all selected folders", "Save root folder as...")),
      bsTooltip(id = "save", title = "Write sample x3p or png file for reference.",
                placement = "bottom", trigger = "hover")
    )
    
      ),
  mainPanel(
    # tags$head(tags$style(
    #   type = 'text/css',
    #   'form-group { max-height: 600px; overflow-y: auto; }')),
    tags$hr(),
    tags$h2("Instructions"),
    tags$h4("1. Choose folders with bullet lands"),
    tags$h4("2. Choose appropriate scaling and transformation options"),
    tags$h4("3. Save a sample x3p and png files reflecting changes setting for a batch operation to be performed later"),
    tags$h4("4. Save all loaded files after transformation"),
    tags$h4("5. Please remember any changes will be applied to all files in all folders selected"),
    tags$h4("6. If you have special transformations for different sets of folders, please apply them in different sessions to be able to batch it correctly"),
    uiOutput("instructions"),
    verbatimTextOutput("directorypath"),
    tags$head(tags$style("#clickGene{color:red; font-size:12px; font-style:italic; 
                         overflow-y:scroll; max-height: 50px; background: ghostwhite;}")),
    tags$hr(),
    tags$h4("Sample Land Image for the chosen Bullet"),
    rglwidgetOutput('sampleImage', width = "1024px", height = "400px"),#"720px", height = "400px"),
    tags$h4("Summary of the Surface Matrix"),
    verbatimTextOutput("summarySurfaceMatrix"), ##>>
    conditionalPanel(condition = "input.rotate.includes('Rotate by 90')",
                     tags$p("Batch Rotation by 90 will be performed")
    ),
    conditionalPanel(condition = "input.rotate.includes('Rotate by -90')",
                     tags$p("Batch Rotation by -90 will be performed")
    ),
    conditionalPanel(condition = "input.rotate.includes('Flip Vertically')",
                     tags$p("Batch Vertical Flip will be performed")
    ),
    conditionalPanel(condition = "input.rotate.includes('Flip Horizontally')",
                     tags$p("Batch Horizontal Flip will be performed")
    ),
    verbatimTextOutput("eText"),
    tags$hr(),
    column(12,
           htmlOutput("table_dim1_captions"),
           tags$p(),
           dataTableOutput('table_dim1',width = "90%")
    ),
    column(12,
           htmlOutput("table_dim2_captions"),
           tags$p(),
           tags$hr(),
           dataTableOutput('table_dim2',width = "90%")    ),
    tags$p(),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    actionButton("close", "Close window")
    )
)

dir_list_shiny<- NULL
server<- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), Here = ".", "R Installation" = R.home(), getVolumes()())
  # volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session)
  shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileSave(input, "savebatch", roots = volumes, session = session, restrictions = system.file(package = "base"))
  
  b_react<- reactiveValues(a=NULL)
  react_file_list<- reactiveValues(a=NULL)
  react_dir_list<- reactiveValues(a=list())
  react_save_dir<- reactiveValues(a=NULL)
  sample_bullet<- reactiveValues(a=NULL)
  all_bullets<- reactiveValues(a=NULL)
  nonsticky_transform<- reactiveValues(a=NULL) # Used when a mid-sticky-session change in transform is made
  sticky_transformation<- reactiveValues(a=NULL)
  idiralt<- reactiveValues(a=0)
  idir_reac<- reactiveValues(a=0)
  rstick<- reactiveValues(a=0)
  indicator_previous_stick<- reactiveValues(a=0) # Determines when exactly sticky turned on
  nonx3p_folder_react<- reactiveValues(a=0)
  micron_react<- reactiveValues(a=NULL)
  pathlist_react<- reactiveValues(a=list())
  different_orientation_react<- reactiveValues(a=NULL)
  different_orientation_lands_react<- reactiveValues(a=NULL)
  
  
  b <- list()
  notif_id<- NULL
  # rstick$a<- c() # sticky transformation record
  
  ## print to console to see how the value of the shinyFiles 
  ## button changes after clicking and selection
  
  imagex3p_shiny_rgl<- function(x3p, file = NULL, col = "#cd7f32", 
                                crosscut = NA, ccParam = list(color = "#e6bf98", 
                                                              radius = 5), 
                                size = c(750, 250), zoom = 0.35, multiply = 5, 
                                ...){
    x3p <- sample_x3p(x3p, m=5 )
    surface<- x3p$surface.matrix
    z <- multiply * surface
    yidx <- ncol(z):1
    y <- x3p$header.info$incrementY * yidx
    x <- x3p$header.info$incrementX * (1:nrow(z))    
    params <- rgl::r3dDefaults
    params$windowRect <- c(40, 125, 40 + size[1], 125 + size[2])
    params$userMatrix <- diag(c(1, 1, 1, 1))
    params$zoom <- zoom
    
    open3d(params = params)
    rgl.pop("lights")
    xyz <- matrix(c(
      min(y) - diff(range(y)),
      mean(y), max(z, na.rm = TRUE)
    ), ncol = 3)
    
    light3d(
      x = xyz, diffuse = "gray40",
      specular = "gray40", ambient = "grey10", viewpoint.rel = TRUE
    )
    light3d(diffuse = "gray20", specular = "gray20")
    
    if (!is.na(crosscut)) {
      crosscutidx <- which(yidx == crosscut)
      
      colmat <- matrix(rep(col, length(z)), nrow = nrow(z), ncol = ncol(z))
      
      if (length(crosscutidx) > 0) {
        coloridx <- pmax(crosscutidx - ccParam$radius, 0):pmin(crosscutidx + ccParam$radius, ncol(z))
        colmat[, coloridx] <- ccParam$color
      } else {
        warning("Crosscut does not map to x3p file correctly.")
      }
      
      rgl.open(useNULL=T)
      return(surface3d(x, y, z, color = colmat, back = "fill"))
    } else {
      rgl.open(useNULL=T)
      return(surface3d(x, y, z, color = col, back = "fill"))
    }
  }
  
  observe({
    cat("\ninput$file value:\n\n")
    print(input$file)
  })
  
  observe({
    cat("\ninput$directory value:\n\n")
    # cat("\ninput$directory2 value:\n\n")
    
    print(input$directory)
    # print(input$directory2)
  })
  
  observe({
    cat("\ninput$save value:\n\n")
    print(input$save)
  })
  
  ## print to browser
  output$filepaths <- renderPrint({
    parseFilePaths(volumes, input$file)
    
  })
  
  observeEvent(input$file,{
    ifile<<- ifile +1 #i increases in +2 for some reason 2,4,6,8
    
    react_file_list$a<- c(isolate(react_file_list$a),list(isolate(input$file)))
    file_list<<- react_file_list$a
  })
  
  
  observeEvent(input$directory,{
    idir<<- idir +1 #i increases in +2 for some reason 2,4,6,8
    idiralt$a<- idiralt$a +1
    if(nonx3p_folder_react$a==1){
      # react_dir_list$a[length(react_dir_list$a)-1]<- NULL
      # react_dir_list$a[length(react_dir_list$a)-2]<- NULL
      react_dir_list$a[length(react_dir_list$a)]<- NULL
      # react_dir_list$a[length(react_dir_list$a)-1]<- NULL
      hhhhh<<- react_dir_list$a
      # browser()
      
    }
    react_dir_list$a<- c(isolate(react_dir_list$a),list(isolate(input$directory)))
    dir_list<<- react_dir_list$a
    
    # if(!purrr::is_empty(check_dir)){
    output$instructions<- renderUI({
      if (length(dir_list) > 1)
        tags$h4("Chosen Folders")
    })
    
    if(length(dir_list) >= 2){
      #    browser()
      
      ## Required for dir x3p check and generating list
      nk_shiny<<-length(dir_list)
      k_shiny<<- seq(2,nk_shiny,2)
      dir_list_shiny<- dir_list[k_shiny]
      volumes <- c(Home = fs::path_home(), Here = ".", "R Installation" = R.home(), getVolumes()())
      
      dir_list_shiny<- as.character(lapply(dir_list_shiny, function(x) parseDirPath(volumes, x)))
      check_dir<- dir(dir_list_shiny[length(dir_list_shiny)], pattern = ".x3p$", recursive = TRUE, full.names = TRUE)
      arbit<<- dir_list_shiny

      if(idiralt$a %% 2 == 0){
        check_dir<- dir(dir_list_shiny[length(dir_list_shiny)], pattern = ".x3p$", recursive = TRUE, full.names = TRUE)
        
        if(!purrr::is_empty(check_dir)){  
          nonx3p_folder_react$a<- 0

          b <- c()
          ## Split multiple bullets
          if(length(check_dir)>6){
            # b<- c()
            bulletidx<- list()
            b_temp<- read_bullet(dir_list_shiny[length(dir_list_shiny)])
            level_source <- levels(factor(reducePaths(dirname(b_temp$source))))
            for(i in 1:length(level_source)){
              bulletidx[i]<- list(which(reducePaths(dirname(b_temp$source))== level_source[i], arr.ind = T))
              b<- c(b, list(b_temp[as.numeric(unlist(bulletidx[i])),]))
              
            }
            b<- c(isolate(b_react$a),b)
            names(bulletidx)<- 1:length(level_source)
            # Update Path
            pathlist<- path_dir(unlist(purrr::map(b, function(x) x[['source']][[1]])))
            # pathlist_react$a<- pathlist
            
            # names(b)<- paste0("b", 1:length(b))
          }else{
            if(is.null(b)){
              # names_b_shiny<<- paste0("b",length(dir_list_shiny))
              b<- read_bullet(dir_list_shiny[length(dir_list_shiny)])
              # names(b) <- names_b_shiny
              #Update Path
              
              pathlist <- print(as.character(lapply(dir_list[seq(2,length(dir_list),2)], function(x) parseDirPath(volumes, x))))      
              # pathlist_react$a<- pathlist
            }
            b<- c(isolate(b_react$a),list(b))
            
          }
          
          ### Update the paths
          # pathlist <- print(as.character(lapply(dir_list[seq(2,length(dir_list),2)], function(x) parseDirPath(volumes, x))))      
          # only show things that are actually different
          
          
          pathlist <- reducePaths(pathlist)
          pathlist_react$a<- pathlist
          absf_pathlist<<- pathlist # for checking
          
          updateSelectInput(session, "selectID", label = NULL, 
                            choices = pathlist,
                            selected = NULL)
          
          
          names(b) <- paste0("b",1:length(b))
          
          for(i in 1:length(b)){
            
            b[[i]]$bullet<- i
            b[[i]]$land<- 1:length(b[[i]][["source"]])
          }
          
          if(length(unlist(pathlist))>1){

            dimension_ratio <<-  purrr::map(b, 
                                            function(z) purrr::map(z[["x3p"]], 
                                                                   function(x) (x[["header.info"]]$sizeY/x[["header.info"]]$sizeX))) %>%
              as_tibble %>% purrr::map(.f= unlist) %>% as.data.frame
            
            #Checks how many lands in a bullet has ration of sizeY/sizeX >1
            check_dim_ratio<<- dimension_ratio %>% 
              purrr::map(function(x) x>1) %>% as.data.frame %>% colSums
            
            # Now checking which bullets have check_dim_ratio = 6 and which have = 0
            # if something is between 0 and 6 means, some lands of that bullet have incorrect orientation
            
            
            SetA <<- pathlist[which(check_dim_ratio ==0)]
            SetB <<- pathlist[which(check_dim_ratio ==6)]
            SetC <<- pathlist[which(check_dim_ratio %in% seq(1:5))]
            
            different_orientation<- data.frame(matrix(NA,ncol = 3, nrow= max(length(SetA),length(SetB),length(SetC))))
            colnames(different_orientation)<- c("SetA", "SetB", "SetC")
            
            

            # Considering we have 6 lands in a bullet # if this changes we need to change the sums, something for later
            if(is_empty(which(check_dim_ratio %in% seq(1:5)))){

              if(!is_empty(SetA)){
                different_orientation$SetA[1:length(SetA)]<- SetA
              }
              if(!is_empty(SetB)){
                different_orientation$SetB[1:length(SetB)]<- SetB
              }
              # SetC remains NA

              different_orientation_react$a<- NULL
              
              different_orientation_react$a<- different_orientation

            }else{
              # Some 0 and 6 and some lands within bullets have different orientations
              # Bullets with different orientation but all lands within bullet have same orientation
              # The ones with 0 and 6 i.e all lands in these bullets have same orientation
              if(!is_empty(SetA)){
                different_orientation$SetA[1:length(SetA)]<- SetA
              }
              if(!is_empty(SetB)){
                different_orientation$SetB[1:length(SetB)]<- SetB
              }
              
              different_orientation$SetC[1:length(SetC)]<- SetC
              
              different_orientation_react$a<- NULL
              
              different_orientation_react$a<- different_orientation

              
              #
              # Identify bullets with different lands having different orientations
              
              temp_dimension_ratio<- dimension_ratio
              temp_dimension_ratio[,-c(which(check_dim_ratio %in% seq(1:5)))]<- NA
              
              # check_dim_ratio_withlands<- dimension_ratio[,which(check_dim_ratio %in% seq(1:5))] %>% data.frame() #, nrow = 6, ncol= length(which(check_dim_ratio %in% seq(1:6))), byrow = T)
              # colnames(check_dim_ratio_withlands)<- colnames(dimension_ratio)[which(check_dim_ratio %in% seq(1:5))]
              # check_dim_ratio_withlands<- check_dim_ratio_withlands %>%  purrr::map(function(x) x>1) %>% as.data.frame
              
              check_dim_ratio_withlands<- temp_dimension_ratio %>%  purrr::map(function(x) x>1) %>% as.data.frame
              dimland_index<- which(check_dim_ratio_withlands == FALSE, arr.ind = T)
              # cols is bullets and row is land
              different_orientation_lands<- data.frame(Bullet_Location = pathlist[dimland_index[,2]],
                                                       Set_C_Land_number = dimland_index[,1])
              
              different_orientation_lands_react$a<- NULL
              
              different_orientation_lands_react$a<- different_orientation_lands


            }
            
            
            
            
          }
          
          
          temp_view<- b[[1]]$x3p[[1]]  # Later add update slider UI to change lands and view them
          sample_bullet$a<- temp_view
          sample_bullet$a
          
          if (!is.null(sample_bullet$a)) {
            resolution <- x3p_get_scale(sample_bullet$a)
            updateTextInput(session, "resolutionID", label="Resolution: ", value = resolution)
          }
          
          if(idiralt$a == 2){    
            default_sceneLoad<- reactive({
              if(!is.null(sample_bullet$a)){
                transformed_view<- sample_bullet$a#isolate(local(rotate_example_image()))
                output$eText<- renderText({
                  dp<- "Inside Scene Load"
                  
                })
                imagex3p_shiny_rgl(transformed_view)
                # try(rgl.close())
              }else{
                output$eText <- renderText({
                  "Nothing loaded yet, get to the folders"
                })
              }
            })
            
            output$sampleImage<- renderRglwidget({
              # x3p<- read_x3p("./data/Barrel 1/Bullet 1/HS36 - Barrel 1 - Bullet 1 - Land 1 - Sneox1 - 20x - auto light left image +20% - threshold 2 - resolution 4 - Connor Hergenreter.x3p")
              if(!is.null(dir_list)){
                default_sceneLoad()
                rgl.viewpoint(phi= 5, zoom = 0.45)
                # try(rgl.close())
                rglwidget()
              }
            })
          }
          
          
          blop_alt<<- b #for checking

          b_react$a<- b
          
          all_bullets$a<- b
          # }
          
          #########################
          
        }else{
          nonx3p_folder_react$a<- 1
          if(!is.null(dir_list_shiny)){
            if(length(dir_list_shiny)==1){
              dir_list_shiny<- NULL
            }else{
              dir_list_shiny<- dir_list_shiny[-length(dir_list_shiny)]
              # browser()
              
            }
          }
          
          # idiralt$a<- idiralt$a - 2
          # Save the ID for removal later
          notif_id <<- showNotification(paste("The selected folder does not have any x3p files. Please choose a different folder"), duration = 5, type = "warning")
        }
      }
    }
    # firstload()
  })
  
  observeEvent(input$table_show,{
    if(input$table_show==1){
      if(!is.null(different_orientation_react$a)){
        output$table_dim1_captions<- renderUI({
          tags$div(
            tags$h4("Table showing bullets with different orientations"),
            tags$p("Set A and Set B show the location of bullets with different orientation. 
                   Within each bullet, all lands have same orientation in Set A and Set B. 
                   Set C shows the location of Bullets with mixed land orientations. 
                   Within each bullet, all lands DO NOT have same orientation in Set C")
            )
        })
        
        # colnames(different_orientation_react$a)<- c("height > width","width > height", "mixed")
        # Check if all bullets and lands in each Set have the same resulotion other wide make another Column
        # for the same height/width ratio but different resolution

        output$table_dim1 <- renderDataTable(different_orientation_react$a)
        
        if(!is.null(different_orientation_lands_react$a)){
          output$table_dim2_captions<- renderUI({
            tags$h4("Table showing bullets from Set C and their respective lands that have different orientations")
          })
          output$table_dim2 <- renderDataTable(different_orientation_lands_react$a)
        }
        }
      
      }else{
        output$table_dim1_captions<- NULL 
        output$table_dim1 <- NULL
        output$table_dim2_captions<- NULL
        output$table_dim2 <- NULL
      }
    
    })
  
  # Change this to input$ updateselect input id of selected
  # temp_view1<- eventReactive(input$new_bullet_sample, {
  observeEvent(input$new_bullet_sample, {
    
    if(!is.null(dir_list)){
      ############
      
      pathlist<- unlist(isolate(local(pathlist_react$a)))
      pl<<- pathlist
      abcf<<- input$selectID
      id_dir_select<<- which(pathlist== input$selectID)
      
      
      b<- local(isolate(all_bullets$a))
      
      # all_bullets$a<- b
      
      temp_view<- b[[id_dir_select]]$x3p[[1]]  # Later add update slider UI to change lands and view them
      sample_bullet$a<- temp_view
      sample_bullet$a
      # abccheck<<- sample_bullet$a
      
      refresh_sceneLoad<- reactive({
        if(!is.null(sample_bullet$a)){
          transformed_view<- sample_bullet$a#isolate(local(rotate_example_image()))
          output$eText<- renderText({
            dp<- "Inside Scene Load"
            
          })
          imagex3p_shiny_rgl(transformed_view)
          # try(rgl.close())
        }else{
          output$eText <- renderText({
            "Nothing loaded yet, get to the folders"
          })
        }
      })
      
      output$sampleImage<- renderRglwidget({
        # x3p<- read_x3p("./data/Barrel 1/Bullet 1/HS36 - Barrel 1 - Bullet 1 - Land 1 - Sneox1 - 20x - auto light left image +20% - threshold 2 - resolution 4 - Connor Hergenreter.x3p")
        if(!is.null(dir_list)){
          refresh_sceneLoad() 
          rgl.viewpoint(phi= 5, zoom = 0.45)
          # try(rgl.close())
          rglwidget()
        }
      })
      
      if (!is.null(sample_bullet$a)) {
        resolution <- x3p_get_scale(sample_bullet$a)
        updateTextInput(session, "resolutionID", label="Resolution: ", value = resolution)
      }
      
      if(!is.null(temp_view)){
        output$eText<- renderText({
          dp<- "temp view not empty, now need to show scene"#rglwidget(scene1())
          
        })
      }
      
    }else{
      output$eText<- renderText(
        dp<- "temp view is empty, now need to show scene"#rglwidget(scene1())
        # temp_view<<- img(src = "logo.png", style = "float: left; width: 120px; margin-right: 10px; margin-top: 5px")
      )
      temp_view<- matrix(1, nrow = 100, ncol = 100)
      
    }
  })
  

  observeEvent(input$micronscaling, {
    if (!is.null(sample_bullet$a)) {
      resolution <- x3p_get_scale(sample_bullet$a)
      multiply <- 1
      if (input$micronscaling) multiply<- 10^6
      updateTextInput(session, "resolutionID", label="Resolution: ", value = multiply*resolution)
    }
  })
  
  
  observeEvent({input$interpscans
    input$micronscaling},{
      
      if (!is.null(sample_bullet$a)) {
        resolution <- x3p_get_scale(sample_bullet$a)
        multiply <- 1
        if (input$micronscaling) multiply<- 10^6
        if(input$interpscans == "Interpolate"){
          updateTextInput(session, "resx_interp_scans", label="Interpolation Resolution ", value = multiply*resolution)
          # updateTextInput(session, "maxgap_interp_scans", label="Set number of NA values to interpolate", value = 1)
        }
        
      }
    })
  
  rotate_example_image<- reactive({
    
    # r$a<- input$rotate
    # mic_scaling$a<- input$micronscaling
    if(!is.null(dir_list)){ 
      
      # if(!is.null(temp_view)){
      temp_view_micron <- isolate(local(sample_bullet$a))
      if(input$micronscaling==1){
        temp_view_micron <- x3p_m_to_mum(temp_view_micron)#sample_bullet$a)
        output$eText <- renderText({
          dp<- "Micronscaling on, if statement entered"
        })
        
        temp_view_micron <- temp_view_micron
        # sample_bullet$a<- temp_view_micron
        example_image<- temp_view_micron
        
      }else{
        
        # temp_view_micron<- isolate(local(sample_bullet$a))
        example_image<-temp_view_micron
      }
      
      if(input$show_res_transforms==1 ){
        temp_res_transforms<- temp_view_micron#isolate(local(sample_bullet$a)) #sample_bullet$a
        if(input$interpscans == "Interpolate"){
          temp_res_transforms <-  interpolate_x3p(temp_res_transforms,
                                                  resx = round(as.numeric(input$resx_interp_scans),3), maxgap = as.numeric(input$maxgap_interp_scans))
          # sample_bullet$a<- temp_res_transforms
          example_image<- temp_res_transforms
        }else if(input$interpscans == "Sample"){
          temp_res_transforms <-  sample_x3p(temp_res_transforms,
                                             m = round(as.integer(input$m_sample_scans),3), offset = as.integer(input$offset_sample_scans))
          # sample_bullet$a<- temp_res_transforms
          example_image<- temp_res_transforms
        }else if(input$interpscans == "No Changes"){
          temp_res_transforms <- temp_view_micron#sample_bullet$a
          example_image<- temp_res_transforms
        }
      }else if(input$show_res_transforms==0){
        example_image<- temp_view_micron
        # sample_bullet$a
      }
      
      
      # if(input$sticky_transformation=='stickyoff'){
      if(input$sticky_transformation== 0){
        if(input$rotate == 'off'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          temp_view_rotate <- temp_view_micron
          nonsticky_transform$a<- temp_view_rotate
          rstick$a<- c() # this sets it to zero for nonsticky transf each time 
          rstick$a<- "No Change"
          # rstick$a<- c() 
          temp_view_rotate
        }else if(input$rotate == 'rplus90'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          temp_view_rotate<- rotate_x3p(temp_view_micron, angle = 90)
          nonsticky_transform$a<- temp_view_rotate
          rstick$a<- c() # this sets it to zero for nonsticky transf each time 
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- FALSE # Determines when exactly sticky turned on
          temp_view_rotate
        }else if(input$rotate == 'rminus90'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          temp_view_rotate<- rotate_x3p(temp_view_micron, angle = -90)
          nonsticky_transform$a<- temp_view_rotate
          rstick$a<- c() # this sets it to zero for nonsticky transf each time 
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- FALSE # Determines when exactly sticky turned on
          temp_view_rotate
        }else if (input$rotate == 'flipvert'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          temp_view_rotate<- temp_view_micron#transpose_x3p(temp_view_micron)
          temp_view_rotate$surface.matrix<- temp_view_rotate$surface.matrix[nrow(temp_view_rotate$surface.matrix):1,] 
          temp_view_rotate<- rotate_x3p(temp_view_rotate, angle = 180)
          nonsticky_transform$a<- temp_view_rotate
          rstick$a<- c() # this sets it to zero for nonsticky transf each time 
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- FALSE # Determines when exactly sticky turned on
          temp_view_rotate
        }else if(input$rotate == 'fliphoriz'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          temp_view_rotate<- transpose_x3p(temp_view_micron)
          temp_view_rotate<- rotate_x3p(temp_view_rotate, angle = -90)
          nonsticky_transform$a<- temp_view_rotate
          rstick$a<- c() # this sets it to zero for nonsticky transf each time 
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- FALSE # Determines when exactly sticky turned on
          # temp_view_rotate$surface.matrix<- temp_view_rotate$surface.matrix[,ncol(temp_view_rotate$surface.matrix):1] 
          temp_view_rotate
        }
        # }else if(input$sticky_transformation=='stickyon'){
      }else if(input$sticky_transformation==1){
        
        # temp_view_micron <- isolate(local(example_image()))
        # sticky_transformation$a <- temp_view_micron
        if(input$rotate == 'off'){
          temp_view_micron <- example_image #isolate(local(example_image()))
          sticky_transformation$a <- temp_view_micron
          rstick$a<- c() 
          # Below two added later
          rstick$a<- "No Change"
          rstick$a<- c() 
          sticky_transformation$a
        }else if(input$rotate == 'rplus90'){
          if(indicator_previous_stick$a == FALSE){
            temp_view_micron <- isolate(nonsticky_transform$a)
          }else{
            temp_view_micron <- isolate(sticky_transformation$a)
          }
          sticky_transformation$a<- rotate_x3p(temp_view_micron, angle = 90)
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- TRUE # Determines when exactly sticky turned on 
          sticky_transformation$a
        }else if(input$rotate == 'rminus90'){
          if(indicator_previous_stick$a == FALSE){
            temp_view_micron <- isolate(nonsticky_transform$a)
          }else{
            temp_view_micron <- isolate(sticky_transformation$a)
          }
          # temp_view_micron <- isolate(local(sticky_transformation$a))
          sticky_transformation$a<- rotate_x3p(temp_view_micron, angle = -90)
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- TRUE # Determines when exactly sticky turned on 
          sticky_transformation$a
        }else if (input$rotate == 'flipvert'){
          if(indicator_previous_stick$a == FALSE){
            temp_view_micron <- isolate(nonsticky_transform$a)
          }else{
            temp_view_micron <- isolate(sticky_transformation$a)
          }
          # temp_view_micron <- isolate(local(sticky_transformation$a))
          temp_view_rotate<- temp_view_micron#transpose_x3p(temp_view_micron)
          temp_view_rotate$surface.matrix<- temp_view_rotate$surface.matrix[nrow(temp_view_rotate$surface.matrix):1,] 
          sticky_transformation$a<- rotate_x3p(temp_view_rotate, angle = 180)
          rstick$a<- rbind(rstick$a, input$rotate)
          indicator_previous_stick$a<- TRUE # Determines when exactly sticky turned on 
          sticky_transformation$a
        }else if(input$rotate == 'fliphoriz'){
          if(indicator_previous_stick$a == FALSE){
            temp_view_micron <- isolate(nonsticky_transform$a)
          }else{
            temp_view_micron <- isolate(sticky_transformation$a)
          }
          # temp_view_micron <- isolate(local(sticky_transformation$a))
          temp_view_rotate<- transpose_x3p(temp_view_micron)
          sticky_transformation$a<- rotate_x3p(temp_view_rotate, angle = -90)
          # temp_view_rotate$surface.matrix<- temp_view_rotate$surface.matrix[,ncol(temp_view_rotate$surface.matrix):1] 
          rstick$a<- rbind(rstick$a, input$rotate) # saves the order of changes
          indicator_previous_stick$a<- TRUE # Determines when exactly sticky turned on 
          sticky_transformation$a
        }
      }
      
    }
  })
  
  
  sceneLoad<- eventReactive(input$show,{
    if(!is.null(sample_bullet$a)){
      transformed_view<- isolate(local(rotate_example_image()))
      transformed_view<- transformed_view
      
      output$eText<- renderText({
        dp<- "Inside Scene Load"
        
      })
      imagex3p_shiny_rgl(transformed_view)
      # try(rgl.close())
    }else{
      output$eText <- renderText({
        "Nothing loaded yet, get to the folders"
      })
    }
  })
  
  observeEvent(rstick$a,{
    updateSelectInput(session, "transformationList", label = NULL, 
                      choices = rstick$a,
                      selected = NULL)
    
  })
  
  observeEvent(input$show,{
    # temp_view1()
    rrr<- isolate(local(rstick$a))
    rrrr<<- rrr
    output$sampleImage<- renderRglwidget({
      # x3p<- read_x3p("./data/Barrel 1/Bullet 1/HS36 - Barrel 1 - Bullet 1 - Land 1 - Sneox1 - 20x - auto light left image +20% - threshold 2 - resolution 4 - Connor Hergenreter.x3p")
      if(!is.null(dir_list)){
        sceneLoad()
        rgl.viewpoint(phi= 5, zoom = 0.45)
        # try(rgl.close())
        rglwidget()
      }
    })
  })  
  
  
  output$savefile <- renderPrint({
    parseSavePath(volumes, input$save)
  })
  
  observeEvent(input$save, {
    # idirsave<<- idirsave +1 #i increases in +2 for some reason 2,4,6,8
    
    react_save_dir$a<- isolate(input$save)
    save_dir_shiny<<- react_save_dir$a
    volumes <<- c(Home = fs::path_home(), Here = ".", "R Installation" = R.home(), getVolumes()())
    
    save_dir_shiny<<- as.character(parseSavePath(volumes, save_dir_shiny))
    save_dir_shiny_path <- save_dir_shiny[3]
    
    if (!is.null(save_dir_shiny_path)) {
      splits <- strsplit(save_dir_shiny_path, split = "\\.")
      extension <- splits[[1]][length(splits[[1]])]
      if(extension == "x3p"){
        transformed_view<- isolate(local(rotate_example_image()))
        write_x3p(x3p = transformed_view, file = save_dir_shiny_path)
      }
      if (extension == "png") {
        transformed_view<- isolate(local(rotate_example_image()))
        # rgl.useNULL = FALSE
        # # try(rgl.close(useNULL = FALSE))
        # rgl.open(useNULL = rgl.useNULL())
        # try(rgl.close())
        # imagex3p_shiny_rgl(transformed_view)
        # rgl.snapshot(filename = save_dir_shiny_path)
        options(rgl.useNULL = F)
        image_x3p(transformed_view, file = save_dir_shiny_path)
        # try(rgl.close())
        # rgl.useNULL=TRUE
        
      }
      
    }
    
  })
  
  nsavebatch<<- 0
  react_savebatchdir_list<- reactiveValues(a=list())
  observeEvent(input$savebatch,{
    if(length(dir_list) > 1){
      nsavebatch<<- nsavebatch +1
      #    browser()
      react_savebatchdir_list$a<- c(isolate(react_savebatchdir_list$a),list(isolate(input$savebatch)))
      savebatchdir_list<<- react_savebatchdir_list$a
      
      
      if(!is_empty(savebatchdir_list) & length(savebatchdir_list) >= 1){
        volumes <<- c(Home = fs::path_home(), Here = ".", "R Installation" = R.home(), getVolumes()())
        save_batchdir_shiny<<- as.character(parseSavePath(volumes, isolate(input$savebatch)))

        save_batchdir_shiny_path <<- save_batchdir_shiny[3]
        if( nsavebatch %% 2 == 0){
          if (!is.null(save_batchdir_shiny_path)& !is_empty(save_batchdir_shiny_path)) {

            b<- isolate(local(all_bullets$a))
            b_savebatch<<- b
            # bullets<- do.call("rbind",b)
            bullets<- dplyr::bind_rows(b)

            
            if(input$micronscaling==1){
              bullets <- bullets %>% mutate(
                x3p = x3p %>% purrr::map(.f = x3p_m_to_mum)
              )
            }
            
            if(input$interpscans == "Interpolate"){
              bullets <- bullets %>% mutate(
                x3p = x3p %>% purrr::map(function(x) interpolate_x3p(x, resx = round(as.numeric(input$resx_interp_scans),3), maxgap = as.numeric(input$maxgap_interp_scans)))
              )
            }
            
            if(input$interpscans == "Sample"){
              bullets <- bullets %>% mutate(
                x3p = x3p %>% purrr::map(function(x) sample_x3p(x, m = round(as.integer(input$m_sample_scans),3), offset = as.integer(input$offset_sample_scans)))
              )
            }
            
            
            if(!is.null(rstick$a) & !is_empty(rstick$a)){

              # rstick$a<- r # This makes the loop below run for both sticky and independent transforms
              for(rstick_temp in rstick$a){
                if(rstick_temp=='rplus90'){
                  bullets <- bullets %>% mutate(
                    x3p = x3p %>% purrr::map(function(x) rotate_x3p(x, angle = 90))
                  )
                }else if(rstick_temp=='rminus90'){
                  bullets <- bullets %>% mutate(
                    x3p = x3p %>% purrr::map(function(x) rotate_x3p(x, angle = -90))
                  )
                }else if(rstick_temp== 'flipvert'){
                  bullets <- bullets %>% mutate(
                    x3p = x3p %>% purrr::map(.f =  vertical_flip_transform)
                  )
                }else if(rstick_temp=='fliphoriz'){
                  bullets <- bullets %>% mutate(
                    x3p = x3p %>% purrr::map(.f = horizontal_flip_transform)
                  )
                }
              }

            }
            
            # bullets_temp<<- bullets
            
            source_dirname<<- reducePaths(dirname(bullets$source))

            for(i in 1:length(bullets$source)){
              source_basefilename<- basename(bullets$source[i])
              batch_x3p_folderpath<- paste0(save_batchdir_shiny_path,"/",source_dirname[i])
              dir.create(batch_x3p_folderpath,recursive = T, showWarnings = F)
              batch_x3p_filename<- paste0(batch_x3p_folderpath, "/",source_basefilename)
              cat(batch_x3p_filename,"\n") # console print

              write_x3p(bullets$x3p[[i]], batch_x3p_filename)
              # lapply(bullets_temp$x3p, function(x) write_x3p(x, ))
            }
            
            levels_source_dirname<- levels(factor(source_dirname))
            
            fullpath_levels_source_dirname<- unlist(purrr::map(levels_source_dirname, function(x) paste0(save_batchdir_shiny_path, "/", x)))
            
            for(i in 1:length(fullpath_levels_source_dirname)){
              transformations_log<-  paste0(fullpath_levels_source_dirname[i], "/","transformations_log.txt")
              cat(file = transformations_log, "Applied Transformations Settings", "\n", append = TRUE)
              cat(file = transformations_log, "Folder: ", fullpath_levels_source_dirname[i], "\n", append = TRUE)
              
              cat(file = transformations_log, "Micron Scaling: ",input$micronscaling, "\n", append = TRUE)
              cat(file = transformations_log, "Resolution Level Transformations: ",input$interpscans, "\n", append = TRUE)
              if(input$interpscans == "Interpolate"){
                cat(file = transformations_log, "resx =", input$resx_interp_scans, "maxgap = ", input$maxgap_interp_scans, "\n", append = TRUE)
              }else if(input$interpscans == "Sample"){
                cat(file = transformations_log, "m = ", input$m_sample_scans, "offset = ",input$offset_sample_scans, "\n", append = TRUE)
              }
              cat(file = transformations_log, "Rotations: ", append = TRUE)
              cat(rstick$a, "\n",file = transformations_log, append = TRUE)
            }

          }
        }
      }
    }
  })
  
  observeEvent(input$batchit, {
    if(input$batch_scripting== 'useapp'){
      # process_script<- function(){
        withProgress(message = 'Scaling and Rotation', value = 0, {
          b<- isolate(local(all_bullets$a))
          bt<<- b
          # browser()
          # bullets<- do.call("rbind",b)
          bullets<- dplyr::bind_rows(b)
          
          if(input$micronscaling==1){
            bullets <- bullets %>% mutate(
              x3p = x3p %>% purrr::map(.f = x3p_m_to_mum)
            )
          }
          
          if(input$interpscans == "Interpolate"){
            bullets <- bullets %>% mutate(
              x3p = x3p %>% purrr::map(function(x) interpolate_x3p(x, resx = round(as.numeric(input$resx_interp_scans),3), maxgap = as.numeric(input$maxgap_interp_scans)))
            )
          }
          
          if(input$interpscans == "Sample"){
            bullets <- bullets %>% mutate(
              x3p = x3p %>% purrr::map(function(x) sample_x3p(x, m = round(as.integer(input$m_sample_scans),3), offset = as.integer(input$offset_sample_scans)))
            )
          }
          
          
          if(!is.null(rstick$a) & !is_empty(rstick$a)){
            # rstick$a<- r # This makes the loop below run for both sticky and independent transforms
            
            for(rstick_temp in rstick$a){
              if(rstick_temp=='rplus90'){
                bullets <- bullets %>% mutate(
                  x3p = x3p %>% purrr::map(function(x) rotate_x3p(x, angle = 90))
                )
              }else if(rstick_temp=='rminus90'){
                bullets <- bullets %>% mutate(
                  x3p = x3p %>% purrr::map(function(x) rotate_x3p(x, angle = -90))
                )
              }else if(rstick_temp== 'flipvert'){
                bullets <- bullets %>% mutate(
                  x3p = x3p %>% purrr::map(.f =  vertical_flip_transform)
                )
              }else if(rstick_temp=='fliphoriz'){
                bullets <- bullets %>% mutate(
                  x3p = x3p %>% purrr::map(.f = horizontal_flip_transform)
                )
              }
            }
          } 
          
          
          
        })
        
        withProgress(message = 'Optimize and Extract Crosscuts', value = 0, {
          bullets <- bullets %>% mutate(
            crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
          )
          
          
          # now extract the crosscuts
          bullets <- bullets %>% mutate(
            ccdata = purrr::map2(.x = x3p, .y = crosscut, 
                                 .f = x3p_crosscut)
          )
          
          crosscuts <- bullets %>% tidyr::unnest(ccdata)
        })
        
        withProgress(message = 'Locate grooves and Get Sig', value = 0, {
          bullets <- bullets %>% mutate(
            grooves = ccdata %>% 
              purrr::map(.f = cc_locate_grooves, method = "middle", 
                         adjust = 30, return_plot = TRUE)
          )
          
          bullets <- bullets %>% mutate(
            sigs = purrr::map2(
              .x = ccdata, .y = grooves, 
              .f = function(x, y) {
                cc_get_signature(
                  ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
              })
          )
          
          signatures <- bullets %>% select(source, sigs) %>% tidyr::unnest(sigs)
        })
        
        withProgress(message = 'Generate Comparisons', value = 0, {
          bullets$bulletland <- paste0(bullets$bullet,"-", bullets$land)
          lands <- unique(bullets$bulletland)
          comparisons <- data.frame(
            expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
          
          comparisons <- comparisons %>% mutate(
            aligned = purrr::map2(.x = land1, .y = land2, .f = function(xx, yy) {
              land1 <- bullets$sigs[bullets$bulletland == xx][[1]]
              land2 <- bullets$sigs[bullets$bulletland == yy][[1]]
              land1$bullet <- "first-land"
              land2$bullet <- "second-land"
              
              sig_align(land1$sig, land2$sig)
            })
          )
          
        })
        
        withProgress(message = 'Extract features/ Align', value = 0, {
          comparisons <- comparisons %>% mutate(
            ccf0 = aligned %>% 
              purrr::map_dbl(.f = function(x) extract_feature_ccf(x$lands)),
            lag0 = aligned %>% 
              purrr::map_dbl(.f = function(x) extract_feature_lag(x$lands)),
            D0 = aligned %>% 
              purrr::map_dbl(.f = function(x) extract_feature_D(x$lands)),
            length0 = aligned %>% 
              purrr::map_dbl(.f = function(x) extract_feature_length(x$lands)),
            overlap0 = aligned %>% 
              purrr::map_dbl(.f = function(x) extract_feature_overlap(x$lands))
          )
          
          comparisons <- comparisons %>% mutate(
            striae = aligned %>% purrr::map(.f = sig_cms_max, span = 75) 
          )
          
        })
        
        withProgress(message = 'Numbering Lands and Bullets', value = 0, {
          comparisons <- comparisons %>% mutate(
            matches0 = striae %>% purrr::map_dbl(.f = function(s) {
              bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = TRUE)
            }),
            mismatches0 = striae %>% purrr::map_dbl(.f = function(s) {
              bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = FALSE)
            })
            
          )
          # target_nos <- paste0('([1-', length(dir_list),'])-([1-6])') # Do not remove this
          target_nos <- paste0('([1-', 2*length(levels(factor(bullets$bullet))),'])-([1-6])')
          
          comparisons <- comparisons %>% mutate(
            bulletA = gsub(target_nos,"\\1",land1),
            bulletB = gsub(target_nos,"\\1",land2),
            # bulletC = gsub("([1-2])-([1-6])","\\1",land3),
            landA = gsub(target_nos,"\\2",land1),
            landB = gsub(target_nos,"\\2",land2)#,
            # landC = gsub("([1-2])-([1-6])","\\2",land3)
          )
          
        })
        
        withProgress(message = 'Extracting All features', value = 0, {
          comparisons <- comparisons %>% mutate(
            features = purrr::map2(.x = aligned, .y = striae, .f = extract_features_all, resolution = 1.5625)
          )
          
          comparisons <- comparisons %>% mutate(
            legacy_features = purrr::map(striae, extract_features_all_legacy, resolution = 1.5625)
          )
          
          comparisons <- comparisons %>% tidyr::unnest(legacy_features) 
          # scale features before using them in the random forest, legacy features can be used out of the box
          
          
          
        })
        
        withProgress(message = 'Random Forest Scores', value = 0, {
          # quick visualization:
          l2l_ccf<- comparisons %>% 
            ggplot(aes(x = landA, y = landB, fill = ccf)) +
            geom_tile() +
            scale_fill_gradient2(low = "grey80", high = "darkorange", 
                                 midpoint = 0.5) +
            facet_grid(bulletB~bulletA, labeller = "label_both") +
            xlab("Land A") +
            ylab("Land B")
          
          
          comparisons$rfscore <- predict(bulletxtrctr::rtrees, newdata = comparisons, type = "prob")[,2]
          
          l2l_rf<- comparisons %>% 
            ggplot(aes(x = landA, y = landB, fill = rfscore)) +
            geom_tile() +
            scale_fill_gradient2(low = "grey80", high = "darkorange", 
                                 midpoint = .5) +
            facet_grid(bulletB~bulletA, labeller = "label_both") +
            xlab("Land A") +
            ylab("Land B")
          
          
          
        })
        
        withProgress(message = 'Bullet Scores', value = 0, {
          parse_number <- readr::parse_number
          bullet_scores <- comparisons %>% group_by(bulletA, bulletB) %>% tidyr::nest()
          bullet_scores <- bullet_scores %>% mutate(
            bullet_score = data %>% purrr::map_dbl(
              .f = function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore)))
          )
          bs<- bullet_scores %>% select(-data) %>% 
            ggplot(aes(x = bulletA, y = bulletB, fill = bullet_score)) +
            geom_tile() +
            scale_fill_gradient2(low = "grey80", high = "darkorange", 
                                 midpoint = .5) 
          
        })
      #   return(list(bs = bs,bullet_scores = bullet_scores, 
      #               l2l_rf = l2l_rf, comparisons = comparisons, l2l_ccf= l2l_ccf, target_nos = target_nos, 
      #               bullets =  bullets, signatures = signatures, lands = lands, crosscuts = crosscuts))
      # }
      # processed<<- list(bs = bs,bullet_scores = bullet_scores,
      #                                 l2l_rf = l2l_rf, comparisons = comparisons, l2l_ccf= l2l_ccf, target_nos = target_nos,
      #                                 bullets =  bullets, signatures = signatures, lands = lands, crosscuts = crosscuts)
      # 
      bs <<- bs
      bullet_scores <<- bullet_scores
      l2l_rf <<- l2l_rf
      comparisons <<- comparisons
      l2l_ccf <<- l2l_ccf
      target_nos <<- target_nos
      bullets <<-  bullets
      signatures <<- signatures
      lands <<- lands
      crosscuts <<- crosscuts
      
      # processed<<- process_script()
      withProgress(message = 'Saving Data for Visualization', value = 0, {
        if (file.exists("transformations_log.txt")){
          file.remove("transformations_log.txt")
        }
          # transformations_log<-  paste0(fullpath_levels_source_dirname[i], "/","transformations_log.txt")
          cat(file = "transformations_log.txt", "Applied Transformations Settings : Batch", "\n", append = TRUE)
          cat(file = "transformations_log.txt", "Folders on which Transformation and extraction was performed: ", pathlist_react$a, "\n", append = TRUE)
          
          cat(file = "transformations_log.txt", "Micron Scaling: ",input$micronscaling, "\n", append = TRUE)
          cat(file = "transformations_log.txt", "Resolution Level Transformations: ",input$interpscans, "\n", append = TRUE)
          if(input$interpscans == "Interpolate"){
            cat(file = "transformations_log.txt", "resx =", input$resx_interp_scans, "maxgap = ", input$maxgap_interp_scans, "\n", append = TRUE)
          }else if(input$interpscans == "Sample"){
            cat(file = "transformations_log.txt", "m = ", input$m_sample_scans, "offset = ",input$offset_sample_scans, "\n", append = TRUE)
          }
          cat(file = "transformations_log.txt", "Rotations: ", append = TRUE)
          cat(rstick$a, "\n",file = "transformations_log.txt", append = TRUE)
        
        
        # save(processed, file = "preprocessing_datadump1.RData")
        # save.image("preprocessing_datadump1.RData")
        output$eText<- renderText({
          "In-app Batch operation completed"
        })
      })
      
    }else {
      b<<- isolate(local(all_bullets$a))
      # if(!is.null(rstick)){r<- rstick}
      batch_scripting_data <- list(b = b,
                                   dir_list = dir_list, 
                                   r = input$rotate, 
                                   rstick = rstick$a,
                                   mic_scaling = input$micronscaling)
      save(batch_scripting_data, file = "batch_scripting_data.RData")
      output$eText<- renderText({
        "The saving operation for running a script completed"
      })
      
    }
    
  })
  
  observeEvent(input$close, {
    js$closeWindow()
    # rgl.close()
    stopApp()
  })
  
}




shinyApp(ui, server)
