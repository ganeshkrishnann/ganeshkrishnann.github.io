library(tidyverse)
library(bulletxtrctr)
library(plotly)
library(x3ptools)
library(randomForest)
library(shiny)
library(shinyjs)
library(mlbench)
library(jpeg)
library(shinycssloaders)
library(RColorBrewer)

if (file.exists("new-comparisons.rds")) {
  comparisons <- readRDS("new-comparisons.rds")
} else {
  print("navigate to the comparisons.rds file")
  comparisons <- readRDS(file.choose())
}

if (file.exists("houston-sub.rds")) {
  bullets <- readRDS("houston-sub.rds") 
} else {
  print("navigate to the file containing the x3p files")
  bullets <- readRDS(file.choose()) 
}

bullet_opts <- unique(bullets$bullet)
land_opts_A <- unique(bullets$land)
land_opts_B <- unique(bullets$land)

bullets <- bullets %>%
  mutate(bullet = factor(bullet, levels = bullet_opts),
         land = factor(land, levels = land_opts_A))

crosscuts <- bullets %>% unnest(ccdata)
signatures <- bullets %>% unnest(sigs)

data(rtrees)
comparisons <- comparisons %>%
  mutate(barrelA = str_extract(landid1, "KA|Unk"),
         barrelB = str_extract(landid2, "KA|Unk"),
         bulletA = str_remove(landid1, "FSI01-G1-(KA|Unk)-") %>% str_remove("-L\\d") %>% factor(levels = bullet_opts),
         bulletB = str_remove(landid2, "FSI01-G1-(KA|Unk)-") %>% str_remove("-L\\d") %>% factor(levels = bullet_opts),
         landA = str_extract(landid1, "L\\d$") %>% factor(levels = land_opts_A),
         landB = str_extract(landid2, "L\\d$") %>% factor(levels = land_opts_B)) 
if (is.null(comparisons$rfscore)) {
 comparisons <- comparisons %>%
   mutate(rfscore = purrr::map_dbl(features, ~predict(rtrees, newdata = ., type = "prob")[,2]),
          ccf = purrr::map_dbl(features, "ccf"))
 write_rds(comparisons, "new-comparisons.rds")
}
  
bullet_scores <- comparisons %>% 
  group_by(bulletA, bulletB) %>% 
  tidyr::nest() %>% 
  mutate(
    bullet_score = data %>% purrr::map_dbl(
      .f = function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore)))
  )
bullet_scores %>% select(-data)

rfs <- comparisons %>%
  select(bulletA, bulletB, landA, landB, rfscore, ccf, features) %>%
  mutate(lines = comparisons$striae %>% map("lines")) %>%
  mutate(striae = comparisons$striae %>% map("lands")) 

l2l_rf <- comparisons %>%
  ggplot(aes(x = landA, y = landB, fill = rfscore)) +
  ggtitle("Land signatures") + 
  geom_tile() +
  scale_fill_gradient2(
    low = "grey80", high = "darkorange",
    midpoint = .5
  ) +
  facet_grid(bulletB ~ bulletA, labeller = "label_both") +
  xlab("Land A") +
  ylab("Land B")


# Helper functions
# Create new groove plot with manually set grooves
fix_plot <- function(bb) {
  validate(need(nrow(bb) >= 1, "Need at least one row of bullet data"),
           need(nrow(bb) <= 2, "Need at most two rows of bullet data"),
           need("ccdata" %in% names(bb), "ccdata not found"),
           need("id" %in% names(bb), "id not found"))
  
  ccdata_joint <- bb %>%
    select(id, ccdata) %>% 
    mutate(id2 = paste0("Bullet ", LETTERS[row_number()], ": ", id)) %>%
    unnest(ccdata)
  
  grooves_joint <- bb %>%
    select(id, grooves) %>%
    mutate(id2 = paste0("Bullet ", LETTERS[row_number()], ": ", id)) %>%
    mutate(grooves = purrr::map(grooves, "groove")) %>%
    unnest(grooves)
  
  left_grooves <- grooves_joint %>% group_by(id) %>% summarize(left = min(grooves))
  grooves_joint <- grooves_joint %>% left_join(left_grooves, by="id")
  ccdata_joint <- ccdata_joint %>% left_join(left_grooves, by="id")
  
  ggplot() + 
    geom_line(aes(x = x-left, y = value), data = ccdata_joint) + 
    geom_vline(aes(xintercept = grooves-left), data = grooves_joint, color = "blue") +
    facet_wrap(~id2, ncol = 1) + 
    theme_bw(base_size = 16) +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank())
}

parseFeatures <- function(namedVals) {
  named <- names(namedVals)
  values <- as.numeric(namedVals)
  data.frame(feature=named, value=values)
}

# new_land <- FALSE

ui <- fluidPage(
  titlePanel("Bullet Comparison Explorer"),
  wellPanel(
    fluidRow(
      column(width = 3,
             htmlOutput("table")
      ),
      tags$head(tags$script(src = "jquery.elevateZoom.min.js")),
      column(
        width = 4, offset = 1,
        plotlyOutput("heat1"),
        singleton(
          tags$script(src = "sample.js")
        )
      ),
      column(
        width = 4,
        plotlyOutput("heat2"),
        singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
                                       function(message) {
                                       $("#img1 img").elevateZoom({scrollZoom : true, zoomType : "lens"});
                                       $("#img2 img").elevateZoom({scrollZoom : true, zoomType : "lens"});
                                       $("#img_combined_heel_to_heel img").elevateZoom({scrollZoom : true, zoomType : "lens"});
                                       $("#img_combined_side_by_side img").elevateZoom({scrollZoom : true, zoomType : "lens"});
                                       $("#img_truncated_heel_to_heel img").elevateZoom({scrollZoom : true, zoomType : "lens"});
                                       }
                 );'))
        )
      )
      
    )
  ),
  fluidRow(
    column(
      width = 6,
      uiOutput("x3p_renders")
    ),
    column(
      width = 6,
      plotlyOutput("profiles",height = "730px")
    )
  ),
  plotlyOutput("signatures"),
  tableOutput("features")
) # fluidpage

server <- function(input, output, session) {
  
  new_land <- reactiveVal(FALSE)
  
  b2b_heatmap_sel <- eventReactive(
    event_data("plotly_click", source = "heatplot1"), {
    s <- event_data("plotly_click", source = "heatplot1")
     print(s)
     
    new_land(FALSE)
    ret <- if (length(s)) {
      bA <- s[["x"]] # Indices of plotly tile coordinates
      bB <- s[["y"]]
      bAname <- bullet_opts[bA]
      bBname <- bullet_opts[bB]
      list(bAidx = bA, bBidx = bB, bAname = bAname, bBname = bBname)
    } else {
      list()
    }
    print(ret)
    
    ret
  }, ignoreNULL = F, ignoreInit = F)
  
  l2l_heatmap_sel <- eventReactive(
    event_data("plotly_click", source = "heatplot2"), {
    s <- event_data("plotly_click", source = "heatplot2")
    # print(s)
    
    new_land(TRUE)
    ret <- if (length(s)) {
      lA <- s[["x"]] # Indices of plotly tile coordinates
      lB <- s[["y"]]
      
      lAname <- land_opts_A[lA]
      lBname <- land_opts_B[lB]
      list(lAidx = lA, lBidx = lB, lAname = lAname, lBname = lBname)
    } else {
      list()
    }
    print(ret)
    
    ret
  }, ignoreNULL = F, ignoreInit = F)
  
  
  
  rfs_react <- reactive({
    df <- rfs %>% filter(bulletA == b2b_heatmap_sel()$bAname, 
                   bulletB == b2b_heatmap_sel()$bBname)
    
    # change order of levels to move best scores into the diagonal
    scores <- bulletxtrctr:::compute_average_scores(df$landA, df$landB, df$rfscore)
    idx <- which.max(scores)
    levels <- levels(df$landB)
    p <- length(levels)
    new_order <- ((idx + 1:p -2) %% p) +1
    
    land_opts_B <<- levels(df$landB)[new_order]
    df %>%
      mutate(landB = factor(landB, levels = land_opts_B)) 
  })
  
  rfs_react_lands <- reactive({
    rfs_react() %>%
      filter(landA == levels(landA)[l2l_heatmap_sel()$lAidx], # shouldn't be re-ordered, but just in case
             landB == levels(landB)[l2l_heatmap_sel()$lBidx]) # landB might be reordered
  })
  
  bullets_react <- reactive({
    left_join(
      tibble(
        idx = 1:2,
        bullet = factor(b2b_heatmap_sel()[c("bAname", "bBname")], levels = bullet_opts),
        land = factor(l2l_heatmap_sel()[c("lAname", "lBname")], levels = land_opts_A)),
      bullets
    ) %>%
      arrange(idx)
  })

  # d <<- data.frame(bulletA = NULL, bulletB = NULL)
  # k_heat1 <- reactiveValues(a = NULL)
  # k_heat2 <- reactiveValues(a = NULL)
  # # combined_x3p_heel_to_heel<- reactiveValues(a=NULL)
  # # name_combined_x3p_heel_to_heel<- reactiveValues(a=NULL)


  output$heat1 <-
    renderPlotly({
      bs <- bullet_scores %>%
        select(-data) %>%
        ggplot(aes(x = bulletA, y = bulletB, fill = bullet_score)) +
        ggtitle("Bullet-to-Bullet Comparisons") + 
        geom_tile() +
        scale_fill_gradient2(
          low = "grey80", high = "darkorange",
          midpoint = .5
        ) + 
        scale_x_discrete(expand = c(0,0), name = "Bullet A") + 
        scale_y_discrete(expand = c(0, 0), name = "Bullet B") + 
        theme_bw(base_size = 16)


      heat1_view <- ggplotly(bs, mode = "markers", source = "heatplot1")

      if (length(b2b_heatmap_sel())) {
        # If there are bullet-to-bullet comparisons selected, display an annotated plot
        hv <- heat1_view %>% 
          add_annotations(x = b2b_heatmap_sel()$bAidx, 
                          y = b2b_heatmap_sel()$bBidx, 
                          text = "X", xref = "x", yref = "y", 
                          showarrow = FALSE, font = list(color = "black")) 
      } else {
        # If no bullet-to-bullet comparisons are selected, display an empty plot
        heat1_view
      }
    })

  output$heat2 <- renderPlotly({
    # print(b2b_heatmap_sel())
    if (length(b2b_heatmap_sel())) {
      #print(rfs_react()[,1:6])
      
      #browser()
      
      heat2 <- rfs_react() %>%
        ggplot(aes(x = landA, y = landB, fill = rfscore)) +
        geom_tile() +
        ggtitle("Land-to-Land Comparisons") + 
        scale_x_discrete(expand = c(0,0), name = "Land A") + 
        scale_y_discrete(expand = c(0, 0), name = "Land B") +
        scale_fill_gradient2(
          low = "grey80", high = "darkorange",
          midpoint = .5
        ) + 
        theme_bw(base_size = 16)
      
      heat2_view <- ggplotly(heat2, mode = "markers", source = "heatplot2")
      
      if (length(l2l_heatmap_sel()) & new_land()) {
        # If there are land to land comparisons selected, display an annotated plot
        heat2_view <- heat2_view %>%
          add_annotations(x = l2l_heatmap_sel()$lAidx, y = l2l_heatmap_sel()$lBidx, 
                          text = "X", xref = "x", yref = "y", 
                          showarrow = FALSE, font = list(color = "black"))
      } else {
        # If no land to land comparisons are selected, display a plain plot
        heat2_view
      }
      
    } else { 
      # If nothing is selected in b2b heatmap, display an empty plot
      plotly_empty()
    }
  })
  


  output$profiles <- renderPlotly({

    validate(
      need(length(b2b_heatmap_sel()) > 0, "Click on a bullet-to-bullet comparison to see more information"),
      if (length(b2b_heatmap_sel()) > 0) need(new_land() &  length(l2l_heatmap_sel()) > 0, "Click on a land-to-land comparison to see more information"))
    
    validate(need(nrow(bullets_react()) > 0, "No data found for selected lands"))
    
    fix_plot(bullets_react())
  
  })


  output$signatures <- renderPlotly({
    
    validate(
      need(length(b2b_heatmap_sel()) > 0, "Click on a bullet-to-bullet comparison to see more information"),
      if (length(b2b_heatmap_sel()) > 0) need(new_land() &  length(l2l_heatmap_sel()) > 0, "Click on a land-to-land comparison to see more information"))
    
 #   browser()
    sign_comp <- rfs_react_lands() %>%
      select(striae) %>%
      unnest(striae) %>%
      gather(sigs, value, sig1, sig2) %>%
      mutate(sigs = factor(
        sigs, levels = c("sig1", "sig2"), 
        labels = c(paste0(b2b_heatmap_sel()$bAname, "-", l2l_heatmap_sel()$lAname),
                   paste0(b2b_heatmap_sel()$bBname, "-", l2l_heatmap_sel()$lBname)))) %>%
      ggplot(aes(x = x, y = value, colour = sigs)) + geom_line() +
      scale_color_manual(name = "Signatures",
        values = c("black", "grey"),
        guide = F
      ) + 
      theme_bw(base_size = 16) + 
 #     theme(legend.position = c(1, 0), legend.justification = c(1, 0)) + 
      ggtitle("Signature Comparison")

    lines <- rfs_react_lands() %>%
      select(lines) %>% unnest(lines) 
    
    if (nrow(lines) > 0) {
    sign_comp <- sign_comp + geom_vline(
      aes(xintercept = (xmin+xmax)/2, linetype=factor(match)), 
      data = lines) +
      theme(legend.position = "bottom") +
      scale_linetype_manual("Matching striae", values=c(2,1))
    }
    peaks <- lines %>% filter(type==1)
    valleys <- lines %>% filter(type==-1)
    
    if (nrow(peaks) > 0)
    sign_comp <- sign_comp + 
      geom_vline(xintercept=(peaks$xmin+peaks$xmax)/2, colour="darkorange")
    
    if (nrow(valleys) > 0)
    sign_comp <- sign_comp +
      geom_vline(xintercept=(valleys$xmin+valleys$xmax)/2, colour="steelblue")

    ggplotly(sign_comp, mode = "markers+text", source = "sign_scatter")
  })
  
  output$features <- renderTable({
    if (!length(b2b_heatmap_sel())) {
      return(NULL)
    }
    result <- data.frame(features=NULL, values=NULL)
    
    validate(need(nrow(rfs_react()) > 0, "No data found"))
    
    if (new_land() & length(l2l_heatmap_sel())) {
      #  browser()
      result <- parseFeatures(rfs_react_lands()$features[[1]])
      result <- rfs_react_lands()$features[[1]]
    }
    
    result
  })
  
  output$table <- renderUI({
    if (!length(b2b_heatmap_sel())) {
      return(list())
    }
    
    sam_score <- rfs_react() %>%
      select(bulletA, bulletB) %>%
      unique() %>%
      left_join(bullet_scores)
    
    validate(need(nrow(rfs_react()) > 0, "No data found"))
    # print(rfs_react()$rfscore)
    lst <- list(
      h4(sprintf("Bullet A: %s", sam_score$bulletA)),
      h4(sprintf("Bullet B: %s", sam_score$bulletB)),
      hr(),
      h4(sprintf("Bullet-to-Bullet score: %.04f", sam_score$bullet_score)),
      h4(checkboxInput("predID", "match?", value = FALSE))
    )

    if (new_land() & length(l2l_heatmap_sel())) {
    #  browser()
      lst <- c(lst, 
               list(hr(),
                    h4("Land-to-Land:"),
                    h4(sprintf("Random Forest score: %.04f", rfs_react_lands()$rfscore)),
                    h4(sprintf("Cross-Correlation: %.04f", rfs_react_lands()$ccf))
               ))
    }
    
    lst
  })
  
  
  
  
  output$x3p_renders <- renderUI({
    validate(
      need(length(b2b_heatmap_sel()) > 0, "Click on a bullet-to-bullet comparison to see more information"),
      if (length(b2b_heatmap_sel()) > 0) need(new_land() & length(l2l_heatmap_sel()) > 0, "Click on a land-to-land comparison to see more information"))
    
    bullet_files <- bullets_react() %>%
      select(id, x3p, crosscut) %>%
      mutate(fname = file.path("www", paste0(id, ".png")))
    
    purrr::pwalk(bullet_files, function(id, x3p, crosscut, fname) {
      if (!file.exists(fname) | is.na(file.size(fname)) | file.size(fname) < 4096) {
        x3p %>%
          x3p_add_hline(crosscut, size = 30) %>%
          image_x3p(file = fname, size = c(1200, 400))
      } else {
        message(sprintf("%s exists. Skipping regeneration", fname))
      }
    })
    
    purrr::walk(bullet_files$fname, function(x) {
      if (!file.exists(x)) {
        message(sprintf("%s file generation failed", basename(x)))
      } else if (file.size(x) <= 4096) {
        message(sprintf("%s file is all black - rgl failed", basename(x)))
      }
    })
    
    list(
      h3(bullet_files$id[1]),
      tags$img(
        src = basename(bullet_files$fname[1]),
        "data-zoom-image" = basename(bullet_files$fname[1]), 
        width = "100%", height = "auto"
      ),  
      h3(bullet_files$id[2]),
      tags$img(
        src = basename(bullet_files$fname[2]),
        "data-zoom-image" = basename(bullet_files$fname[2]), 
        width = "100%", height = "auto"
      )
    )
    
  })


}

shinyApp(ui, server)
