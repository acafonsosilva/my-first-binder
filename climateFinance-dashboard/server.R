library(shiny)
library(shinyFiles)
library(tidyverse)
library(readxl)
library(R3port)
library(gt)
library(psycModel)
library(lubridate)
path <- this.path::this.dir()

function(input, output, session){
  
  ## select folder with design elements
  shinyDirChoose(input, 'iconsFolder', roots = c(home = path), 
                 filetypes = 'svg')
  
  ### trying to use more simple code breaks it even if I am not sure what this is doing
  global <- reactiveValues(datapath = getwd())
  dir <- reactive(input$iconsFolder)
  observeEvent(ignoreNULL = TRUE, eventExpr = {input$iconsFolder},
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath(path)
                 global$datapath <-file.path(home, paste(unlist(dir()$path[-1]), 
                                                         collapse = .Platform$file.sep))})
  
  ## read data input file and makes it a reactive element
  input_dataset <- reactive({
    req(input$dataset)
    read_excel(input$dataset$datapath, 
               sheet = "ClimateFinance-Subratings", na = "NA",
               col_types = 'text')
  })
  
  ## updates the countryVariable in such a way the options are in the first column of the data file
  observeEvent(input$dataset, {
    freezeReactiveValue(input, "countryVariable")
    updateSelectInput(session = session, 
                      inputId = "countryVariable", 
                      choices = pull(input_dataset(),1))
  })
  
  
  ################################
  ## renders the first gt table ##
  ################################
  data1 <- reactive({
    if (is.null(input_dataset)) {
      return(NULL)}
    icons <- data.frame(icons = list.files(global$datapath, 
                                           pattern = "svg", full.names = TRUE)) %>%
      mutate(name = basename(tools::file_path_sans_ext(icons))) %>%
      separate(name, into = c("columns","content"), sep = "_", extra = "drop", 
               remove = TRUE, fill = 'left') 
    
    # countries <- read.delim('countryNames.txt') %>% 
    #   select(ISO, full) %>% 
    #   rename(Country = 'full')
    
    data <- input_dataset()
    data0 <- data %>% 
      rename(Current = 'Absolute contributions',
             Trend = 'Historic trend',	Future = 'Future commitments',
             Overseas = 'Overseas finance', Overall =	'Overall rating') %>% 
      # select(-Country) %>% 
      # left_join(countries) %>% 
      filter(!is.na(Overall)) %>% 
      replace(is.na(.), 'NA') %>% 
      relocate(c(Country,Overall,Trend,Future,Current), .after = ISO) %>% 
      select(-ISO) 
    
    data1 <- data0 %>%
      # select(-Country) %>% 
      pivot_longer(cols = -Country, 
                   names_to = 'columns', values_to = 'content')  %>%
      left_join(icons, by = c("columns", "content")) %>% 
      select(-content)
    
    tb01 <- data1 %>% 
      filter(Country %in% input$countryVariable, ##### <-
             columns %in% c('Overall','Current')) %>% 
      select(icons)
    
    tb1 <- tb01 %>% 
      gt() %>% 
      opt_table_font(font = list(google_font("Ubuntu"), default_fonts())) %>% 
      text_transform(locations = cells_body(columns = 1, rows = 1),
                     fn = function(x) { map_chr(x, ~ local_image(filename = .x,
                                                                 height = 30))}) %>%
      text_transform(locations = cells_body(columns = 1, rows = 2),
                     fn = function(x) { map_chr(x, ~ local_image(filename = .x,
                                                                 height = 22))}) %>%
      cols_align(align = "center", columns = everything()) %>%  # column headers in the center
      tab_options(  column_labels.hidden = TRUE,
                    table.font.size = 9,
                    data_row.padding = px(3), #3
                    data_row.padding.horizontal = px(1),
                    table.border.top.style = "hidden",
                    table_body.border.top.style = "hidden",
                    table_body.border.bottom.style = "hidden",
                    table_body.hlines.color = "white",  
                    table.border.bottom.style = "hidden", 
                    table.background.color = "transparent"  
      ) %>% 
      tab_header(title = paste(input$countryVariable, 'Climate Finance rating')) %>% 
      tab_style(style = cell_text(v_align="middle",  weight = 'bold', 
                                  color =  "#4F8EB5", transform = "uppercase"), 
                locations = cells_title()) 
  })
  
  output$climateFinanceRatings1 <- render_gt({
    data1()
  })
  
  
  #################################  
  ## renders the second gt table ##
  #################################
  date_stamp0 <- reactive({input$date})
  
  data2 <- reactive({
    if (is.null(input_dataset)) {
      return(NULL)}
    # date_stamp <- lubridate::stamp("Jan 2000", orders = '"%Ob %Y"(1)', quiet = TRUE)
    
    date_stamp <- date_stamp0()
    
    icons <- data.frame(icons = list.files(global$datapath, 
                                           pattern = "svg", full.names = TRUE)) %>%
      mutate(name = basename(tools::file_path_sans_ext(icons))) %>%
      separate(name, into = c("columns","content"), sep = "_", extra = "drop", 
               remove = TRUE, fill = 'left') 
    
    # countries <- read.delim('countryNames.txt') %>% 
    #   select(ISO, full) %>% 
    #   rename(Country = 'full')
    
    
    data <- input_dataset()
    data0 <- data %>% 
      rename(Current = 'Absolute contributions',
             Trend = 'Historic trend',	Future = 'Future commitments',
             Overseas = 'Overseas finance', Overall =	'Overall rating') %>% 
      # select(-Country) %>% 
      # left_join(countries) %>% 
      filter(!is.na(Overall)) %>% 
      replace(is.na(.), 'NA') %>% 
      relocate(c(Country,Overall,Trend,Future,Current), .after = ISO) %>% 
      select(-ISO) 
    
    data2 <- data0 %>%
      # select(-Country) %>% 
      pivot_longer(cols = -Country, names_to = 'columns', values_to = 'content')  %>%
      left_join(icons,by = c("columns", "content")) %>% 
      select(-content)
    
    tb02 <- data2 %>% 
      filter(Country %in% input$countryVariable,
             columns %in% c('Trend','Future','Overseas')) %>%
      pivot_wider(names_from = columns, values_from = icons) %>%   
      select(-Country) 
    
    tb2 <- tb02 %>% 
      replace(is.na(.), '') %>% 
      add_row(Trend = 'climateactiontracker.org',
              Future = '',
              Overseas = paste(date_stamp,'Update')) %>%
      gt() %>%
      opt_table_font(font = list(google_font("Ubuntu"), default_fonts())) %>%
      text_transform(locations = cells_body(columns = which(!is.na(tb02[1,])), rows = 1),
                     fn = function(x) { map_chr(x, ~ local_image(filename = .x,
                                                                 height = 22))}) %>%
      cols_align(align = "center", columns = everything()) %>%  # column headers in the center
      tab_options(  column_labels.hidden = TRUE,
                    table.font.size = 4,
                    data_row.padding = px(4), #4
                    data_row.padding.horizontal = px(4),
                    table.border.top.style = "hidden",
                    table_body.border.top.style = "hidden",
                    # table_body.border.bottom.style = "hidden",
                    table_body.hlines.color = "white",
                    table.border.bottom.style = "hidden", ### problem only solved with extra last row
                    # column_labels.border.bottom.style = "hidden",
                    table.background.color = 'transparent') %>%
      tab_style(style = cell_text(v_align="top", align="left", size = 'x-small'),
                locations = cells_body(columns = 1, rows = 2)) %>%
      tab_style(style = cell_text(v_align="top", align="right", size = 'x-small'),
                locations = cells_body(columns = 3, rows = 2)) %>%
      tab_style(style = cell_borders(sides = c("left","bottom"), color = 'white',
                                     weight = px(8), style = "solid"),
                locations = cells_body(columns = 1, rows = 1)) %>%
      tab_style(style = cell_borders(sides = c("bottom"), color = 'white',
                                     weight = px(8), style = "solid"),
                locations = cells_body(columns = 2, rows = 1)) %>%
      tab_style(style = cell_borders(sides = c("right","bottom"), color = 'white',
                                     weight = px(8), style = "solid"),
                locations = cells_body(columns = 3, rows = 1))
    
    
    
  })
  
  output$climateFinanceRatings2 <- render_gt({
    data2()
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(path, '/temp/tempTB-',input$countryVariable, "1.html")
    } ,
    content = function(file1) {
      dir.create(paste0(path, '/temp/'), showWarnings = FALSE)
      gtsave(data1(), file1)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(path, '/temp/tempTB-',input$countryVariable, "2.html")
    } ,
    content = function(file2) {
      dir.create(paste0(path, '/temp/'), showWarnings = FALSE)
      gtsave(data2(), file2)
    }
  )
  
  output$downloadFinal <- downloadHandler(filename = function() {
    paste0('CAT_rExport_ClimateFinance-',input$countryVariable, ".pdf")
  } ,
  content = function(file) {
    dir.create(paste0(path, '/temp/'), recursive = TRUE, showWarnings = FALSE)
    
    pts <- list.files(path, recursive = TRUE, full.names = T, pattern = 'tempTB-')
    file.rename(pts[[1]], paste0(path, '/temp/tb1.rawhtml'))
    file.rename(pts[[2]], paste0(path, '/temp/tb2.rawhtml'))
    
    tableHTML::make_css(list('html', 'height', '100%'),
                        list('body', c('margin', 'padding', 'font','height'),
                             c('10px', '0', '13px "Ubuntu", sans-serif', '100%')),
                        file = paste0(path, '/temp/style_manual.css'))
    
    R3port::html_combine(combine = paste0(path, '/temp/'), 
                         out = "test.html", toctheme = TRUE, clean = 2, show = FALSE,
                         css = paste0(path, '/temp/style_manual.css'))
    
    psycModel::html_to_pdf(file_path = paste0(path, '/temp/test.html'))
    file.rename(paste0(path, '/temp/test.pdf'), file)
    unlink(paste0(path, '/temp/'), recursive = TRUE)
  }
  )
}