## load
source('share_load.R')

ui<- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML(""))),
  
navbarPage(
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML")),
    title = tags$a(tags$img(src = "Logo/logo_tk.png", style = "vertical-align: middle; margin-top: -7.5px", height = "35px", width = "auto"), href = "https://github.com/Temurbek-stack/"
                   ),

tabPanel("Trade Intelligence Dashboard",

    ## setup global variables
         # maxYear <- max(ex_im_uz$Period),
         # 
         # maxYear_lb <- paste0('Dec'," ", maxYear),
         # 
         # maxYear_lb <- paste0( substr(maxYear_lb, 1, 3 ),
         #                       "  ",
         #                       substr(maxYear_lb, nchar(maxYear_lb)-1, nchar(maxYear_lb) )),

    
  
    
    
    tags$head(
    #tags$script(src = "world.js" ),
    tags$script("document.title = 'Uzbekistan Trade Intelligence Dashboard'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    ),
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      ')) ,
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),

    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## head dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}')),

  
  
             

          
    ### 1.1 Export/import board ---------------------------
  #   h1(paste0("Uzbekistan trade for the ", maxYear)),
  #  # h2(paste0("Uzbekistan trade for the ", maxYear)) ,
  #   fluidRow(
  #     valueBoxOutput("ExTotBox") %>% withSpinner(type=4),
  #     valueBoxOutput("ImTotBox"),
  #     valueBoxOutput("BlTotBox")
  #   ),
  #   
  #  # p(" - "),
  #   
  #   h2(paste0("Goods")),
  #   fluidRow(
  #     valueBoxOutput("ExGBox") ,
  #     valueBoxOutput("ImGBox") ,
  #     valueBoxOutput("BlGBox")
  #   ),
  #   
  # #  p(" - "),
  #   
  #   h2(paste0("Services")),
  #   fluidRow(
  #     valueBoxOutput("ExSBox") ,
  #     valueBoxOutput("ImSBox") ,
  #     valueBoxOutput("BlSBox")
  #   ) ,
  #   
  # br(), br(),
  # 
  #### 1.2 Time serise plot ----------------------------------------
  h1(paste0("Uzbekistan Trade for the period 2000-2021")),
  fluidRow( column( width = 6,h4("Goods and services trade", align = 'center'), plotlyOutput('IEGSLineHc') %>% withSpinner(type=4) ),
            column( width = 6,h4("Trade balance", align = 'center'), plotlyOutput('GSTotalBalanceLineHc') )
  ),
  
  #### 1.3 Table shows growth rate ---------------------------------
  h2(paste0("Short, medium, and long term growth")),
  p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ,
  #fluidRow( h2(paste0("Short, medium, and long term growth")),
  #          p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ),
  fluidRow( dataTableOutput('GrowthTab')  ),
             
             
  
  ## 1.5 Line chart key commodities and services exports that are over $1bn the most recent years -------------
  h2(paste0('Trends of key commodities and services')),
  p("Click on the commodity or service names in the legend area to show their trends"),
  fluidRow( h3("key commodities and services EXPORTS", align = 'center'),
            column( width = 6, h4("Export values"), plotlyOutput('KeyExLine')  ),
            column( width = 6, h4("As a percentage of total exports"), plotlyOutput('KeyExLinePercent')  ) ),
  fluidRow( h3("key commodities and services IMPORTS", align = 'center'),
            column( width = 6, h4("Import values"), plotlyOutput('KeyImLine') ),
            column( width = 6, h4("As a percentage of total imports"), plotlyOutput('KeyImLinePercent')  ) ),
  
  ## 1.6 World map on total exports by country -----------------
  h2(paste0('Global trading partners at a glance')),
  HTML( "<p> The map shows Uzbekistan's trading partners. The size of bubble area represents the magnitude of two way trade.
                                        <span style='color:green'> Green </span> and <span style='color:red'> red </span> color indicate whether is trade
                                        <span style='color:green'> surplus </span> or <span style='color:red'> deficit. </span> </p>" ),
  fluidRow( leafletOutput('TradeMap') ),
  
  ## 1.6.1 FTA time line  -----------------
  h2(paste0('Free trade agreements in force')),
  tags$p( "The timeline below shows when the FTAs negotiation started and then put into force.
                                          Click on each FTA's name for more information.",
          tags$b( "In addition, you can select the 'FTA in force' market group under the Market Intelligence panel to get more insights." )
  ),
  tags$p("FTA stands for free trade agreement; CER stands for closer economic relations; CEP stands for closer economic partnership, and P4
                                         is short for the Trans-pacific Strategic Economic Partnership."),
  fluidRow( timevisOutput("FTATimeLine")   ),
  
  ## 1.7 Trend of key export markets --------------------
  h2(paste0('Trends of key trading partners')),
  p("Only top 5 markets are shown. Click on the country names in the legend area to show/hide their trends"),
  
  ## Two way trade and trade balance
  fluidRow( h3("Two-way trade and trade surplus/deficit (2001-2021)", align = 'center'),
            column( width = 6, h4("Trade turnover with Top 12 trading partners"), plotlyOutput("TwowayMarketLine") ),
            column( width = 6, h4("Trade surplus/deficit with Top 12 trading partners"), plotlyOutput("BalanceMarketLine") )
  ),
  
  ## total Exports
  fluidRow( h3("Total exports, goods exports and services exports (2001-2021)" ,align = 'center'),
            h4(tags$b("Total exports: Top 12 EXPORTS markets ")),
            column( width = 6, h4("Export values"), plotlyOutput("ExMarketLine") ),
            column( width = 6, h4("As a percentage of total exports"), plotlyOutput("ExMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  ),
  
  fluidRow( h4(tags$b("Goods exports: key EXPORTS markets for GOODS")),
            column( width = 6, h4("Export values"), plotlyOutput("ExGMarketLine") ),
            column( width = 6, h4("As a percentage of goods exports"), plotlyOutput("ExGMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  ),
  
  fluidRow( h4(tags$b("Services exports: key EXPORTS markets for SERVICES")),
            column( width = 6, h4("Export values"), plotlyOutput("ExSMarketLine") ),
            column( width = 6, h4("As a percentage of services exports"), plotlyOutput("ExSMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  ),
  
  ## total Imports
  fluidRow( h3("Total imports, goods imports and services imports (2001-2021)" ,align = 'center'),
            h4(tags$b("Total imports: Top 12 IMPORTS markets")),
            column( width = 6, h4("Import values"), plotlyOutput("ImMarketLine") ),
            column( width = 6, h4("As a percentage of total imports"), plotlyOutput("ImMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  ),
  
  fluidRow( h4(tags$b("Goods imports: key IMPORTS markets for GOODS")),
            column( width = 6, h4("Import values"), plotlyOutput("ImGMarketLine") ),
            column( width = 6, h4("As a percentage of goods imports"), plotlyOutput("ImGMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  ),
  
  fluidRow( h4(tags$b("Services imports: key IMPORTS markets for SERVICES")),
            column( width = 6, h4("Import values"), plotlyOutput("ImSMarketLine") ),
            column( width = 6, h4("As a percentage of services imports"), plotlyOutput("ImSMarketLinePercent") )
            #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
  )

)
)
)
