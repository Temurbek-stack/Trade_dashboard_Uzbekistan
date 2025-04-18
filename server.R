## load
source('share_load.R')
## 000 user input setup. Please pay close attention and change -----
## un comtrade max year, you can find it on https://comtrade.un.org/data/da
tmp_un_comtrade_max_year <- year(Sys.time()) - 2 # 2 years of lag

## build server.R
server <- 
  function(input, output, session) {
    ## I. Main dashboard -----------------------------
    i_prog <- 1
    tot_step <- 25
    
    # 1. Value boxes  ---------------------------------------------------------
    ## try add progress bars
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
      # Increment the progress bar, and update the detail text.
      incProgress( i_prog/tot_step, detail = NULL)
      ##Sys.sleep(0.1)
      
    })
    i_prog <- i_prog + 1
    
    tmp_ex_g <-
      ex_im_uz %>%
      filter( Period == max(Period)-1,
              var == 'Export (Goods)'
      ) %>%
      # group_by( Period ) %>%
      # summarise( Value = round(sum(Value/10^6),0) ) %>%
      # dplyr::ungroup() %>%
      dplyr::select(value) %>%
      as.numeric
    
    ###
    tmp_ex_s <-
      ex_im_uz %>%
      filter( Period == max(Period)-1,
              var == 'Export (Services)'
      ) %>%
      # group_by( Year ) %>%
      # summarise( Value = round(sum(Value/10^6),0) ) %>%
      # dplyr::ungroup() %>%
      dplyr::select(value) %>%
      as.numeric
    
    ###
    tmp_ex_tot <- tmp_ex_g + tmp_ex_s
    
    ###
    tmp_im_g <-
      ex_im_uz %>%
      filter( Period == max(Period)-1,
              var == 'Import (Goods)'
      ) %>%
      # group_by( Year ) %>%
      # summarise( Value = round(sum(Value/10^6),0) ) %>%
      # dplyr::ungroup() %>%
      dplyr::select(value) %>%
      as.numeric
    
    ###
    tmp_im_s <-
      ex_im_uz %>%
      filter( Period == max(Period)-1,
              var == 'Import (Services)'
      ) %>%
      # group_by( Year ) %>%
      # summarise( Value = round(sum(Value/10^6),0) ) %>%
      # dplyr::ungroup() %>%
      dplyr::select(value) %>%
      as.numeric
    
    
    ###
    tmp_im_tot <- tmp_im_g + tmp_im_s
    
    ###
    tmp_balance_g <- tmp_ex_g - tmp_im_g
    tmp_balance_s <- tmp_ex_s - tmp_im_s
    tmp_balance_tot <- tmp_balance_g + tmp_balance_s
    
    ## build GOODS value boxes
    # output$ExGBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$',format(round(tmp_ex_g,2),big.mark=','), " m" ),  "font-size: 60%;"  ),
    #     VB_style( paste0("Goods exports (", round(tmp_ex_g/tmp_ex_tot*100,0) ,"%)")  ), 
    #     icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
    #     color = "green"
    #   )
    # })
    # 
    # ###
    # output$ImGBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$', format(round(tmp_im_g,2), big.mark = ','), " m"),  "font-size: 60%;"  ),
    #     paste0("Goods imports (", round(tmp_im_g/tmp_im_tot*100,0) ,"%)"),
    #     icon = icon('import', lib = 'glyphicon'),# icon("sign-out"),
    #     color = "red"
    #   )
    # })
    # 
    # ###
    # output$BlGBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( ifelse( tmp_balance_g>0, '+', '-' ), '$', format(abs(round(tmp_balance_g,2)),big.mark=','), " m"),  "font-size: 60%;"  ),
    #     "Goods balance",
    #     icon = icon("balance-scale"),
    #     color = ifelse( tmp_balance_g>0, 'green', 'red' )
    #   )
    # })
    # 
    # ## build Services value boxes
    # output$ExSBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$', format(round(tmp_ex_s,2),big.mark=','), " m"), "font-size: 60%;"  ),
    #     paste0("Services exports (", round(tmp_ex_s/tmp_ex_tot*100,0) ,"%)"),
    #     icon = icon('export', lib = 'glyphicon'),#icon("sign-in"),
    #     color = "green"
    #   )
    # })
    # 
    # ###
    # output$ImSBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$',format(round(tmp_im_s,2), big.mark = ','), " m"),"font-size: 60%;"  ),
    #     paste0("Services imports (", round(tmp_im_s/tmp_im_tot*100,0) ,"%)"),
    #     icon = icon('import', lib = 'glyphicon'), #icon("sign-out"),
    #     color = "red"
    #   )
    # })
    # 
    # ###
    # output$BlSBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( ifelse( tmp_balance_s>0, '+', '-' ),'$',format(abs(round(tmp_balance_s,2)),big.mark=','), " m"), "font-size: 60%;"  ),
    #     "Services balance",
    #     icon = icon("balance-scale"),
    #     color = ifelse( tmp_balance_s>0, 'green', 'red' )
    #   )
    # })
    # 
    # ## build Total trade value boxes
    # output$ExTotBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$',format(round(tmp_ex_tot,2),big.mark=','), " m"), "font-size: 60%;"  ),
    #     "Total exports",
    #     icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
    #     color = "green"
    #   )
    # })
    # 
    # ###
    # output$ImTotBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( '$',format(round(tmp_im_tot,2), big.mark = ','), " m"),"font-size: 60%;"  ),
    #     "Total imports",
    #     icon = icon('import', lib = 'glyphicon'), #icon("sign-out"),
    #     color = "red"
    #   )
    # })
    # 
    # ###
    # output$BlTotBox <- renderValueBox({
    #   valueBox(
    #     VB_style( paste0( ifelse( tmp_balance_tot>0, '+', '-' ),'$', format(abs(round(tmp_balance_tot,2)),big.mark=','), " m"),"font-size: 60%;"  ),
    #     "Trade balance",
    #     icon = icon("balance-scale"),
    #     color = ifelse( tmp_balance_tot>0, 'green', 'red' )
    #   )
    # })
    # 
    
        

    # 2. Total Trade a line chart  -----------------------------------------------------------------
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
       # Increment the progress bar, and update the detail text.
       incProgress( i_prog/tot_step, detail = NULL)
       ##Sys.sleep(0.1)

    })
    i_prog <- i_prog + 1
 
    tmp_dtf <-
      gather(trade_data_uz, var, value, `Goods exports`:`Services imports`, factor_key=TRUE) %>%
       filter(# Country == 'World',
               #Type_ie == 'Imports',
               Years >= (max(Years) - 20) ) %>%
       mutate( value = round(value))


    
    ##trade line chart with plotly
    output$IEGSLineHc <-renderPlotly({
      plot_ly(data = trade_data_uz, x = ~Years,
              hoverinfo = "text",
              text = ~paste("Year:", Years, "<br>",
                            "Goods exports:", `Goods exports`, "mln. USD<br>",
                            "Services exports:", `Services exports`, "mln. USD<br>",
                            "Goods imports:", `Goods imports`, "mln. USD<br>",
                            "Services imports:", `Services imports`, "mln. USD")
      ) %>%
        config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                          list("zoomIn2d"), 
                                                          list("zoomOut2d"), 
                                                          list("resetScale2d"))
               )%>%
        add_trace(y = ~`Goods exports`, 
                  name = "Goods exports",
                  line = list(width = 2),
                  mode = "lines+markers",
                  marker = list(size = 6, symbol = "square")
                  ) %>%
        add_trace(y = ~`Services exports`, 
                  name = "Services exports",
                  line = list(width = 2, dash="dot"),
                  mode = "lines+markers",
                  marker = list(size = 6, symbol = "star")
                  ) %>%
        add_trace(y = ~`Goods imports`,
                  name = "Goods imports",
                  line = list(width = 2),
                  mode = "lines+markers",
                  marker = list(size = 6, symbol = "x")
                  ) %>%
        add_trace(y = ~`Services imports`, 
                  name = "Services imports",
                  line = list(width = 3, dash="dash"),
                  mode = "lines+markers",
                  marker = list(size = 5)
                  ) %>%
        layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent",
               legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
               hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
               #title = "Trade in Uzbekistan",
               xaxis = list(title = "Years"),
               yaxis = list(title = "Million USD"))
        
        
        # layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        #        #title = "Trade in Uzbekistan",
        #        xaxis = list(title = "Years"),
        #        yaxis = list(title = "Million USD"),
        #        legend = list(x = 0.09, y = 0.98, bgcolor = 'rgba(0,0,0,0)'))

    })
    
    
    
    # 2.1 Total Trade balance a line chart  -----------------------------------------------------------------
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
       # Increment the progress bar, and update the detail text.
       incProgress( i_prog/tot_step, detail = NULL)
       ##Sys.sleep(0.1)

    })
    i_prog <- i_prog + 1

    tmp_dtf_balance <-
      gather(trade_balance_data_uz, var, value, `Goods.balance`:`Trade.balance`, factor_key=TRUE) %>%
      filter(# Country == 'World',
        #Type_ie == 'Imports',
        Years >= (max(Years) - 20) ) %>%
      mutate( value = round(value))
    
    
    
    output$GSTotalBalanceLineHc <-renderPlotly({
      plot_ly(data = trade_balance_data_uz, x = ~Years, 
              hoverinfo = "text",
              text = ~paste("Year:", Years, "<br>",
                            "Goods balance:", Goods.balance, "mln. USD<br>",
                            "Services balance:", Services.balance, "mln. USD<br>",
                            "Trade balance:", Trade.balance, "mln. USD")
      ) %>% 
        config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                          list("zoomIn2d"), 
                                                          list("zoomOut2d"), 
                                                          list("resetScale2d"))
        ) %>%
        add_trace(
          y = ~round(Goods.balance,1),
          name = "Goods balance",
          line = list(width = 2),
          mode = "lines+markers",
          marker = list(size = 6, symbol = "square")
        ) %>%
        add_trace(
          y = ~Services.balance,
          name = "Services balance",
          line = list(width = 2, dash="dot"),
          mode = "lines+markers",
          marker = list(size = 6, symbol = "star")
        ) %>%
        add_trace(
          y = ~Trade.balance,
          name = "Trade balance",
          line = list(width = 2, dash="dash"),
          marker = list(size = 6, symbol = "x")
        ) %>%
        layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", 
               legend = list(x = 0.1, y = 0.3, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
               hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
               #title = "Trade Balance",
               xaxis = list(title = "Years"),
               yaxis = list(title = "Million USD")
        )

      })
    
    
    
    # 3. Growth prospective ---------------------------------------------------
    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
       # Increment the progress bar, and update the detail text.
       incProgress( i_prog/tot_step, detail = NULL)
       ##Sys.sleep(0.1)

    })
    i_prog <- i_prog + 1

    tmp_dtf2 <- as_tibble(tmp_dtf) %>%
      mutate(type_ie=substring(var, regexpr(" ", var) + 1)) %>%
      mutate(type_gs=gsub( " .*$", "", var ))
   
    # detach(package:plyr) 
    tmp_tot <-
      tmp_dtf2 %>%
      dplyr::group_by( type_ie, Years ) %>%
      summarise( value = sum(value,na.rm=T) ) %>%
      ungroup( ) %>%
      mutate( Name = paste0('Total', ' ', tolower(type_ie)) )

    tmp_tab <-
       tmp_dtf2 %>%
       mutate( Name = paste0( type_gs,' ', tolower(type_ie) ) ) %>%
       bind_rows( tmp_tot ) %>%
       group_by( Name) %>%
       mutate( CAGR1 = CAGR( value[Years == max(Years)]/value[Years == (max(Years)-1)], 1)/100,
               CAGR5 = CAGR( value[Years == max(Years)]/value[Years == (max(Years)-5)], 5)/100,
               CAGR10 = CAGR( value[Years == max(Years)]/value[Years == (max(Years)-10)], 10)/100,
               CAGR20 = CAGR( value[Years == max(Years)]/value[Years == (max(Years)-20)], 20)/100
       ) %>%
       ungroup %>%
       filter( Years == max(Years) ) %>%
       dplyr::select( Name, value, CAGR1, CAGR5, CAGR10, CAGR20 ) %>%
       mutate( Name = factor(Name, levels = c("Total exports",
                                     'Goods exports',
                                     'Services exports',
                                     'Total imports',
                                     'Goods imports',
                                     'Services imports')) ) %>%
       arrange( Name )


    output$GrowthTab <- renderDataTable({
       datatable( tmp_tab,
                  rownames = F,
                  extensions = 'Buttons',
                  options = list(dom = 'Bt',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 scrollX = TRUE) ,
                  colnames=c(" ", 'Value ($m)', 'CAGR 1', 'CAGR 5', 'CAGR 10', 'CAGR 20')
                 ) %>%
          formatStyle(columns = 'Name',
                      target = 'row',
                      fontWeight = styleEqual(c('Total imports','Total exports'), c('bold','bold')),
                      backgroundColor = styleEqual(c('Total imports','Total exports'), c('lightgrey','lightgrey'))
                     ) %>%
          formatStyle(
             c('CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'),
             background = styleColorBar( c(0,max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10, tmp_tab$CAGR20))*2) , 'lightblue'),
             backgroundSize = '100% 90%',
             backgroundRepeat = 'no-repeat',
             backgroundPosition = 'center'
          ) %>%
          formatPercentage( c('CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'),digit = 1 ) %>%
          formatStyle( columns = c('Name','value','CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'), `font-size`= '115%' ) %>%
          formatCurrency( columns = c('value'), mark = " ", digits = 0)
    })


    # ## remove the waiting message --
    # removeUI( selector = '#main_wait_message' )

    
    
    
    

    # # 7.10  Show more button --------------------
    # observeEvent( input$btn_show_more,
    #               {
    # 
    #                  ## disable the buttone ---
    #                  shinyjs::disable("btn_show_more")
    #                  ## --- hide message to show more -----
    #                  shinyjs::hide(id = 'message_to_show_more')
    #                  ## --- show loading message ---
    #                  shinyjs::show( id = "load_more_message" )
    # 
       
                    withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     
                       KPExp <- round(KeyProdExp[ ,c(2:13)],1)
                       KPExp$Years <- KeyProdExp$Years
                     
                       ### plot
                       output$KeyExLine <- renderPlotly({
                         plot_ly(data = KPExp, x = ~Years,
                                 hoverinfo = "text",
                                 text = ~paste("Year:", Years, "<br>",
                                               "Cotton:", Cttn, "mln. USD<br>",
                                               "Chemical products:", ChmProd, "mln. USD<br>",
                                               "Black metals:", BlckM, "mln. USD<br>",
                                               "Non-ferrous metals:", NFerM, "mln. USD<br>",
                                               "Energy and oil products:", Nrg, "mln. USD<br>",
                                               "Cars and equipment:", CarsEq, "mln. USD<br>",
                                               "Gold:", Gold, "mln. USD<br>",
                                               "Food:", Food, "mln. USD<br>",
                                               "Construction:", Constr, "mln. USD<br>",
                                               "Transport:", Transport, "mln. USD<br>",
                                               "Trip:", Trip, "mln. USD<br>",
                                               "Other services:", Other, "mln. USD")
                                 
                         )%>%
                           config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                             list("zoomIn2d"), 
                                                                             list("zoomOut2d"), 
                                                                             list("resetScale2d"))
                           )%>%
                           add_trace(
                             y = ~Cttn,
                             name = "Cotton",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~ChmProd,
                             name = "Chemical products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~BlckM,
                             name = "Black metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~NFerM,
                             name = "Non-ferrous metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Nrg,
                             name = "Energy and oil products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~CarsEq,
                             name = "Cars and equipment",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Gold,
                             name = "Gold",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Food,
                             name = "Food",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~Constr,
                             name = "Construction",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Transport,
                             name = "Transport",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~Trip,
                             name = "Trip",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           add_trace(
                             y = ~Other,
                             name = "Other services",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                  # legend = list(x = 0.1, y = 1, opacity = 0),
                                  legend = list(x = 0.1, y = 0.97, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                  hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                  #title = "Key Commodities and Services Exports",
                                  xaxis = list(title = "Years"),
                                  yaxis = list(title = "Million USD")
                           )
                             
                                                  
                       })
                      


                      # 5.0.1 Top key commodities exports over time -- Percentage -------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     
                       KPExpPerc <- round(KeyProdExpPerc[ ,c(2:13)],1)
                       KPExpPerc$Years <- KeyProdExpPerc$Years
                      
                       ##plot
                       output$KeyExLinePercent <- renderPlotly({
                         plot_ly(data = KPExpPerc, x = ~Years,
                                 hoverinfo = "text",
                                 text = ~paste("Year:", Years, "<br>",
                                               "Cotton:", Cttn, "%<br>",
                                               "Chemical products:", ChmProd, "%<br>",
                                               "Black metals:", BlckM, "%<br>",
                                               "Non-ferrous metals:", NFerM, "%<br>",
                                               "Energy and oil products:", Nrg, "%<br>",
                                               "Cars and equipment:", CarsEq, "%<br>",
                                               "Gold:", Gold, "%<br>",
                                               "Food:", Food, "%<br>",
                                               "Construction:", Constr, "%<br>",
                                               "Transport:", Transport, "%<br>",
                                               "Trip:", Trip, "%<br>",
                                               "Other services:", Other, "%")
                                 
                         )%>%
                           config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                             list("zoomIn2d"), 
                                                                             list("zoomOut2d"), 
                                                                             list("resetScale2d")))%>%
                           add_trace(
                             y = ~Cttn,
                             name = "Cotton",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~ChmProd,
                             name = "Chemical products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~BlckM,
                             name = "Black metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~NFerM,
                             name = "Non-ferrous metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Nrg,
                             name = "Energy and oil products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~CarsEq,
                             name = "Cars and equipment",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Gold,
                             name = "Gold",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Food,
                             name = "Food",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~Constr,
                             name = "Construction",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Transport,
                             name = "Transport",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~Trip,
                             name = "Trip",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           add_trace(
                             y = ~Other,
                             name = "Other services",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                  # legend = list(x = 0.1, y = 1, opacity = 0),
                                  legend = list(x = 0.1, y = 0.97, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                  hoverlabel=list(bgcolor='rgba(255,255,255,0.1)', font=list(color='black')),
                                  #title = "Key Commodities and Services Imports <br> as a percentage of total exports",
                                  xaxis = list(title = "Years"),
                                  yaxis = list(title = "%")
                           )
                         
                       })

                       
                       
                     # 5.1 Top key commodities and import over time -----------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                      KPImp <- round(KeyProdImp[ ,c(2:13)],1)
                      KPImp$Years <- KeyProdImp$Years
                     
                      ##plot
                      output$KeyImLine <- renderPlotly({
                        plot_ly(data = KPImp, x = ~Years,
                                hoverinfo = "text",
                                text = ~paste("Year:", Years, "<br>",
                                              "Cotton:", Cttn, "mln. USD<br>",
                                              "Chemical products:", ChmProd, "mln. USD<br>",
                                              "Black metals:", BlckM, "mln. USD<br>",
                                              "Non-ferrous metals:", NFerM, "mln. USD<br>",
                                              "Energy and oil products:", Nrg, "mln. USD<br>",
                                              "Cars and equipment:", CarsEq, "mln. USD<br>",
                                              "Gold:", Gold, "mln. USD<br>",
                                              "Food:", Food, "mln. USD<br>",
                                              "Construction:", Constr, "mln. USD<br>",
                                              "Transport:", Transport, "mln. USD<br>",
                                              "Trip:", Trip, "mln. USD<br>",
                                              "Other services:", Other, "mln. USD")
                                
                        ) %>%
                          config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                            list("zoomIn2d"), 
                                                                            list("zoomOut2d"), 
                                                                            list("resetScale2d")))%>%
                          add_trace(
                            y = ~Cttn,
                            name = "Cotton",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~ChmProd,
                            name = "Chemical products",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5)
                          ) %>%
                          add_trace(
                            y = ~BlckM,
                            name = "Black metals",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~NFerM,
                            name = "Non-ferrous metals",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~Nrg,
                            name = "Energy and oil products",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5)
                          ) %>%
                          add_trace(
                            y = ~CarsEq,
                            name = "Cars and equipment",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5)
                          ) %>%
                          add_trace(
                            y = ~Gold,
                            name = "Gold",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~Food,
                            name = "Food",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~Constr,
                            name = "Construction",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~Transport,
                            name = "Transport",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          add_trace(
                            y = ~Trip,
                            name = "Trip",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          
                          add_trace(
                            y = ~Other,
                            name = "Other services",
                            line = list(width = 2),
                            mode = "lines+markers",
                            marker = list(size = 5),
                            visible = "legendonly"
                          ) %>%
                          
                          layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                 # legend = list(x = 0.1, y = 1, opacity = 0),
                                 legend = list(x = 0.1, y = 0.97, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                 hoverlabel=list(bgcolor='rgba(255,255,255,0.1)', font=list(color='black')),
                                 #title = "Key Commodities and Services Imports",
                                 xaxis = list(title = "Years"),
                                 yaxis = list(title = "Million USD")
                          )
                      })
                      

 
                     # 5.1.1 Top key commodities and import over time Percent -----------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                       KPImpPerc <- round(KeyProdImpPerc[ ,c(2:13)],1)
                       KPImpPerc$Years <- KeyProdImpPerc$Years
                       
                       output$KeyImLinePercent <- renderPlotly({
                         plot_ly(data = KPImpPerc, x = ~Years,
                                 hoverinfo = "text",
                                 text = ~paste("Year:", Years, "<br>",
                                               "Cotton:", Cttn, "%<br>",
                                               "Chemical products:", ChmProd, "%<br>",
                                               "Black metals:", BlckM, "%<br>",
                                               "Non-ferrous metals:", NFerM, "%<br>",
                                               "Energy and oil products:", Nrg, "%<br>",
                                               "Cars and equipment:", CarsEq, "%<br>",
                                               "Gold:", Gold, "%<br>",
                                               "Food:", Food, "%<br>",
                                               "Construction:", Constr, "%<br>",
                                               "Transport:", Transport, "%<br>",
                                               "Trip:", Trip, "%<br>",
                                               "Other services:", Other, "%")
                                 
                         ) %>%
                           config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                             list("zoomIn2d"), 
                                                                             list("zoomOut2d"), 
                                                                             list("resetScale2d")))%>%
                           add_trace(
                             y = ~Cttn,
                             name = "Cotton",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~ChmProd,
                             name = "Chemical products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~BlckM,
                             name = "Black metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~NFerM,
                             name = "Non-ferrous metals",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Nrg,
                             name = "Energy and oil products",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~CarsEq,
                             name = "Cars and equipment",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5)
                           ) %>%
                           add_trace(
                             y = ~Gold,
                             name = "Gold",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Food,
                             name = "Food",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Constr,
                             name = "Construction",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Transport,
                             name = "Transport",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           add_trace(
                             y = ~Trip,
                             name = "Trip",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           add_trace(
                             y = ~Other,
                             name = "Other services",
                             line = list(width = 2),
                             mode = "lines+markers",
                             marker = list(size = 5),
                             visible = "legendonly"
                           ) %>%
                           
                           layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                  # legend = list(x = 0.1, y = 1, opacity = 0),
                                  legend = list(x = 0.1, y = 0.97, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                  hoverlabel=list(bgcolor='rgba(255,255,255,0.1)', font=list(color='black')),
                                  #title = "Key Commodities and Services Imports<br> as a percentage of total imports",
                                  xaxis = list(title = "Years"),
                                  yaxis = list(title = "%")
                           )
                         
                       })

                       
                                    
                     # 6. Global trading partners glance ---------------------------------------
                       ##map partners
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                         # Increment the progress bar, and update the detail text.
                         incProgress( i_prog/tot_step, detail = NULL)
                         ##Sys.sleep(0.1)
                         
                       })
                       i_prog <- i_prog + 1
                       
                       trade_partners_with_sf_deficit <- trade_partners_with_sf %>%
                         mutate(max_total=max(Total),
                                pie_size=70 * sqrt(Total) / sqrt(max_total),
                                pie_size=replace(pie_size, Total<1000000, log(Total)/log(max_total)*20))%>%
                                # Export=round(Export, 2),
                                # Import=round(Import, 2)) %>%
                         filter(balance<0)
                       
                       trade_partners_with_sf_surplus <- trade_partners_with_sf %>%
                         mutate(max_total=max(Total),
                                pie_size=70 * sqrt(Total) / sqrt(max_total),
                                pie_size=replace(pie_size, Total<1000000, log(Total)/log(max_total)*20))%>%
                                # Export=round(Export, 2),
                                # Import=round(Import, 2)) %>%
                         filter(balance>=0)
                       
                       colors_deficit <- c("#FF9999", "#CC0000")
                       colors_surplus <- c("#00CC00", "#99FF99")
                       
                       output$TradeMap <-renderLeaflet({
                         leaflet() %>% 
                           addTiles() %>% 
                           addProviderTiles("Esri", group = "Esri")  %>%
                           addMinicharts(
                             trade_partners_with_sf_deficit$longitude, trade_partners_with_sf_deficit$latitude,
                             type = "pie",
                             chartdata = select(trade_partners_with_sf_deficit,Export,Import), 
                             # width = 60 * sqrt(trade_partners_with_sf_deficit$Total) / sqrt(trade_partners_with_sf_deficit$max_total), transitionTime = 0,
                             width = trade_partners_with_sf_deficit$pie_size, transitionTime = 0,
                             colorPalette = colors_deficit,
                             opacity = 0.7,
                             legend = FALSE,
                             popup=popupArgs(
                               # labels=c("Country", "Export:", "Foussball"),
                               html=paste0("<h4>", trade_partners_with_sf_deficit$country, "</h4>",
                                           "<span style='color:#FF9999'> Export:  </span>", round(trade_partners_with_sf_deficit$Export, 2), "<br>",
                                           "<span style='color:#CC0000'> Import:  </span>", round(trade_partners_with_sf_deficit$Import, 2)
                               )
                             )
                           ) %>%
                           addMinicharts(
                             trade_partners_with_sf_surplus$longitude, trade_partners_with_sf_surplus$latitude,
                             type = "pie",
                             chartdata = select(trade_partners_with_sf_surplus,Export,Import), 
                             #width = 60 * sqrt(trade_partners_with_sf_surplus$Total) / sqrt(trade_partners_with_sf_surplus$max_total), transitionTime = 0,
                             width = trade_partners_with_sf_surplus$pie_size, transitionTime = 0,
                             colorPalette = colors_surplus,
                             opacity = 0.7,
                             legend = FALSE,
                             popup=popupArgs(
                               # labels=c("Country", "Export:", "Foussball"),
                               html=paste0("<h4>", trade_partners_with_sf_surplus$country, "</h4>",
                                           "<span style='color:#00CC00'> Export:  </span>", round(trade_partners_with_sf_surplus$Export, 2), "<br>",
                                           "<span style='color:#99FF99'> Import:  </span>", round(trade_partners_with_sf_surplus$Import, 2)
                               )
                             )
                           )
                         
                         
                       })
                       

                     # 7.0 FTA timeline ----------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     
                     data_fta <- data.frame(
                       No = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                       Title = c("Uzbekistan - Economic Corporation Organization Trade agreement",
                                 "Kyrgyz-Uzbekistan Free Trade agreement",
                                 "Azerbaijan- Uzbekistan Free Trade agreement",
                                 "Tajikistan-Uzbekistan Free Trade agreement",
                                 "Kazakhstan-Uzbekistan Free Trade agreement",
                                 "Georgia-Uzbekistan Free Trade agreement",
                                 "Uzbekistan -Moldova Free Trade agreement",
                                 "Uzbekistan-Ukraine FTA",
                                 "Uzbekistan -Russia Free Trade agreement"),
                       Type = rep("FTA", 9),
                       Status = c("In force", "In effect", "In effect", "In effect", "In effect", "In effect", "In effect", "In effect", "In effect"),
                       Parties = c("Economic Corporation Organization Trade agreement",
                                   "Kyrgyzstan",
                                   "Azerbaijan",
                                   "Tajikistan",
                                   "Kazakhstan",
                                   "Georgia",
                                   "Moldova",
                                   "Ukraine",
                                   "Russia"),
                       DateOfSignature = c(2003, 1996, 1996, 1996, 1997, 1995, 1995, 2006, 1993),
                       DateOfEntryIntoForce = c(2008, 1998, 1996, 1996, 1997, 1995, 1995, NA, 1993)
                     )
                     
                     
                     timeline_data <- data.frame(
                       id = data_fta$No,
                       content = data_fta$Title,
                       start = data_fta$DateOfSignature,
                       end = data_fta$DateOfEntryIntoForce
                     )
                     
                     timeline_data <- timeline_data %>% mutate(across(c(start, end), as.Date, origin = "1991-01-01"))
                     
                     #timevis(data = timeline_data, options = list(align = 'left'))
                     
                     
                     output$FTATimeLine <- renderTimevis({ timevis(data = timeline_data, options = list(align = 'left')) })
                     


    #                  ### FTA infomration
    #                  # dtf_fta <- 
    #                  #    data.frame(
    #                  #       id = 1:9,
    #                  #       content = c("<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-china-free-trade-agreement/' target = '_blank'> NZ-China FTA </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-australia-closer-economic-relations-cer/' target = '_blank'> NZ-Australia CER </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/aanzfta-asean-australia-new-zealand-fta/' target = '_blank'> AANZFTA </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/hong-kong-fta/' target = '_blank'> NZ-Hong Kong, China CEP </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/malaysia-fta/' target = '_blank'> NZ-Malaysia FTA </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/singapore/' target = '_blank'> NZ-Singapore CEP </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/thailand/' target = '_blank'> NZ-Thailand CEP </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/p4/' target = '_blank'> P4 </a>",
    #                  #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-korea-free-trade-agreement/' target = '_blank'> NZ-Korea FTA </a>"),
    #                  #       ## time when FTAs in forece
    #                  #       start = c("2008-04-07",# cn
    #                  #                 "1983-01-01",# aus
    #                  #                 "2010-01-01",# asean
    #                  #                 "2011-01-01",# hk
    #                  #                 "2010-08-01", #my
    #                  #                 "2001-01-01", #sing
    #                  #                 "2005-07-01", # Thai
    #                  #                 "2006-01-01", #p4
    #                  #                 "2015-12-20"
    #                  #       )
    #                  #       
    #                  #    )
    #                  # 
    #                  # output$FTATimeLine <- 
    #                  #    renderTimevis({ timevis(dtf_fta) })
    #                  
    #                  ### FTA infomration
    #                  groups <- 
    #                     data.frame( id = c('cn', 'aus', 'asean',
    #                                        'hk', 'my', 'sin',
    #                                        'thai', 'p4', 'sk'),
    #                                 content =c("<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-china-free-trade-agreement/' target = '_blank'> NZ-China FTA </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-australia-closer-economic-relations-cer/' target = '_blank'> NZ-Australia CER </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/aanzfta-asean-australia-new-zealand-fta/' target = '_blank'> AANZFTA </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/hong-kong-fta/' target = '_blank'> NZ-Hong Kong, China CEP </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/malaysia-fta/' target = '_blank'> NZ-Malaysia FTA </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/singapore/' target = '_blank'> NZ-Singapore CEP </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/thailand/' target = '_blank'> NZ-Thailand CEP </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/p4/' target = '_blank'> P4 </a>",
    #                                            "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-korea-free-trade-agreement/' target = '_blank'> NZ-Korea FTA </a>")
    #                     )
    #                  
    #                  
    #                  dtf_fta <- 
    #                     data.frame(
    #                        id = 1:9,
    #                        content = c("5 years",
    #                                    "3 years",
    #                                    "5 years",
    #                                    "10 years",
    #                                    "5 years",
    #                                    "1 year and 4 months",
    #                                    "1 year",
    #                                    "2 years and 4 months",
    #                                    "6 years and 6 months"
    #                        ) ,
    #                        ## talk started
    #                        start = c("2003-10-01", # cn
    #                                  "1979-12-31", #aus
    #                                  "2005-03-01", #asean
    #                                  "2001-01-01", #hk
    #                                  "2005-03-01", # my
    #                                  "1999-09-01", #singapore
    #                                  "2004-06-01", ## thia
    #                                  "2003-09-01", #p4
    #                                  "2009-06-01" #sk
    #                        ),
    #                        ## time when FTAs in forece
    #                        end = c("2008-10-01",# cn
    #                                "1983-01-01",# aus
    #                                "2010-01-01",# asean
    #                                "2011-01-01",# hk
    #                                "2010-08-01", #my
    #                                "2001-01-01", #sing
    #                                "2005-07-01", # Thai
    #                                "2006-01-01", #p4
    #                                "2015-12-20" # sk
    #                        ),
    #                        group = c('cn', 'aus', 'asean',
    #                                  'hk', 'my', 'sin',
    #                                  'thai', 'p4', 'sk') #,
    #                        #type = 'range'
    #                        
    #                     )
    #                  
    #                  output$FTATimeLine <-
    #                     renderTimevis({ timevis(data = dtf_fta, groups = groups, options = list(align = 'left'))  })
    #                  
    #                  
                     # 7.1 Key exports market trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     tmp_data_country_ex <- filter(df_countries_95_21_totals, Flow == "Exports")
                     
                     # top 12 countries by value in 2020
                     tmp_data_exports_top12 <- tmp_data_country_ex %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12) %>%
                       pull(Country)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_ex_final <- tmp_data_country_ex %>%
                       filter(Country %in% tmp_data_exports_top12,
                              Year >= 2001,
                              Year <= 2020)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_ex_plot <- tmp_data_country_ex_final[c(21:80),]
                     
                     
                     ### plot
                     output$ExMarketLine <- renderPlotly({
                       plot_ly(data = tmp_data_country_ex_plot,
                               x = ~Year, 
                               hoverinfo = 'text',
                               hovertext = ~paste0("Year: ", Year, "<br>",
                                                   "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                         config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                           list("zoomIn2d"), 
                                                                           list("zoomOut2d"), 
                                                                           list("resetScale2d"),
                                                                           list("hoverClosestCartesian"), 
                                                                           list("hoverCompareCartesian")
                         ))%>%
                         
                         add_trace(
                           y = ~Value,
                           line = list(width = 2),
                           color = ~as.factor(Country),
                           colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                      "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                           mode = "lines+markers",
                           marker = list(size = 6)
                         ) %>%
                         add_trace( data = tmp_data_country_ex_final[c(1:20, 81:240),],
                                    y = ~Value,
                                    line = list(width = 2),
                                    color = ~as.factor(Country),
                                    colors = c("#17becf", "#2ca02c"),
                                    mode = "lines+markers",
                                    marker = list(size = 6),
                                    visible = "legendonly"
                                    
                         )%>%
                         
                         layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                #title = "Dynamics of Total Exports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Exports (bln. USD)")
                         )
                       
                       })

                     # 7.1.1 Key exports market trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     # get the top 12
                     tmp_data_exports_top12_value <- tmp_data_country_ex %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     # total export value
                     tmp_data_exports_top12_value <- sum(tmp_data_exports_top12_value$Value, na.rm = TRUE)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_ex_final <- tmp_data_country_ex %>%
                       filter(Country %in% tmp_data_exports_top12,
                              Year >= 2001,
                              Year <= 2020)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_ex_pc <- tmp_data_country_ex_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_exports_top12_value * 100) %>%
                       # round the Percentage column to two decimal places
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_ex_pc_plot <- tmp_data_country_ex_pc[c(21:80),]
                     
                     ##plot
                     output$ExMarketLinePercent <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_ex_pc_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", Percentage, " %"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Percentage,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_ex_pc[c(1:20, 81:240),],
                                       y = ~Percentage,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Export Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Export Percentage, %")
                            )
                        })

                 # 7.2 Key imports market trend line ------------------------------------
                 withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                    # Increment the progress bar, and update the detail text.
                    incProgress( i_prog/tot_step, detail = NULL)
                    ##Sys.sleep(0.1)

                 })
                 i_prog <- i_prog + 1

                 tmp_data_country_im <- filter(df_countries_95_21_totals, Flow == "Imports")
                 
                 # top 12 countries by value in 2020
                 tmp_top_country_im_top12 <- tmp_data_country_im %>%
                   filter(Year == 2020) %>%
                   arrange(desc(Value)) %>%
                   slice_head(n = 12) %>%
                   pull(Country)
                 
                 # top 12 countries and the last 19 years
                 tmp_data_country_im_final <- tmp_data_country_im %>%
                   filter(Country %in% tmp_top_country_im_top12,
                          Year >= 2001,
                          Year <= 2020)%>%
                   arrange(Country, Year)
                 
                 tmp_data_country_im_plot <- tmp_data_country_im_final[c(1:20, 121:140, 221:240),]
                 

                 ### plot
                 output$ImMarketLine <- renderPlotly({
                   plot_ly(data = tmp_data_country_im_plot,
                           x = ~Year, 
                           hoverinfo = 'text',
                           hovertext = ~paste0("Year: ", Year, "<br>",
                                               "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                     config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                       list("zoomIn2d"), 
                                                                       list("zoomOut2d"), 
                                                                       list("resetScale2d"),
                                                                       list("hoverClosestCartesian"), 
                                                                       list("hoverCompareCartesian")
                     ))%>%
                     
                     add_trace(
                       y = ~Value,
                       line = list(width = 2),
                       color = ~as.factor(Country),
                       colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                  "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                       mode = "lines+markers",
                       marker = list(size = 6)
                     ) %>%
                     add_trace( data = tmp_data_country_im_final[c(21:120, 141:220),],
                                y = ~Value,
                                line = list(width = 2),
                                color = ~as.factor(Country),
                                colors = c("#17becf", "#2ca02c"),
                                mode = "lines+markers",
                                marker = list(size = 6),
                                visible = "legendonly"
                                
                     )%>%
                     
                     layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                            legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                            hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                            #title = "Dynamics of Total Imports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                            xaxis = list(title = "Year"),
                            yaxis = list(title = "Imports (bln. USD)")
                     )
                   
                 })

                     # 7.2.1 Key imports market trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     # convert
                     tmp_data_country_im$Value <- as.numeric(tmp_data_country_im$Value)
                     
                     # top 12 countries in 2021
                     tmp_data_imports_top12_value <- tmp_data_country_im %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     tmp_data_imports_top12_value <- sum(tmp_data_imports_top12_value$Value, na.rm = TRUE)

                     # top 12 countries and the last 20 years
                     tmp_data_country_im_final <- tmp_data_country_im %>%
                       filter(Country %in% tmp_top_country_im_top12,
                              Year >= 2001,
                              Year <= 2020)%>%
                       arrange(Country, Year)
                     
                     # import percentages for each country and year
                     tmp_data_country_im_pc <- tmp_data_country_im_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_imports_top12_value * 100) %>%
                       # round
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_im_pc_plot <- tmp_data_country_im_pc[c(1:20, 121:140, 221:240),]
                     
                     
                    ##plot
                     output$ImMarketLinePercent <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_im_pc_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", Percentage, " %"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Percentage,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_im_pc[c(21:120, 141:220),],
                                       y = ~Percentage,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Import Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Import Percentage, %")
                            )
                          
                        })

                     # 7.3 Key Two way trade market trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                    
                     data_sum <- df_countries_95_21_totals %>%
                       group_by(Year, Country) %>%
                       summarize(Total = sum(Value)) %>%
                       ungroup()
                     
                     # top 12 countries
                     data_top <- data_sum %>%
                       filter(Year == 2020) %>%
                       group_by(Country) %>%
                       summarize(Total = sum(Total)) %>%
                       top_n(12, Total) %>%
                       arrange(desc(Total))
                     
                     # Filter the data for the top 12 countries and the last 20 years
                     data_final <- data_sum %>%
                       filter(Year >= 2001, Year <= 2020, Country %in% data_top$Country)
                     
                     data_final <- data_final %>%
                       left_join(data_top, by = "Country") %>%
                       arrange(desc(Total.y))
                     
                     data_plot_TwowayMarketLine <- data_final[c(1:60),]
                     
                     
                     ### plot
                       output$TwowayMarketLine <- renderPlotly({
                         plot_ly(data = data_plot_TwowayMarketLine,
                                 x = ~Year, 
                                 hoverinfo = 'text',
                                 hovertext = ~paste0("Year: ", Year, "<br>",
                                                     "Turnover: ", round(Total.x/1000000,2), " bln.USD"))%>%
                           config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                             list("zoomIn2d"), 
                                                                             list("zoomOut2d"), 
                                                                             list("resetScale2d"),
                                                                             list("hoverClosestCartesian"), 
                                                                             list("hoverCompareCartesian")
                           ))%>%
                           
                           add_trace(
                             y = ~Total.x,
                             line = list(width = 2),
                             color = ~as.factor(Country),
                             colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                        "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                             mode = "lines+markers",
                             marker = list(size = 6)
                           ) %>%
                           add_trace( data = data_final[c(61:240),],
                                      y = ~Total.x,
                                      line = list(width = 2),
                                      color = ~as.factor(Country),
                                      colors = c("#17becf", "#2ca02c"),
                                      mode = "lines+markers",
                                      marker = list(size = 6),
                                      visible = "legendonly"
                                      
                           )%>%
                           
                           layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                  legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                  hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                  #title = "Dynamics of Trade Turnover for <br> Top 12 Trading Partners of Uzbekistan (2001-2021)",
                                  xaxis = list(title = "Year"),
                                  yaxis = list(title = "Total Trade Turnover (bln. USD)")
                           )
                         
                         })


                     # 7.4 Trade balance market trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1

                     df_DS <- df_countries_95_21_totals %>%
                       group_by(Country, Year) %>%
                       summarise(DeficitSurplus = sum(Value[Flow == "Exports"]) - sum(Value[Flow == "Imports"]))
                     
                     # Sort
                     df_DS <- df_DS[order(-df_DS$DeficitSurplus), ]
                     
                     # Extract 'DeficitSurplus'
                     df_2020 <- df_DS %>% 
                       filter(Year == 2020) %>% 
                       arrange(desc(DeficitSurplus))
                     
                     # Extract 6 countries from the very beginning of order and 6 from the end of order (total 12 countries)
                     df_top12 <- rbind(head(df_2020, 6), tail(df_2020, 6))
                     
                     df_top12_values <- df_DS %>% 
                       filter(Country %in% df_top12$Country, Year >= 2001)%>%
                       arrange(Country, Year)
                     
                     data_plot_BalanceMarketLine <- df_top12_values[c(61:240),]
 
                     
                     ##plot
                     output$BalanceMarketLine <- renderPlotly({
                       plot_ly(data = data_plot_BalanceMarketLine,
                               x = ~Year, 
                               hoverinfo = 'text',
                               hovertext = ~paste0("Year: ", Year, "<br>",
                                                   "Balance: ", round(DeficitSurplus/1000000,2), " bln.USD"))%>%
                         
                         
                         config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                           list("zoomIn2d"), 
                                                                           list("zoomOut2d"), 
                                                                           list("resetScale2d"),
                                                                           list("hoverClosestCartesian"), 
                                                                           list("hoverCompareCartesian")
                         )
                         )%>%
                         
                         add_trace(
                           y = ~DeficitSurplus,
                           line = list(width = 2),
                           color = ~as.factor(Country),
                           colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                      "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                           mode = "lines+markers",
                           marker = list(size = 6),
                           visible = "legendonly"
                         ) %>%
                         add_trace( data = df_top12_values[c(1:60),],
                                    y = ~DeficitSurplus,
                                    line = list(width = 2),
                                    color = ~as.factor(Country),
                                    colors = c("#17becf", "#2ca02c"),
                                    mode = "lines+markers",
                                    marker = list(size = 6)
                                    
                         )%>%
                         
                         
                         layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                legend = list(x = 0.1, y = 0.98, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                #title = "Top 12 trading partners with trade surplus/deficit (2001-2021)",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Trade Balance (mln. USD)")
                         )
                       
                       })

                     

                     #session$allowReconnect(TRUE)
                     # 7.6 Key exports market for goods trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1

                     # only the exports
                     tmp_data_country_ex_g <- filter(df_countries_95_21_goods, Flow == "Exports")
                     
                     # top 12 countries by value in 2021
                     tmp_data_country_ex_g_top12 <- tmp_data_country_ex_g %>%
                       filter(Year == 2021) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12) %>%
                       pull(Country)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_ex_g_final <- tmp_data_country_ex_g %>%
                       filter(Country %in% tmp_data_country_ex_g_top12,
                              Year >= 2001,
                              Year <= 2021)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_ex_g_plot <- tmp_data_country_ex_g_final[c(1:63),]
                     
                      

                     ### plot
                     output$ExGMarketLine <- renderPlotly({
                       plot_ly(data = tmp_data_country_ex_g_plot,
                               x = ~Year, 
                               hoverinfo = 'text',
                               hovertext = ~paste0("Year: ", Year, "<br>",
                                                   "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                         config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                           list("zoomIn2d"), 
                                                                           list("zoomOut2d"), 
                                                                           list("resetScale2d"),
                                                                           list("hoverClosestCartesian"), 
                                                                           list("hoverCompareCartesian")
                         ))%>%
                         
                         add_trace(
                           y = ~Value,
                           line = list(width = 2),
                           color = ~as.factor(Country),
                           colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                      "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                           mode = "lines+markers",
                           marker = list(size = 6)
                         ) %>%
                         add_trace( data = tmp_data_country_ex_g_final[c(64:252),],
                                    y = ~Value,
                                    line = list(width = 2),
                                    color = ~as.factor(Country),
                                    colors = c("#17becf", "#2ca02c"),
                                    mode = "lines+markers",
                                    marker = list(size = 6),
                                    visible = "legendonly"
                                    
                         )%>%
                         
                         layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                #title = "Dynamics of Exports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Exports (bln. USD)")
                         )
                       


                     })

                     # 7.6.1 Key exports market for goods trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     

                     # get the top 12
                     tmp_data_country_ex_g_top12_value <- tmp_data_country_ex_g %>%
                       filter(Year == 2021) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     # total export value
                     tmp_data_country_ex_g_top12_value <- sum(tmp_data_country_ex_g_top12_value$Value, na.rm = TRUE)

                     tmp_data_country_ex_g_pc_final <- tmp_data_country_ex_g_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_country_ex_g_top12_value * 100) %>%
                       # round the Percentage column to two decimal places
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_ex_g_pc_plot <- tmp_data_country_ex_g_pc_final[c(1:63),]


                     output$ExGMarketLinePercent <-
                        renderPlotly({
                          
                          plot_ly(data = tmp_data_country_ex_g_pc_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", Percentage, " %"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Percentage,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_ex_g_pc_final[c(64:252),],
                                       y = ~Percentage,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Export Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Export Percentage, %")
                            )
                          
                        })

                     # 7.7 Key exports market for services trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1

                     tmp_data_country_ex_s <- filter(df_countries_95_21_services, Flow == "Exports")
                     
                     # top 12 countries by value in 2021
                     tmp_data_country_ex_s_top12 <- tmp_data_country_ex_s %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12) %>%
                       pull(Country)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_ex_s_final <- tmp_data_country_ex_s %>%
                       filter(Country %in% tmp_data_country_ex_s_top12,
                              Year >= 2001,
                              Year <= 2020)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_ex_s_plot <- tmp_data_country_ex_s_final[c(21:40, 141:180),]
                     

                     ### plot
                     output$ExSMarketLine <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_ex_s_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Value,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_ex_s_final[c(1:20, 41:140, 180:240),],
                                       y = ~Value,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Dynamics of Exports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Exports (bln. USD)")
                            )
                          
                        })

                     # 7.7.1 Key exports market for services trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     # get the top 12
                     tmp_data_country_ex_s_top12_value <- tmp_data_country_ex_s %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     # total export value
                     tmp_data_country_ex_s_top12_value <- sum(tmp_data_country_ex_s_top12_value$Value, na.rm = TRUE)
                     
                     tmp_data_country_ex_g_pc_final <- tmp_data_country_ex_s_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_country_ex_s_top12_value * 100) %>%
                       # round the Percentage column to two decimal places
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_ex_g_pc_plot <- tmp_data_country_ex_g_pc_final[c(21:40,121:160),]
                     
                     ##plot
                     output$ExSMarketLinePercent <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_ex_g_pc_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", Percentage, " %"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Percentage,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_ex_g_pc_final[c(1:20, 41:120, 160:240),],
                                       y = ~Percentage,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Export Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Export Percentage, %")
                            )
                          
                        })

                     # 7.8 Key imports market for goods trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     
                     tmp_data_country_im_g <- filter(df_countries_95_21_goods, Flow == "Imports")
                     
                     # top 12 countries by value in 2021
                     tmp_data_country_im_g_top12 <- tmp_data_country_im_g %>%
                       filter(Year == 2021) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12) %>%
                       pull(Country)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_im_g_final <- tmp_data_country_im_g %>%
                       filter(Country %in% tmp_data_country_im_g_top12,
                              Year >= 2001,
                              Year <= 2021)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_im_g_plot <- tmp_data_country_im_g_final[c(1:63),]

                     ### plot
                     output$ImGMarketLine <- 
                       renderPlotly({
                       
                       
                       plot_ly(data = tmp_data_country_im_g_plot,
                               x = ~Year, 
                               hoverinfo = 'text',
                               hovertext = ~paste0("Year: ", Year, "<br>",
                                                   "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                         config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                           list("zoomIn2d"), 
                                                                           list("zoomOut2d"), 
                                                                           list("resetScale2d"),
                                                                           list("hoverClosestCartesian"), 
                                                                           list("hoverCompareCartesian")
                         ))%>%
                         
                         add_trace(
                           y = ~Value,
                           line = list(width = 2),
                           color = ~as.factor(Country),
                           colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                      "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                           mode = "lines+markers",
                           marker = list(size = 6)
                         ) %>%
                         add_trace( data = tmp_data_country_im_g_final[c(64:252),],
                                    y = ~Value,
                                    line = list(width = 2),
                                    color = ~as.factor(Country),
                                    colors = c("#17becf", "#2ca02c"),
                                    mode = "lines+markers",
                                    marker = list(size = 6),
                                    visible = "legendonly"
                                    
                         )%>%
                         
                         layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                #title = "Dynamics of Imports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                xaxis = list(title = "Year"),
                                yaxis = list(title = "Imports (bln. USD)")
                         )
                       
                     })

                     # 7.8.1 Key imports market for goods trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1
                     # convert
                     tmp_data_country_im_g$Value <- as.numeric(tmp_data_country_im_g$Value)
                     
                     # top 12 countries in 2021
                     tmp_data_country_im_g_top12_value <- tmp_data_country_im_g %>%
                       filter(Year == 2021) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     tmp_data_country_im_g_top12_value <- sum(tmp_data_country_im_g_top12_value$Value, na.rm = TRUE)
  
                     # import percentages for each country and year
                     tmp_data_country_im_g_pc_final <- tmp_data_country_im_g_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_country_im_g_top12_value * 100) %>%
                       # round
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_im_g_pc_plot <- tmp_data_country_im_g_pc_final[c(1:63),]

                     output$ImGMarketLinePercent <- 
                       renderPlotly({
                       plot_ly(data = tmp_data_country_im_g_pc_plot,
                               x = ~Year, 
                               hoverinfo = 'text',
                               hovertext = ~paste0("Year: ", Year, "<br>",
                                                   "Turnover: ", Percentage, " %"))%>%
                       config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                         list("zoomIn2d"), 
                                                                         list("zoomOut2d"), 
                                                                         list("resetScale2d"),
                                                                         list("hoverClosestCartesian"), 
                                                                         list("hoverCompareCartesian")
                       ))%>%
                       
                       add_trace(
                         y = ~Percentage,
                         line = list(width = 2),
                         color = ~as.factor(Country),
                         colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                    "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                         mode = "lines+markers",
                         marker = list(size = 6)
                       ) %>%
                       add_trace( data = tmp_data_country_im_g_pc_final[c(64:252),],
                                  y = ~Percentage,
                                  line = list(width = 2),
                                  color = ~as.factor(Country),
                                  colors = c("#17becf", "#2ca02c"),
                                  mode = "lines+markers",
                                  marker = list(size = 6),
                                  visible = "legendonly"
                                  
                       )%>%
                       
                       layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                              legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                              hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                              #title = "Import Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                              xaxis = list(title = "Year"),
                              yaxis = list(title = "Import Percentage, %")
                       )
                     
                        })

                     # 7.9 Key imports market for services trend line ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1

                     tmp_data_country_im_s <- filter(df_countries_95_21_services, Flow == "Imports")
                     
                     # top 12 countries by value in 2021
                     tmp_data_country_im_s_top12 <- tmp_data_country_im_s %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12) %>%
                       pull(Country)
                     
                     # top 12 countries and the last 20 years
                     tmp_data_country_im_s_final <- tmp_data_country_im_s %>%
                       filter(Country %in% tmp_data_country_im_s_top12,
                              Year >= 2001,
                              Year <= 2021)%>%
                       arrange(Country, Year)
                     
                     tmp_data_country_im_s_plot <- tmp_data_country_im_s_final[c(1:63),]


                     ### plot
                     output$ImSMarketLine <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_im_s_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", round(Value/1000000,2), " bln.USD"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Value,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_im_s_final[c(64:252),],
                                       y = ~Value,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Dynamics of Imports of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Imports (bln. USD)")
                            )
                          
                        })

                     # 7.9.1 Key imports market for services trend line Percent ------------------------------------
                     withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                        # Increment the progress bar, and update the detail text.
                        incProgress( i_prog/tot_step, detail = NULL)
                        ##Sys.sleep(0.1)

                     })
                     i_prog <- i_prog + 1

                     # convert
                     tmp_data_country_im_s$Value <- as.numeric(tmp_data_country_im_s$Value)
                     
                     # top 12 countries in 2021
                     tmp_data_country_im_s_top12_value <- tmp_data_country_im_s %>%
                       filter(Year == 2020) %>%
                       arrange(desc(Value)) %>%
                       slice_head(n = 12)
                     
                     tmp_data_country_im_s_top12_value <- sum(tmp_data_country_im_s_top12_value$Value, na.rm = TRUE)

                     
                     # import percentages for each country and year
                     tmp_data_country_im_s_pc_final <- tmp_data_country_im_s_final %>%
                       group_by(Country, Year) %>%
                       summarize(Percentage = sum(Value) / tmp_data_country_im_s_top12_value * 100) %>%
                       # round
                       mutate(Percentage = round(Percentage, 2))
                     
                     tmp_data_country_im_s_pc_plot <- tmp_data_country_im_s_pc_final[c(21:60, 101:120),]
                     
                     
                     ##plot   
                     output$ImSMarketLinePercent <-
                        renderPlotly({
                          plot_ly(data = tmp_data_country_im_s_pc_plot,
                                  x = ~Year, 
                                  hoverinfo = 'text',
                                  hovertext = ~paste0("Year: ", Year, "<br>",
                                                      "Turnover: ", Percentage, " %"))%>%
                            config(displaylogo = FALSE, modeBarButtons = list(list("toImage"), 
                                                                              list("zoomIn2d"), 
                                                                              list("zoomOut2d"), 
                                                                              list("resetScale2d"),
                                                                              list("hoverClosestCartesian"), 
                                                                              list("hoverCompareCartesian")
                            ))%>%
                            
                            add_trace(
                              y = ~Percentage,
                              line = list(width = 2),
                              color = ~as.factor(Country),
                              colors = c("#ff7f0e", "#d62728", "#9467bd", "#8c564b", "#bcbd22", 
                                         "#e377c2", "#7f7f7f", "#bcbd22", "#1f77b4", "#8e44ad"),
                              mode = "lines+markers",
                              marker = list(size = 6)
                            ) %>%
                            add_trace( data = tmp_data_country_im_s_pc_final[c(1:20, 61:100, 120:240),],
                                       y = ~Percentage,
                                       line = list(width = 2),
                                       color = ~as.factor(Country),
                                       colors = c("#17becf", "#2ca02c"),
                                       mode = "lines+markers",
                                       marker = list(size = 6),
                                       visible = "legendonly"
                                       
                            )%>%
                            
                            layout(plot, paper_bgcolor = "transparent", plot_bgcolor = "transparent", bgalpha = 0,
                                   legend = list(x = 0.1, y = 0.9, xanchor = "left", yanchor = "top", bgcolor = "rgba(255, 255, 255, 0)"), 
                                   hoverlabel=list(bgcolor='rgba(255,255,255,0.7)', font=list(color='black')),
                                   #title = "Import Percentage of Top 12 <br> Trading Partners of Uzbekistan (2001-2021)",
                                   xaxis = list(title = "Year"),
                                   yaxis = list(title = "Import Percentage, %")
                            )
                          
                        })


  }