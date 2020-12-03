packages = c("shiny", "here", "readxl","shinydashboard","dplyr","ggplot2","DT","tidyr","lubridate")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# check packages and install if necessary

setwd(here())

comp <-readxl::read_excel("fragrances.xlsx", sheet="samples")

fragrance_dataset <- comp %>%
  mutate(order_date = ymd(order_date),
         shipping_date = ymd(shipping_date),
         arrival_date = ymd(arrival_date),
         fb_cpml = round(fb_cpml,2)) %>%
  separate(house_fragrance,c("house","fragrance"), sep = "-",remove = F) %>%
  select(-house_fragrance)

comp <- comp %>%
  mutate(house_fragrance = as.factor(house_fragrance)) %>%
  separate(house_fragrance,c("house","fragrance"), sep = "-",remove = F)

comp_nd <- comp %>%
  distinct(fragrance,house, .keep_all = T)
# create separate dataset without duplicates

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title="fragrance dashboard"),
  dashboardSidebar(sidebarMenu(
                   menuItem("About this project", tabName = "about", selected = TRUE),
                   menuItem("Overview", tabName = "overview"),
                   menuItem("Ratings", tabName = "ratings"),
                   menuItem("Orders", tabName = "orders"),
                   menuItem("Delivery", tabName = "delivery"),
                   menuItem("Dataset", tabName = "dataset")
                   )),
  dashboardBody(tabItems(
    tabItem(tabName="about", fluidRow(
      column(width = 8,
             box(status="primary", width = NULL,
                 "Some time ago I started to order fragrance samples from a german webshop.",br(),
                 "I was looking for signature scent that I could wear. Little did I know that",br(),
                 "I'D be going down this rabbit hole for quite some time")
    ))),
    tabItem(tabName="overview",fluidRow(
            box(status="primary", width = 12,
                fluidRow(
                  infoBoxOutput("unique_sample_house"),
                  infoBoxOutput("unique_sample_frag"))
            )),br(),
            fluidRow(
              box(status="warning", width = 12,
                  fluidRow(infoBoxOutput("total_sample_cost"),
                    infoBoxOutput("total_sample_ml")),
                  fluidRow(infoBoxOutput("total_fullbottle_cost"),
                      infoBoxOutput("total_fullbottle_ml"))
                  ))
            ),
    tabItem(tabName = "ratings",
            fluidRow(
              column(width = 8,
                box(status="primary", width = NULL,
                    "How are all ratings distributed? (1 - hate it, 5 - love it)", br(),
                    plotOutput("rating_count_plot"),br(),
                    "How many distinct fragrances are distributed among the houses?", br(),
                    plotOutput("house_count"),br(),
                    "What is the average rating for each house with three or more fragrances?", br(),
                    plotOutput("house_ratings"), br(),
                    "Now let's see if the rating correlates with the average cost of full bottle. Do I have a preference for a certain price category?", br(),
                    plotOutput("fragrances_cost_rating_plot")
                )))),
    tabItem(tabName = "orders",
            fluidRow(column(width = 8,
                            box(status="primary", width = NULL,
                                "Do I sample more fragrance per order over time?", br(),
                                plotOutput("order_fragrances_plot"),br(),
                                "Do my orders get more expensive?",br(),
                                plotOutput("order_cost_plot"),br(),
                                "Does my average rating per order change?",br(),
                                plotOutput("orderd_ratings_plot")
                                )))),
    tabItem(tabName = "delivery",
            fluidRow(box(status = "primary", width =12,
                         "How many days do orders need to be processed and arrive?",br(),
                         plotOutput("delivery_plot")))),
    tabItem(tabName = "dataset",
            fluidRow(box(status = "primary",width=12,
                         DT::dataTableOutput("fragrance_dataset_tab"))))
    )))

# server ------------------------------------------------------------------


server <- function(input, output) { 
  
  ## render data table
  output$fragrance_dataset_tab <- renderDataTable({
    DT::datatable(fragrance_dataset, options = list(pageLength = 50))
  })
  
  ## unique houses
  unique_sample_house <- comp_nd %>%
    summarise(sum(n_distinct(comp_nd$house)))
  
  output$unique_sample_house <- renderInfoBox({
    infoBox(title="Unique Houses",value=unique_sample_house, icon = icon("home"),fill=TRUE, width = 4)
  })
  
  ## unique fragrances
  unique_sample_frag <- comp_nd %>%
    summarise(sum(n_distinct(house_fragrance)))
  
  output$unique_sample_frag <- renderInfoBox({
    infoBox(title="Unique Fragrances",value=unique_sample_frag, icon = icon("flask"),fill=TRUE, width = 4)
  })

  ## total sample cost
  total_sample_cost <- sum(comp$s_cost)
  output$total_sample_cost <- renderInfoBox({
    infoBox(title="Total Sample Cost",value=paste(total_sample_cost," €"), icon = icon("dollar-sign"),fill=TRUE, width = 4)
  })
  
  ## total sample ml
  total_sample_ml <- sum(comp$s_ml)
  
  output$total_sample_ml <- renderInfoBox({
    infoBox(title="Total Sample ml",value=paste0(total_sample_ml," ml"), icon = icon("bars"),fill=TRUE, width = 4)
  })  
  
  ## total cost for full bottles
  total_fullbottle_cost <- sum(comp_nd$fb_cost)
  
  output$total_fullbottle_cost <- renderInfoBox({
    infoBox(title="Total Full Bottle Cost",value=paste(
      formatC(total_fullbottle_cost, format="f", big.mark=".", digits=0)," €"),
      icon = icon("dollar-sign"),fill=TRUE, width = 4)
  })
  
  ## total ml full bottles
  total_fullbottle_ml <- sum(comp_nd$fb_ml)
  
  output$total_fullbottle_ml <- renderInfoBox({
    infoBox(title="Total Full Bottle ml",value=
        paste0(formatC(total_fullbottle_ml, format="f", big.mark = ".", digits=0)," ml"), icon = icon("bars"),fill=TRUE, width = 4)
  })
  
  ## rating histogram
  rating_count <- comp_nd %>%
    mutate(s_rating = as.factor(s_rating)) %>%
    group_by(s_rating) %>%
    filter(s_rating != "NA") %>%
    summarise(n = sum(s_amount))
  
  rating_count_plot <- ggplot(rating_count, aes(s_rating, n, label=n))+
    geom_col()+
    theme_classic()+
    geom_label()+
    labs(x="rating",y="n")
  
  output$rating_count_plot <- renderPlot({
    rating_count_plot
  })
  
  ## house_count
  house_count <- comp_nd %>%
    group_by(house) %>%
    summarise(n = sum(s_amount))
  
  house_count_plot <- ggplot(house_count, aes(reorder(house,n), n, label=n))+
    geom_bar(stat="identity")+
    coord_flip()+
    labs(x="house",y="distinct fragrances")+
    theme_classic()+
    geom_label(nudge_y = -0.33)
  
  output$house_count <- renderPlot({
    house_count_plot
  })
  
  ## house_ratings
  house_ratings <- comp_nd %>%
    group_by(house) %>%
    summarize(avg_rating = round(mean(as.double(s_rating), na.rm =T),2),
              n = sum(s_amount)) %>%
    filter(n >= 3)
  
  house_ratings_plot <- ggplot(house_ratings, aes(reorder(house, avg_rating),avg_rating, label=avg_rating))+
    geom_bar(stat="identity")+
    coord_flip()+
    labs(x="house",y="avg rating")+
    theme_classic()+
    geom_label(nudge_y = -0.25)+
    scale_y_continuous(limits =c(0,5))
  
  output$house_ratings <- renderPlot({
    house_ratings_plot
  })
  
  ## fragrances_cost_rating
  fragrances_cost_rating <- comp_nd %>%
    mutate(fb_cml = fb_cost / fb_ml,
           n = n()) %>%
    filter(s_rating != "NA") %>%
    group_by(s_cpml,s_rating) %>%
    summarise(n= n())
    
  
  fragrances_cost_rating_plot <- ggplot(fragrances_cost_rating, aes(s_cpml,s_rating,fill=n))+
    geom_tile()+
    scale_fill_gradient(low="#d8d8d8",high="#00ff00")+
    theme_classic()+
    labs(x="cost per ml",y="rating")
  
  output$fragrances_cost_rating_plot <- renderPlot({
    fragrances_cost_rating_plot
  })
  
  ## ratings per order_date
  orderd_ratings <- comp %>%
    mutate(order_date = as.Date(order_date),
           s_rating = as.double(s_rating)) %>%
    group_by(order_date) %>%
    summarise(avg_rating_order = round(mean(s_rating, na.rm=T),1))
  
  orderd_ratings_plot <- ggplot(orderd_ratings, aes(order_date, avg_rating_order, group=1, label=avg_rating_order))+
    geom_line()+
    scale_y_continuous(limits=c(0,5.5))+
    labs(x="order",y="average rating")+
    geom_point()+
    geom_label(nudge_y = 0.5)+
    theme_classic()+
    scale_x_date(date_breaks="3 month")
  
  output$orderd_ratings_plot <- renderPlot({
    orderd_ratings_plot
  })
  
  ## amount of fragrances by order date
  order_fragrances <- comp %>%
    group_by(order_date) %>%
    tally()
  
  order_fragrances_plot <- ggplot(order_fragrances, aes(order_date, n, label=n))+
    geom_line()+
    labs(x="order date", y="samples per order")+
    scale_y_continuous(limits = c(0,10))+
    geom_point()+
    geom_label(nudge_y = 0.5)+
    theme_classic()
  
  output$order_fragrances_plot <- renderPlot({
    order_fragrances_plot
  }) 
  
  ## cost by order date
  order_cost <- comp %>%
    group_by(order_date) %>%
    summarise(o_cost = sum(s_cost))
  
  order_cost_plot <- ggplot(order_cost, aes(order_date, o_cost, label=paste0(o_cost,"€")))+
    geom_line()+
    labs(x="order date", y="cost per order")+
    geom_point()+
    scale_y_continuous(limits = c(0,65))+
    geom_label(nudge_y = 5)+
    theme_classic()
  
  
  output$order_cost_plot <- renderPlot({
    order_cost_plot  })
  
  ## 
  
  delivery <- comp %>%
    group_by(order_id) %>%
    select(order_id, shipping_date, order_date, arrival_date) %>%
    distinct(order_id, shipping_date, order_date, arrival_date, .keep_all = T) %>%
    mutate(processing_time = as.numeric(shipping_date - order_date),
           shipping_time = as.numeric(arrival_date - shipping_date)) %>%
    select(order_id, shipping_time, processing_time) %>%
    gather(key = "key","time", -order_id) %>%
    filter(order_id >= 4)
  
  delivery_plot <- ggplot(delivery, aes(time, as.factor(order_id), fill = key, label=time))+
    geom_col(position = "dodge")+
    labs(x="days", y="order")+
    theme_classic()+
    geom_label(position = "dodge")
  
  output$delivery_plot <- renderPlot({
    delivery_plot  })
  
  
  }

shinyApp(ui, server)