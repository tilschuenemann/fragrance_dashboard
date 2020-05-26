packages = c("shiny", "shinydashboard","dplyr","ggplot2","DT","tidyr","lubridate")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

comp <- readxl::read_excel(choose.files(multi=F,caption="Select fragrances.xlsx"),
                              sheet ="samples")

fragrance_dataset <- comp %>%
  mutate(order_date = ymd(order_date),
         shipping_date = ymd(shipping_date),
         arrival_date = ymd(arrival_date),
         fb_cpml = round(fb_cpml,2)) %>%
  separate(house_fragrance,c("house","fragrance"), sep = "-",remove = F) %>%
  select(-house_fragrance)


str(fragrance_dataset)

comp <- comp %>%
  mutate(house_fragrance = as.factor(house_fragrance)) %>%
  separate(house_fragrance,c("house","fragrance"), sep = "-",remove = F)

comp_nd <- comp %>%
  distinct(fragrance,house, .keep_all = T)

total_sample_cost <- sum(comp$s_cost)
total_sample_ml <- sum(comp$s_ml)

unique_sample_frag <- comp_nd %>%
  summarise(sum(n_distinct(house_fragrance)))

unique_sample_house <- comp_nd %>%
  
  summarise(sum(n_distinct(comp_nd$house)))

total_fullbottle_cost <- sum(comp_nd$fb_cost)
total_fullbottle_ml <- sum(comp_nd$fb_ml)

hist(comp$s_cpml)
hist(comp$fb_cpml)

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


## house_count
house_count <- comp_nd %>%
  group_by(house) %>%
  summarise(n = sum(s_amount))

house_count_plot <- 
  
ggplot(house_count, aes(reorder(house,n), n, label=n))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="house",y="distinct fragrances")+
  theme_classic()+
  geom_label(nudge_y = -0.33)
    
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

## liked scents per house
house_45 <- comp_nd %>%
  filter(s_rating >= 4) %>%
  group_by(house) %>%
  summarize(avg_rating = round(mean(as.double(s_rating), na.rm =T),2),
            n = sum(s_amount)) %>%
  filter(avg_rating != is.na(avg_rating))

house_45_plot <- ggplot(house_45, aes(reorder(house, avg_rating),avg_rating, label=avg_rating))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="house",y="avg rating")+
  theme_classic()+
  geom_label(nudge_y = -0.25)

## fragrances_cost_rating
fragrances_cost_rating <- comp_nd %>%
  mutate(fb_cml = fb_cost / fb_ml) %>%
  filter(s_rating != "NA")

fragrances_cost_rating_plot <- ggplot(fragrances_cost_rating, aes(as.factor(s_rating),fb_cml))+
  labs(x="rating",y="avg cost per ml of full bottle")+
  theme_classic()+
  theme(legend.position = "none")+
  geom_jitter(position = position_jitter(0.15), aes(colour = s_rating, alpha = 0.5,size = 1))

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

## cost by order date
order_cost <- comp %>%
  group_by(order_date) %>%
  summarise(o_cost = sum(s_cost))

order_cost_plot <- ggplot(order_cost, aes(order_date, o_cost, label=o_cost))+
  geom_line()+
  labs(x="order date", y="cost per order")+
  geom_point()+
  scale_y_continuous(limits = c(0,65))+
  geom_label(nudge_y = 5)+
  theme_classic()

# actual app

ui <- dashboardPage(
  dashboardHeader(title="fragrance dashboard"),
  dashboardSidebar(sidebarMenu(
                   menuItem("Dataset", tabName = "dataset"),
                   menuItem("Overview", tabName = "overview"),
                   menuItem("Ratings", tabName = "ratings"),
                   menuItem("Orders", tabName = "orders"),
                   menuItem("Delivery", tabName = "delivery")
                   )),
  dashboardBody(tabItems(
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
                    "How are all ratings distributed?", br(),
                    plotOutput("rating_count_plot"),br(),
                    "How are the amount of distinct fragrances distributed among the houses?", br(),
                    plotOutput("house_count"),br(),
                    "There are a lot of house with just one fragrance. In the next graph only houses with three or more fragrance ratings are kept. What is their average rating?", br(),
                    plotOutput("house_ratings"), br(),
                    "We can also look for the average rating per house if we filter down to fragrances that were liked and recieved a rating of 4 or 5.",br(),
                    "Single fragrances are now included - this introduces some bias and changes the graph for a bit:",br(),
                    plotOutput("house_45"), br(),
                    "Now let's see if the rating correlates with the average cost of full bottle. Do I have a preference for a certain price category?", br(),
                    plotOutput("fragrances_cost_rating_plot")
                )
                )
            )
            
            ),
    tabItem(tabName = "orders",
            fluidRow(column(width = 8,
                            box(status="primary", width = NULL,
                                "Do I sample more fragrance per order over time?", br(),
                                plotOutput("order_fragrances_plot"),br(),
                                "Do my orders change in price per order?",br(),
                                plotOutput("order_cost_plot"),br(),
                                "Does my average rating per order change?",br(),
                                plotOutput("orderd_ratings_plot")
                                )))),
    tabItem(tabName = "dataset",
            fluidRow(box(status = "primary",width=12,
                         DT::dataTableOutput("fragrance_dataset_tab")))),
    tabItem(tabName = "delivery",
            fluidRow(box(status = "primary", width =12,
                         "How many days do orders need to be processed and arrive?",br())))
    )))

## server

server <- function(input, output) { 
  output$total_sample_ml <- renderInfoBox({
    infoBox(title="Total Sample ml",value=total_sample_ml, icon = icon("bars"),fill=TRUE, width = 4)
  })
  
  output$total_sample_cost <- renderInfoBox({
    infoBox(title="Total Sample Cost",value=paste(total_sample_cost," €"), icon = icon("dollar-sign"),fill=TRUE, width = 4)
  })
  output$unique_sample_frag <- renderInfoBox({
    infoBox(title="Unique Fragrances",value=unique_sample_frag, icon = icon("flask"),fill=TRUE, width = 4)
  })
  output$unique_sample_house <- renderInfoBox({
    infoBox(title="Unique Houses",value=unique_sample_house, icon = icon("home"),fill=TRUE, width = 4)
  })
  output$total_fullbottle_cost <- renderInfoBox({
    infoBox(title="Total Full Bottle Cost",value=paste(total_fullbottle_cost," €"), icon = icon("dollar-sign"),fill=TRUE, width = 4)
  })
  
  output$total_fullbottle_ml <- renderInfoBox({
    infoBox(title="Total Full Bottle ml",value=total_fullbottle_ml, icon = icon("bars"),fill=TRUE, width = 4)
  })
  output$fragrance_dataset_tab <- renderDataTable({
    DT::datatable(fragrance_dataset, options = list(pageLength = 50))
  })
  output$house_ratings <- renderPlot({
    house_ratings_plot
  })
  output$house_count <- renderPlot({
    house_count_plot
  })
  output$house_45 <- renderPlot({
    house_45_plot
  })
  output$fragrances_cost_rating_plot <- renderPlot({
    fragrances_cost_rating_plot
  })
  output$rating_count_plot <- renderPlot({
    rating_count_plot
  })
  
  output$orderd_ratings_plot <- renderPlot({
    orderd_ratings_plot
  })
  output$order_fragrances_plot <- renderPlot({
    order_fragrances_plot
  }) 
  output$order_cost_plot <- renderPlot({
    order_cost_plot
  })
  }

shinyApp(ui, server)