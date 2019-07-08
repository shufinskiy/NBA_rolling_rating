##The function is to build animated graphics
rolling_def_rating_nba <- function(table, name, variable = DRTG, col1 = col1, col2 = col2){

##Return the entered value in the function argument in the type quosure 
  quo_rating <- enquo(variable)
  quo_col1 <- enquo(col1)
  quo_col2 <- enquo(col2)
  
##Changing the data type of multiple columns  
  test1 <- table %>%
    mutate(GAME_DATE = as.Date(ymd_hms(GAME_DATE))) %>%
    mutate_at(vars(ORTG:NRTG), list(~as.numeric)) %>%
    arrange(GAME_DATE)
  
##The calculation of the moving average    
  team <- test1 %>%
    filter(TEAM_ABBREVIATION == name) %>%
    mutate(RATING = rollmeanr(!! quo_rating, k = 10, fill= NA)) %>%
    na.omit(test1)
  
##Create a table with the average ratings of each team 
  league <- test1 %>%
    group_by(TEAM_NAME) %>%
    summarise(ORTG = mean(ORTG),
              DRTG = mean(DRTG),
              NTRG = mean(NRTG))
  
##The average, 10 and 21 ratings in the entire League.  
  average <- league %>%
    mutate(average = mean(!! quo_rating)) %>%
    select(average) %>%
    unique() %>%
    .$average
  
  
  top10 <- league %>%
    arrange(desc(!! quo_rating)) %>%
    select(!! quo_rating) %>%
    slice(21)
  top10 <- top10[[1]]
  
  bottom10 <- league %>%
    arrange(desc(!! quo_rating)) %>%
    select(!! quo_rating) %>%
    slice(10)
  bottom10 <- bottom10[[1]]

##Getting the date of the first rollaverage   
  data <- team %>%
    select(GAME_DATE) %>%
    arrange(GAME_DATE)
  data <- data[[1,1]]

##Getting color and color_name selected color
  table_color <- read_delim("F:/NBA_20191705/Excel/Team_color.csv", delim = ";", col_names = TRUE)
  
  color1 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(!! quo_col1)
  color1 <- color1[[1]]
  
  color2 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(!! quo_col2)
  color2 <- color2[[1]]
  
  name1 <- paste0("name_", quo_name(quo_col1))
  name2 <- paste0("name_", quo_name(quo_col2))
  
  name_color1 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(name1)
  name_color1 <- name_color1[[1]]
  
  name_color2 <- table_color %>%
    filter(TEAM_ABBREVIATION == name) %>%
    select(name2)
  name_color2 <- name_color2[[1]]
  
##The maximum value of the rating  
  max <- team %>%
    filter(RATING == max(RATING)) %>%
    select(RATING)
  max <- max[[1]]

##Building and save a static chart
  Sys.setlocale("LC_ALL", "C")
  gg <- ggplot(team, aes(GAME_DATE, RATING)) +
    geom_hline(yintercept = c(top10, bottom10), col = c("red", "blue")) +
    annotate(geom = "text", x = as.Date(data) + 2, y = top10 - 0.2,
             label = "TOP 10", col = "red") +
    annotate(geom = "text", x = as.Date(data) + 2, y = bottom10 + 0.2,
             label = "BOTTOM 10", col = "blue") +
    geom_line(size = 2, col = if_else(team$RATING < average, color1, color2)) +
    theme_tufte() +
    labs(title = paste0(team$TEAM_NAME, " 10-Game Rolling ", quo_name(quo_rating)),
         subtitle = paste0(paste0(name_color1, " - above average ", quo_name(quo_rating)),
                           "\n", paste0(name_color2, " - below average ",quo_name(quo_rating))),
         caption = "Source: BBall Index Data & Tools\nTelegram: @NBAatlantic, twitter: @vshufinskiy")
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
  ggsave(paste0(unique(team$TEAM_NAME), quo_name(quo_rating), ".jpeg"), gg, width = 8, units = "in")
 
##Building animations
  anim <- gg +
    theme(plot.title = element_text(hjust = 0.5, size = 25),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 15),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 18)) +
    geom_text(aes(x = as.Date(data), y = max + 0.5),
              label = paste0(quo_name(quo_rating)," ", round(team$RATING, digits = 1)), size = 6,
              col = if_else(team$RATING < average, color1, color2)) +
    transition_reveal(GAME_DATE) +
    labs(title = paste0(team$TEAM_NAME, " 10-Game Rolling ", quo_name(quo_rating)),
         subtitle = paste0(paste0(name_color1, " - above average ",quo_name(quo_rating)),
                           "\n", paste0(name_color2, " - below average ",quo_name(quo_rating)),
                           "\n", "Date: {frame_along}"),
         caption = paste0("Source: stats.nba.com\nTelegram: @NBAatlantic, twitter: @vshufinskiy"))

##Save results 
  animate(anim, fps = 30, duration = 15, width = 1280, height = 720,
          renderer = gifski_renderer(paste0(unique(team$TEAM_ABBREVIATION), "_", quo_name(quo_rating), ".gif")))
}
