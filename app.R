library (MASS)
library(ggplot2)
library(corrplot)
library(rpart)
library(tidyr)
library(dplyr)

library(shiny)

library(shiny)

library(shinyWidgets)

names = read.delim("Names.txt")
Countries_color = read.delim("Countries_color", sep = ",")

load("all_cricket.Rdata")

players.data = data.frame (names)

Give_Player_Total_Runs_in_Format <- function(player_name, which_format)
{
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )
    
    my_sum=sum(as.numeric(temp$Innings.Runs.Scored.Num), na.rm = TRUE)
    
    return(my_sum)
}

Give_Player_Total_Runs_in_Format_in_innings <- function(player_name, which_format, innings)
{
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )%>%arrange(Year)
    temp
    temp = temp %>% head(innings)
    temp
    
    my_sum = sum(as.numeric(temp$Innings.Runs.Scored.Num), na.rm = TRUE)
    
    
    return(my_sum)
}

Give_Bowler_Total_Wickets_in_Format_in_innings <- function(player_name, which_format, innings)
{
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )%>%arrange(Year)
    temp
    temp = temp %>% head(innings)
    temp
    
    my_sum = sum(as.numeric(temp$Innings.Wickets.Taken), na.rm = TRUE)
    
    
    return(my_sum)
}

Give_Player_Debut_year <- function(player_name, which_format)
{
    # player_name = "Babar Azam"
    #  which_format = "T20"
    #   
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )%>%arrange(desc(Year))
    my_year = temp %>% tail(1) %>% select(Year)
    # my_year = as.numeric() 
    return(my_year)
}

Give_No_of_Innings_in_first_5_years <- function(player_name, which_format)
{
    y1 = Give_Player_Debut_year(player_name,which_format )
    y1 = as.numeric(y1$Year)
    temp = all_cricket %>% filter(Player == player_name& Format == which_format )
    temp %>% filter(Year == y1+1 |Year == y1 +2 | Year == y1+3 | Year == y1+4 | Year == y1+5)
    innings = temp %>% count
    
    return(innings)
}

Give_Player_Average_in_Fromat_In_Innings<- function(player_name, which_format, innings)
{
    # player_name = "Babar Azam"
    # which_format = "T20"
    # innings =30
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )%>%arrange(Year)
    temp
    temp = temp %>% head(innings)
    temp
    
    average = mean(as.numeric(temp$Innings.Runs.Scored.Num), na.rm = TRUE)
    
    
    return(average)
}

Give_Bowler_Average_in_Fromat_In_Innings<- function(player_name, which_format, innings)
{
    # player_name = "Babar Azam"
    # which_format = "T20"
    # innings =30
    temp = all_cricket %>% filter(all_cricket$Player == player_name & Format == which_format )%>%arrange(Year)
    temp
    temp = temp %>% head(innings)
    temp
    
    average = mean(as.numeric(temp$Innings.Economy.Rate), na.rm = TRUE)
    
    
    return(average)
}

Compare_two_Batsmen_in_Format<- function(player_name_1, player_name_2, which_format)
{
    # player_name_1 = "Babar Azam"
    # player_name_2 = "V Kohli"
    # which_format = "T20"
    # innings =30
    in1 = Give_No_of_Innings_in_first_5_years(player_name_1,which_format)
    in2 = Give_No_of_Innings_in_first_5_years(player_name_2,which_format)
    
    min_innings = min(in1,in2)
    
    sum_1 = Give_Player_Total_Runs_in_Format_in_innings(player_name_1,which_format, min_innings)
    # sum_1
    sum_2 = Give_Player_Total_Runs_in_Format_in_innings(player_name_2,which_format, min_innings)
    # sum_2
    
    cat ("\n\nTotal Score of ", player_name_1, " in first ", min_innings , " innings of ",which_format ," is ", sum_1)
    cat ("\nAnd Total Score of ", player_name_2, " in first ", min_innings , " innings of ",which_format," is ", sum_2)
    
    avg_1 = Give_Player_Average_in_Fromat_In_Innings(player_name_1,which_format, min_innings)
    # avg_1
    avg_2 = Give_Player_Average_in_Fromat_In_Innings(player_name_2,which_format, min_innings)
    # avg_2
    
    cat ("\n\nAverage Score of ", player_name_1, " in first ", min_innings , " innings of ",which_format ," is ", avg_1)
    cat ("\nAnd Average Score of ", player_name_2, " in first ", min_innings , " innings of ",which_format," is ", avg_2)
    
    Name = c(player_name_1, player_name_2)
    Average = c(avg_1, avg_2)
    players.data <- data.frame(Name, Average)
    
    
    #   ggplot(players.data, aes(x = Name, y = Average)) +
    #       geom_bar(stat = "identity")
    
    # plot(players.data$Name, players.data$Average, xlab = "Batsmen", ylab = "Averages", title(player_name_1," Vs ",player_name_2))
    # 
    # plot(avg_1,avg_2)
    # 
    # ggplot(players.data,aes(x=players.data$Name,y=players.data$Average,colour=players.data$Average,group=colors(1))) +
    #   stat_summary(fun.data = "mean_cl_boot", geom = "smooth")
    #
    # p <- ggplot(players.data, aes(x=players.data$Name, y=players.data$Average  ))+geom_jitter(alpha=0.6)
    # p
    # p +facet_grid(. ~players.data$avg_1)
}

Compare_two_Bowlers_in_Format<- function(player_name_1, player_name_2, which_format)
{
    # player_name_1 = "Babar Azam"
    # player_name_2 = "V Kohli"
    # which_format = "T20"
    # innings =30
    in1 = Give_No_of_Innings_in_first_5_years(player_name_1,which_format)
    in2 = Give_No_of_Innings_in_first_5_years(player_name_2,which_format)
    
    min_innings = min(in1,in2)
    
    sum_1 = Give_Bowler_Total_Wickets_in_Format_in_innings(player_name_1,which_format, min_innings)
    # sum_1
    sum_2 = Give_Bowler_Total_Wickets_in_Format_in_innings(player_name_2,which_format, min_innings)
    # sum_2
    
    cat ("\n\nTotal Wickets of ", player_name_1, " in first ", min_innings , " innings of ",which_format ," is ", sum_1)
    cat ("\nAnd Total Wickets of ", player_name_2, " in first ", min_innings , " innings of ",which_format," is ", sum_2)
    
    avg_1 = Give_Bowler_Average_in_Fromat_In_Innings(player_name_1,which_format, min_innings)
    # avg_1
    avg_2 = Give_Bowler_Average_in_Fromat_In_Innings(player_name_2,which_format, min_innings)
    # avg_2
    
    cat ("\n\nAverage Economy rate of ", player_name_1, " in first ", min_innings , " innings of ",which_format ," is ", avg_1)
    cat ("\nAnd Average Economy rate of ", player_name_2, " in first ", min_innings , " innings of ",which_format," is ", avg_2)
    
    Bowler_Name = c(player_name_1, player_name_2)
    Bolwing_Average = c(avg_1, avg_2)
    players.data <- data.frame(Bowler_Name, Bolwing_Average)
    
    
    #   ggplot(players.data, aes(x = Bowler_Name, y = Bolwing_Average)) +
    #       geom_bar(stat = "identity")
    
    # plot(players.data$Name, players.data$Average, xlab = "Batsmen", ylab = "Averages", title(player_name_1," Vs ",player_name_2))
    # 
    # plot(avg_1,avg_2)
    # 
    # ggplot(players.data,aes(x=players.data$Name,y=players.data$Average,colour=players.data$Average,group=colors(1))) +
    #   stat_summary(fun.data = "mean_cl_boot", geom = "smooth")
    #
    # p <- ggplot(players.data, aes(x=players.data$Name, y=players.data$Average  ))+geom_jitter(alpha=0.6)
    # p
    # p +facet_grid(. ~players.data$avg_1)
}

Give_Colour_of_Player<- function(player_name)
{
  player_name = "Babar Azam"
  p_country = all_cricket %>% filter(player_name == Player) %>% select(Country) %>% tail(1) 
  p_color = Countries_color %>% filter(countries == p_country$Country) %>% select(my_colors)
  Player_color = p_color$my_colors
  
  return(Player_color)
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cric Insights"),
    
    setBackgroundImage(
      src = "Stadium.jpg"
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      
      
      
      # setBackgroundImage(
      #   src = "wood.jpg"
      # ),
      
        sidebarPanel(
            # tags$head(tags$style(
            #     HTML('
            #  #sidebar {
            #     background-color: #dec4de;
            # }
            # 
            # body, label, input, button, select { 
            #   font-family: "Arial";
            # }')
            #   )),
            
            selectInput("player_name_1","Select Player",
                        choices= names, selected = "Wasim Akram"),
            
            radioButtons("Radio_Format","Select Format",
                         choices= c("T20","ODI", "Test"), selected = "ODI"),
            
            radioButtons("Radio_span","Time Span",
                         choices= c("All","First 5 Years")),
            
            radioButtons("Radio_1","Select Stats Type",
                         choices= c("Individual","Compare"), selected = "Compare"),
            
            conditionalPanel(
                condition = "input.Radio_1  == 'Compare'",
                selectInput("player_name_2","Select Other Player",
                            choices= (names), selected = "JH Kallis" )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("view"),
            verbatimTextOutput("summary"),
            
            tabsetPanel( type = "tab" , id ='tabs',
                         tabPanel("BATTING",
                                  #plotOutput("distPlot"),
                                  column(12,h4(strong('Average')),plotOutput("My_plotin_average"))
                                  ,
                                  column(12,h4(strong('Total Runs')),plotOutput("My_plotin_runs")) ),
                         
                         tabPanel("BOWLING",
                                  column(12,h4(strong('Total Wickets')),plotOutput("My_plotin_wickets"))
                                  ,
                                  column(12,h4(strong('Economy Rate')),plotOutput("My_plotin_economy")) )
            )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) 
{
    # output$distPlot <- renderPlot({
    #     
    #     # if (input$tabs  == "BATTING")
    #     # {
    #     #     ggplot(players.data, aes(x = players.data$Player, y = players.data$Average_Runs)) +
    #     #         geom_bar(stat = "identity")
    #     #     
    #     #     
    #     #     # p <- ggplot(players.data, aes(x=players.data$Player, y=players.data$Average_Runs  ))+geom_jitter(alpha=0.6)
    #     #     # 
    #     #     # p +facet_grid(. ~players.data$Innings)
    #     # }
    #     # else if (input$tabs  == "BOWLING")
    #     # {
    #     #     ggplot(players.data, aes(x = players.data$Player, y = players.data$Wickets)) +
    #     #         geom_bar(stat = "identity")
    #     # }
    #     
    #     # generate bins based on input$bins from ui.R
    #     # x    <- faithful[, 2]
    #     # bins <- seq(min(x), max(x), length.out = input$binss + 1)
    #     # 
    #     # # draw the histogram with the specified number of bins
    #     # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    # Generate a summary of the dataset
    output$summary <- renderPrint({
        if(input$Radio_1  == "Individual")
        {
            if(input$tabs  == "BOWLING")
            {
                
                Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, "ODI",1000 )
                
            }
            else if (input$tabs  == "BATTING")
            {
                Give_Player_Total_Runs_in_Format(input$player_name_1, "ODI" )
            }
        }
    })
    
    # Show Table
    output$view <- renderTable({
        if(input$Radio_1  == "Individual")
        {
            Player = c(input$player_name_1)
       
            
            # country_name_1 = all_cricket %>% filter(Player == player_name_1) %>% select(Country) %>% tail(1)
            # Country = c(country_name_1)
           # Bolwing_Average = c(avg_1)
          #  players.data <- data.frame(Player, Bolwing_Average)
            
            if(input$Radio_span  == "All")
                innings = 1000
            else if(input$Radio_span  == "First 5 Years")
                    innings = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
            
            if(input$tabs  == "BOWLING")
            {
                
                wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1,input$Radio_Format,innings )
                avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
                
                Wickets = c(wickets_1)
                Bowling_Average_Economy = c(avg_1)
                
                players.data <<- data.frame(Player, Wickets, Bowling_Average_Economy)
                players.data
                
            }
            else if (input$tabs  == "BATTING")
            {
                runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
                avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
                
                Total_Runs = c(runs_1)
                Average_Runs = c(avg_1)
                Innings = c(innings)
                
                players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
                players.data
            }
        }
        else if (input$Radio_1  == "Compare")
        {
            Player = c(input$player_name_1, input$player_name_2)
            
            if(input$Radio_span  == "All")
                innings = 1000
            else if(input$Radio_span  == "First 5 Years")
            {
                innings_1 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
                innings_2 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_2, input$Radio_Format))
                
                innings = min(innings_1, innings_2)
            }
          
            if(input$tabs  == "BOWLING")
            {
                wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, input$Radio_Format,innings )
                wickets_2=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_2, input$Radio_Format,innings )
                
                avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
                avg_2=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings )
                
                Wickets = c(wickets_1, wickets_2)
                Bowling_Average_Economy = c(avg_1, avg_2)
                Innings = c(innings)
                
                players.data <<- data.frame(Player,Innings, Wickets, Bowling_Average_Economy)
                players.data

            }
            else if (input$tabs  == "BATTING")
            {
                runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
                runs_2 = Give_Player_Total_Runs_in_Format(input$player_name_2, input$Radio_Format )
                avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
                avg_2 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings)
                
                runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
                avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
                
                Total_Runs = c(runs_1, runs_2)
                Average_Runs = c(avg_1, avg_2)
                Innings = c(innings)
                
                players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
                players.data
                
            }
            
        }
    })
    
    output$My_plotin_runs <- renderPlot({  # ----------------------------------------------

      if(input$Radio_1  == "Individual")
      {
        Player = c(input$player_name_1)

        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
          innings = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))

        if(input$tabs  == "BOWLING")
        {

          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1,input$Radio_Format,innings )
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )

          Wickets = c(wickets_1)
          Bowling_Average_Economy = c(avg_1)

          players.data <<- data.frame(Player, Wickets, Bowling_Average_Economy)

          # players.data

        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)

          Total_Runs = c(runs_1)
          Average_Runs = c(avg_1)
          Innings = c(innings)

          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)

          # players.data
        }
      }
      else if (input$Radio_1  == "Compare")
      {
        Player = c(input$player_name_1, input$player_name_2)

        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
        {
          innings_1 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
          innings_2 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_2, input$Radio_Format))

          innings = min(innings_1, innings_2)
        }

        if(input$tabs  == "BOWLING")
        {
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, input$Radio_Format,innings )
          wickets_2=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_2, input$Radio_Format,innings )

          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          avg_2=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings )

          Wickets = c(wickets_1, wickets_2)
          Bowling_Average_Economy = c(avg_1, avg_2)
          Innings = c(innings)

          players.data <<- data.frame(Player,Innings, Wickets, Bowling_Average_Economy)
          # players.data

        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          runs_2 = Give_Player_Total_Runs_in_Format(input$player_name_2, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          avg_2 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings)

          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)

          Total_Runs = c(runs_1, runs_2)
          Average_Runs = c(avg_1, avg_2)
          Innings = c(innings)

          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)

          # players.data

        }

      } # ----------------------------------------------
               
      # 
      # ggplot(players.data, aes(x = Player, y = Total_Runs, fill=toString(Give_Colour_of_Player(Player)))) +geom_bar(stat ="identity",color="darkblue")
      
        ggplot(players.data, aes(x = Player, y = Total_Runs, fill=Total_Runs)) +geom_bar(stat ="identity",color="darkblue")

    })
    
    output$My_plotin_average <- renderPlot({

      if(input$Radio_1  == "Individual")
      {
        Player = c(input$player_name_1)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
          innings = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
        
        if(input$tabs  == "BOWLING")
        {
          
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1,input$Radio_Format,innings )
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          
          Wickets = c(wickets_1)
          Bowling_Average_Economy = c(avg_1)
          
          players.data <<- data.frame(Player, Wickets, Bowling_Average_Economy)
          
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1)
          Average_Runs = c(avg_1)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
        }
      }
      else if (input$Radio_1  == "Compare")
      {
        Player = c(input$player_name_1, input$player_name_2)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
        {
          innings_1 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
          innings_2 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_2, input$Radio_Format))
          
          innings = min(innings_1, innings_2)
        }
        
        if(input$tabs  == "BOWLING")
        {
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, input$Radio_Format,innings )
          wickets_2=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_2, input$Radio_Format,innings )
          
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          avg_2=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings )
          
          Wickets = c(wickets_1, wickets_2)
          Bowling_Average_Economy = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player,Innings, Wickets, Bowling_Average_Economy)
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          runs_2 = Give_Player_Total_Runs_in_Format(input$player_name_2, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          avg_2 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings)
          
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1, runs_2)
          Average_Runs = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
          
        }
        
      } # ----------------------------------------------

      # This:
      # ggplot(players.data, aes(x = Player, y = Average_Runs)) +geom_bar(stat = "identity")
      
      ggplot(players.data, aes(x = Player, y = Average_Runs, fill=Average_Runs)) +geom_bar(stat ="identity",color="darkblue")
      
      
      # ggplot(players.data, aes(x=Player,y=Average_Runs, size = Average_Runs, color = Player)) + geom_point()
        

    })
    
    
    output$My_plotin_wickets <- renderPlot({
    
      if(input$Radio_1  == "Individual")
      {
        Player = c(input$player_name_1)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
          innings = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
        
        if(input$tabs  == "BOWLING")
        {
          
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1,input$Radio_Format,innings )
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          
          Wickets = c(wickets_1)
          Bowling_Average_Economy = c(avg_1)
          
          players.data <<- data.frame(Player, Wickets, Bowling_Average_Economy)
          
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1)
          Average_Runs = c(avg_1)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
        }
      }
      else if (input$Radio_1  == "Compare")
      {
        Player = c(input$player_name_1, input$player_name_2)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
        {
          innings_1 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
          innings_2 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_2, input$Radio_Format))
          
          innings = min(innings_1, innings_2)
        }
        
        if(input$tabs  == "BOWLING")
        {
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, input$Radio_Format,innings )
          wickets_2=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_2, input$Radio_Format,innings )
          
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          avg_2=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings )
          
          Wickets = c(wickets_1, wickets_2)
          Bowling_Average_Economy = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player,Innings, Wickets, Bowling_Average_Economy)
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          runs_2 = Give_Player_Total_Runs_in_Format(input$player_name_2, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          avg_2 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings)
          
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1, runs_2)
          Average_Runs = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
          
        }
        
      } # ----------------------------------------------

      # This:
      ggplot(players.data, aes(x = Player, y = Wickets, fill=Wickets)) +geom_bar(stat ="identity",color="darkblue")
      
        # ggplot(players.data, aes(x = Player, y = Wickets)) +geom_bar(stat = "identity")
        

    })
 
    output$My_plotin_economy <- renderPlot({

      if(input$Radio_1  == "Individual")
      {
        Player = c(input$player_name_1)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
          innings = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
        
        if(input$tabs  == "BOWLING")
        {
          
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1,input$Radio_Format,innings )
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          
          Wickets = c(wickets_1)
          Bowling_Average_Economy = c(avg_1)
          
          players.data <<- data.frame(Player, Wickets, Bowling_Average_Economy)
          
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1)
          Average_Runs = c(avg_1)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
        }
      }
      else if (input$Radio_1  == "Compare")
      {
        Player = c(input$player_name_1, input$player_name_2)
        
        if(input$Radio_span  == "All")
          innings = 1000
        else if(input$Radio_span  == "First 5 Years")
        {
          innings_1 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_1, input$Radio_Format))
          innings_2 = as.numeric(Give_No_of_Innings_in_first_5_years(input$player_name_2, input$Radio_Format))
          
          innings = min(innings_1, innings_2)
        }
        
        if(input$tabs  == "BOWLING")
        {
          wickets_1=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_1, input$Radio_Format,innings )
          wickets_2=Give_Bowler_Total_Wickets_in_Format_in_innings(input$player_name_2, input$Radio_Format,innings )
          
          avg_1=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings )
          avg_2=Give_Bowler_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings )
          
          Wickets = c(wickets_1, wickets_2)
          Bowling_Average_Economy = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player,Innings, Wickets, Bowling_Average_Economy)
          # players.data
          
        }
        else if (input$tabs  == "BATTING")
        {
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          runs_2 = Give_Player_Total_Runs_in_Format(input$player_name_2, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          avg_2 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_2, input$Radio_Format,innings)
          
          runs_1 = Give_Player_Total_Runs_in_Format(input$player_name_1, input$Radio_Format )
          avg_1 = Give_Player_Average_in_Fromat_In_Innings(input$player_name_1, input$Radio_Format,innings)
          
          Total_Runs = c(runs_1, runs_2)
          Average_Runs = c(avg_1, avg_2)
          Innings = c(innings)
          
          players.data <<- data.frame(Player, Innings, Total_Runs, Average_Runs)
          
          # players.data
          
        }
        
      } # ----------------------------------------------
      
      
      # This:
      ggplot(players.data, aes(x = Player, y = Bowling_Average_Economy, fill=Bowling_Average_Economy)) +geom_bar(stat ="identity",color="darkblue")
      
      # ggplot(players.data, aes(x = Player, y = Wickets)) +geom_bar(stat = "identity")
        # ggplot(players.data, aes(x=Player,y=Wickets, size = Bowling_Average_Economy, color = Player)) + geom_point()
      
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)


