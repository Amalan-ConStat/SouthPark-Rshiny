# loading packages ----    
library(shinydashboard)
library(shinycssloaders)

library(southparkr)
library(extrafont)
#loadfonts()
library(tidyverse)
library(tidytext)
library(tibble)

library(RColorBrewer)
#library(gganimate)
library(highcharter)
library(kableExtra)
library(plotly)

# Load the data ----
load("Data/southparkData.RData")

# adding 
CharacterList<-Updatedseason %>%
               count(Char=as.factor(Character),sort = TRUE) %>%
               head(1000) %>%
               select(Char)
# Themes for plots ----

# Bar plot theme 1-----
theme_southpark_bp<-theme(
                          plot.background=element_rect(fill="#f5f788"),
                          panel.background=element_rect(fill="#f5f788"),
  
                          panel.grid.minor=element_blank(),
                          panel.grid.major.y=element_blank(),
                          panel.grid.major.x=element_line(),
  
                          axis.ticks=element_blank(),
  
                          panel.border=element_rect(fill=NA,color="white"),
  
                          plot.title = element_text(family = "Constantia",size = "13",
                                                    face = "bold",color ="#ce0e1e" ),
                          axis.text.x = element_text(family = "Constantia",size = "9",
                                                    face = "bold",color ="#ff0000" ),
                          axis.text.y = element_text(family = "Constantia",size = "9",
                                                    face = "bold",color ="#000000" ),
                          axis.title = element_text(family = "Constantia",size = "11",
                                                    face = "bold",color ="#0636a5" ),
                          plot.subtitle = element_text(family = "Constantia",size = "11.5",
                                                       face = "bold",color ="#e8304f")
                          )

# Bar plot theme 2-----
theme_southpark_bp2<-theme(
                           plot.background=element_rect(fill="#f5f788"),
                           panel.background=element_rect(fill="#f5f788"),
  
                           panel.grid.minor=element_blank(),
                           panel.grid.major.x=element_blank(),
                           panel.grid.major.y=element_line(),
  
                           axis.ticks=element_blank(),
  
                           panel.border=element_rect(fill=NA,color="white"),
  
                           plot.title = element_text(family = "Constantia",size = "13",
                                                     face = "bold",color ="#ce0e1e" ),
                           axis.text.y = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#ff0000" ),
                           axis.text.x = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#000000" ),
                           axis.title = element_text(family = "Constantia",size = "11",
                                                     face = "bold",color ="#0636a5" ),
                           plot.subtitle = element_text(family = "Constantia",size = "11.5",
                                                        face = "bold",color ="#e8304f")
                            )

# Bar plot theme 3-----
theme_southpark_bp3<-theme(
                           plot.background=element_rect(fill="#f5f788"),
                           panel.background=element_rect(fill="#f5f788"),
  
                           panel.grid.minor=element_blank(),
                           panel.grid.major.x=element_blank(),
                           panel.grid.major.y=element_line(),
  
                           axis.ticks=element_blank(),
  
                           panel.border=element_rect(fill=NA,color="white"),
  
                           plot.title = element_text(family = "Constantia",size = "13",
                                                     face = "bold",color ="#ce0e1e" ),
                           axis.text.y = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#ff0000" ),
                           axis.text.x = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#000000" ),
                           axis.title = element_text(family = "Constantia",size = "11",
                                                     face = "bold",color ="#0636a5" ),
                           plot.subtitle = element_text(family = "Constantia",size = "11.5",
                                                        face = "bold",color ="#e8304f"),
                           
                           strip.text = element_text(family = "Constantia",size = "10.5",
                                                     face = "bold",color = "#ff0000"),
                           strip.background = element_rect(color = "white",fill ="#0636a5" )
                           )

# Bar plot theme 4-----
theme_southpark_bp4<-theme(
                           plot.background=element_rect(fill="#f5f788"),
                           panel.background=element_rect(fill="#f5f788"),
  
                           panel.grid.minor=element_blank(),
                           panel.grid.major.x=element_blank(),
                           panel.grid.major.y=element_line(),
  
                           axis.ticks=element_blank(),
  
                           panel.border=element_rect(fill=NA,color="white"),
  
                           plot.title = element_text(family = "Constantia",size = "13",
                                                      face = "bold",color ="#ce0e1e" ),
                           axis.text.y = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#ff0000" ),
                           axis.text.x = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#000000" ),
                           axis.title = element_text(family = "Constantia",size = "11",
                                                      face = "bold",color ="#0636a5" ),
                           plot.subtitle = element_text(family = "Constantia",size = "11.5",
                                                        face = "bold",color ="#e8304f"),
  
                           strip.text = element_text(family = "Constantia",size = "10.5",
                                                      face = "bold",color = "#ff0000"),
                           strip.background = element_rect(color = "white",fill ="#0636a5" ),
                           
                           legend.background = element_rect(fill="#f7a5bb",color="#ff0000")
                           )

# scatter plot theme  -----
theme_southpark_sp<-theme(
                           plot.background=element_rect(fill="#f5f788"),
                           panel.background=element_rect(fill="#f5f788"),
  
                           panel.grid.minor=element_blank(),
                           panel.grid.major.x=element_line(color="#ff0000"),
                           panel.grid.major.y=element_line(color="#ff0000"),
  
                           axis.ticks=element_blank(),
  
                           panel.border=element_rect(fill=NA,color="#ff0000"),
  
                           plot.title = element_text(family = "Constantia",size = "13",
                                                     face = "bold",color ="#ce0e1e" ),
                           axis.text.y = element_text(family = "Constantia",size = "9",
                                                      face = "bold",color ="#ff0000" ),
                           axis.text.x = element_text(family = "Constantia",size = "9",
                                                       face = "bold",color ="#ff0000" ),
                           axis.title = element_text(family = "Constantia",size = "11",
                                                     face = "bold",color ="#0636a5" ),
                           plot.subtitle = element_text(family = "Constantia",size = "11.5",
                                                        face = "bold",color ="#e8304f"),
  
                           strip.text = element_text(family = "Constantia",size = "10.5",
                                                      face = "bold",color = "#ff0000"),
                           strip.background = element_rect(color = "white",fill ="#0636a5" ),
  
                           legend.background = element_rect(fill="#f7a5bb",color="#ff0000")
                          )


# Define UI for application ----
dashboardPage(
  dashboardHeader(
    # Add title for the shiy app -------
                  title="South Park  Text Analytics",
                  titleWidth = 305
                   
                ),
    # build the sidebar for shiny app ------
  dashboardSidebar(
                  width=305,
                  sidebarMenu( # sidebar menu created -----------
                              menuItem("Trivia on Southpark",
                                       tabName = "trivia",icon = icon("grin",lib = "font-awesome"),
                                                menuSubItem("Trivia", tabName = "trivia"),
                                                menuSubItem("Lines", tabName = "line"),
                                                menuSubItem("Words", tabName = "words"),
                                                menuSubItem("Special Words",tabName="swords"),
                                                menuSubItem("Rating and Votes",tabName = "rate_vote"),
                                                menuSubItem("Sentiment Analysis",tabName = "sentiment_ana"),
                                                menuSubItem("Bigram and Trigram Analysis", tabName = "ngram")
                                       ),
                              menuItem("Compare Two Seasons",
                                       tabName = "seasons",icon = icon("grin-squint-tears",lib = "font-awesome")),
                              menuItem("Compare Two Characters",
                                       tabName = "characters",icon = icon("grin-squint",lib = "font-awesome")),
                              menuItem("Compare Two Characters but Same Season",
                                       tabName = "character_by_season",icon = icon("grin-tongue",lib = "font-awesome")),
                              menuItem("Compare Two Seasons but Same Character",
                                       tabName = "seasons_by_character",icon = icon("grin-tongue-wink",lib = "font-awesome")),
                              menuItem("About the Author", icon=icon("user-astronaut",lib = "font-awesome"),
                                       href = "https://amalan-con-stat.netlify.com/"
                                       )
                             ),
                  br(),
                  HTML('<center><img src="meme0.gif" width= "265"></center>'),
                  br(),
                  HTML('<center><img src="meme000.gif" width= "265"></center>'),                  
                  br(),
                  HTML('<center><img src="meme0000.gif" width= "265"></center>'),
                  br(),
                  HTML('<center><img src="shinybusy.gif" width= "265"></center>')
                  ),
  dashboardBody(# choosing color for the header by html tag------
                tags$head(tags$style(HTML('
                .skin-blue .main-header .logo {
                background-color: #bd022c !important;
                color: #000000;
                }
                .skin-blue .main-header .logo:hover {
                background-color: #bd022c !important;
                }     
                .skin-blue .main-header .navbar-static-top {
                background-color: #bd022c;
                }
                .skin-blue .main-sidebar .navbar {
                background-color:  #bd022c;
                }
                .skin-blue .main-sidebar {
                background-color:  #bd022c;
                }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                 background-color: #e26c87;
                  }
                                          
                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                background-color: #fbf450;
                color: #000000;
                }
                                          
                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                background-color: #215cdb;
                }
                
                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                background-color: #215cdb;
                }

                .content-wrapper, .right-side {
                background-color: #afd4ff;
                }

                .main-header .logo {
                 font-family: "Garamond";
                 font-weight: bold;
                 font-size: 24px;
                }
                h2 {
                font-family: "Garamond";
                font-weight: bold;
                font-size: 24px;   
                color: #215cdb;
                }
                h3 {
                font-family: "Garamond";
                font-weight: bold;
                font-size: 22px;   
                color: #215cdb;
                }
              ')
                                    )
                        ),
                tabItems( # adding content to trivia tab -----
                        tabItem(tabName = "trivia",
                        h2("Introduction"),
                        fluidRow(
                                box(width=4,
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22px; 
                                    text-align: justify;
                                    color: #bd022c",
                                    "This Rshiny app is about text analytics for the awesome Tv show South Park.",
                                    br(),
                                    br(),
                                    "I will do regular updates when there is a new season.
                                    Until now this app includes from season 1 to 22.",
                                    br(),
                                    br(),
                                    "Text analytics include regarding words, Swear words, Lines,
                                    stop words, sentiment analysis (AFINN,bing,nrc) for seasons
                                    and characters",
                                    br(),
                                    br(),
                                    "Go through the sidebar to check what kind information and analytics
                                    that could be related to South Park."
                                    ),
                                box(width=4,
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22px; 
                                    text-align: justify;
                                    color: #bd022c",
                                    h3("Packages Used"),
                                    tags$a(href="https://github.com/rstudio/shiny","shiny"),"-",
                                    tags$a(href="https://github.com/rstudio/shinydashboard","shinydashboard"),"-",
                                    tags$a(href="https://github.com/andrewsali/shinycssloaders","shinycssloaders"),
                                    br(),
                                    br(),
                                    tags$a(href="https://github.com/pdrhlik/southparkr","southparkr"),"-"
                                    tags$a(href="https://github.com/tidyverse/tibble","tibble"),
                                    br(),
                                    br(),
                                    tags$a(href="https://github.com/tidyverse/tidyverse","tidyverse"),"-",
                                    tags$a(href="https://github.com/juliasilge/tidytext","tidytext"),"-",
                                    tags$a(href="https://github.com/wch/extrafont","extrafont"),
                                    br(),
                                    br(),
                                    tags$a(href="https://github.com/thomasp85/gganimate","gganimate"),"-",
                                    tags$a(href="https://github.com/ropensci/plotly","plotly"),"-",
                                    tags$a(href="https://github.com/jbkunst/highcharter","highcharter"),
                                    br(),
                                    br(),
                                    tags$a(href="https://github.com/jbkunst/highcharter","kableExtra"),"-",
                                    tags$a(href="https://cran.r-project.org/web/packages/RColorBrewer/index.html","RColorBrewer"),
                                    br()
                                    ),
                                box(width=3,
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22px; 
                                    text-align: justify;
                                    color: #bd022c",
                                    h3("Further Work"),
                                    "One grand plot.",
                                    br(),
                                    br(),
                                    "Use packages",
                                    tags$a(href="https://github.com/mjockers/syuzhet","syuzhet"),",",
                                    tags$a(href="https://github.com/trinker/lexicon","lexicon"),",",
                                    tags$a(href="https://github.com/trinker/sentimentr","sentimentr"),",",
                                    "and",
                                    tags$a(href="https://github.com/sfeuerriegel/SentimentAnalysis","SentimentAnalysis"),
                                    " with related to text analytics and sentiment analysis.",
                                    br(),
                                    br(),
                                    "Use the package ",
                                    tags$a(href="https://github.com/ropensci/magick","magick"),",",
                                    tags$a(href="https://github.com/ryantimpe/brickr","brickr"),
                                    "and",
                                    tags$a(href="https://github.com/tylermorganwall/rayshader","rayshader"),
                                    "on memes to make them more fun.",
                                    br()
                                    )
                                ),
                        h2("Summary"),
                        fluidRow(
                                box(width=7,
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22.8px; 
                                    text-align: justify;
                                    color: #304EA8",
                                    "South Park is 22 years old and still going on successfully also there will be new
                                    seasons. There was a movie too. According the current data there are 22 seasons.
                                    Which means there are 297 episodes so far. Highest number of episodes for a season
                                    is 18 and lowest is 10.",
                                    br(),
                                    br(),
                                    "Count of unique characters until now is 4391. It should be clear that in several
                                    episodes we have our characters playing different roles rather than being them 
                                    selves, example Eric Cartman playing Coon, Kenny being Mysterion.",
                                    br(),
                                    br(),
                                    "In this time-period our characters have spoken 79923 lines, which include
                                    940968 words. If we consider the functions related to text analytics there 
                                    are 728 unique stop words in them only 663 have been used. These stop words have 
                                    been used repeatedly, therefore total no of times stop words being used is 
                                    620116. If we consider this as a percentage this is 65.9.",
                                    br(),
                                    br(),
                                    " 'southparkr' package developed by ",
                                    tags$a(href="https://github.com/pdrhlik/southparkr","Patrik Drhlik"),
                                    "to provide information regarding South Park from IMDB and southparkwikia.
                                    I used his updated imdb_ratings dataset to anlayze about the ratings and votes.
                                    Further there is a dataset for the scripts of shows as well, but I have not 
                                    used them. Because Earlier to finding this package I was able to find 
                                    similar data on the script of south park tv show until season 18. I simply
                                    used my R skills to update this data set, by web scraping and textmining 
                                    skills.",
                                    br(),
                                    tags$a(href="https://github.com/walkerkq/textmining_southpark",
                                           "Old Data by Kaylin Pavlik"),
                                    br(),
                                    br(),
                                    "From the southparkr package I have used the swear words function and found
                                    out of 32 unique words only 26 have been used, further only 5221 times they 
                                    have been used which is 0.59% of the total no of words."

                                    ),
                                box(width=3,
                                    tableOutput("table1") %>% withSpinner(type = 6,color ="#A23333", 
                                                                          size=1.5)
                                    )
                                ),
                        h2("Ratings"),
                        fluidRow(
                                box(width=6,
                                    plotlyOutput("plot_Tri1") %>% withSpinner(type = 6,color ="#A23333", 
                                                                               size=1.5)
                                    #includeHTML("www/plot_Tri1.html")
                                    ),
                                box(width=5,
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22.8px; 
                                    text-align: justify;
                                    color: #304EA8",
                                    "First trivial plot is about ratings where from the first episode until the 
                                    297th one. Highest rating of 9.6 goes to 'Scott Tenorman Dies' from season 5
                                    episode 4. Which was about Eric Cartman revenging Scott Tenorman for selling
                                    fake pubes to him. Lowest rating goes to 'A million little fibers' with
                                    6.3 and I agree that was a weird episode. It was simply about a wash cloth 
                                    with drug addiction.",
                                    br(),
                                    br(),
                                    "If we look at the trend here, clearly there is a very stable rating for the 
                                    show from the begining until now. Before season 12 there were only few episodes
                                    which had ratings less than 7.5, but this not the case after season 12. Still
                                    this does not mean the show is any way affected because average ratings are still
                                    higher than 7.",
                                    br(),
                                    br(),
                                    "In between season 6 and 10 there were loads of good episodes which had ratings
                                    higher than 9, for example 'Make Love, Not warcraft', 'The Return of the Fellow
                                    ship of the Ring of the Two Towers', 'AWESOM-O'. "
                                    )
                                ),
                        h2("Votes"),
                        fluidRow(
                                box(width=5,
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22.8px; 
                                    text-align: justify;
                                    color: #304EA8",
                                    "Lets look at the voting from IMDB. Clearly there is a steady trend on average
                                    votes and it is 
                                    in the range of 1250 and 2250 votes. To be more precise, the last few 
                                    seasons which aired do not have a strong voter base as it is slightly 
                                    being reduced from 1750 to 1100. This occurs after season 15.",
                                    br(),
                                    br(),
                                    "Highest vote goes to the episode 'Scott Tenorman Must Die' with 6049, second
                                    place goes to 'Make Love, Not Warcraft' with 5711 votes, while third place 
                                    goes to 'The Return of the Fellow ship of the Ring of the Two Towers' with 3448
                                    votes.",
                                    br(),
                                    br(),
                                    "In between episodes 100 and 170 there are some good episodes which has votes
                                    above 2000, but this is not the case after those episodes, where only two 
                                    episodes have reached that scale.",
                                    br(),
                                    br(),
                                    "Votes reaching the lowest amounts occur in the last season which are ranged
                                    between 1250 and 500. Hope season 23 is fun."
                                    ),
                                box(width=6,
                                    plotlyOutput("plot_Tri2") %>% withSpinner(type = 6,color ="#A23333", 
                                                                              size=1.5)
                                    ) 
                                ),
                        h2("Lines"),
                        fluidRow(
                                 box(width=6,
                                     plotlyOutput("plot_Tri3") %>% withSpinner(type = 6,color ="#A23333", 
                                                                               size=1.5)
                                     ),
                                 box(width=5,
                                     style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 900; 
                                     line-height: 22.8px; 
                                     text-align: justify;
                                     color: #304EA8",
                                     "How many Lines have been spoken for each episode is what this plot is about.
                                     Not going to interpret it, just look at them and take your cursor closer to the
                                     points for more information.",
                                     br(),
                                     br(),
                                     "You know what, do this from now onwards.",
                                     HTML('<center><img src="meme2.jpg" ></center>')
                                     )
                                ),
                        h2("Characters"),
                        fluidRow(
                                box(width=5,
                                    HTML('<center><img src="meme3.jpg" width="575" height="400" ></center>')
                                    ),
                                box(width=6,
                                    plotlyOutput("plot_Tri4") %>% withSpinner(type = 6,color ="#A23333", 
                                                                              size=1.5)
                                    )
                                ),
                        h2("Words"),
                        fluidRow(
                                 box(width=6,
                                     plotlyOutput("plot_Tri5") %>% withSpinner(type = 6,color ="#A23333", 
                                                                               size=1.5)
                                     ),
                                  box(width=6,
                                      HTML('<center><img src="meme5.jpg" width="700" height="400" ></center>')
                                      )
                                ),
                        h2("Stop Words"),
                        fluidRow(
                                box(width=5,
                                    HTML('<center><img src="meme6.png" width="575" height="400" ></center>')
                                    ),
                                box(width=6,
                                    plotlyOutput("plot_Tri6") %>% withSpinner(type = 6,color ="#A23333", 
                                                                              size=1.5)
                                    )
                                ),
                        h2("Swear Words"),
                        fluidRow(
                                box(width=6,
                                    plotlyOutput("plot_Tri7") %>% withSpinner(type = 6,color ="#A23333", 
                                                                              size=1.5)
                                    ),
                                box(width=5,
                                    HTML('<center><img src="meme00.gif" width="575" height="405" ></center>')
                                    )
                               ),
                        h2("Sentiment Analysis by AFINN"),
                        fluidRow(
                                box(width=5,
                                    style = " font-family: Garamond; 
                                     font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22.8px; 
                                    text-align: justify;
                                    color: #304EA8",
                                    "I have used two packages ",
                                    tags$a(href="https://github.com/ryantimpe/brickr","brickr"),",",
                                    tags$a(href="https://github.com/tylermorganwall/rayshader","rayshader"), 
                                    "to convert an above shown image into lego shaped
                                    3d plot. I thinkg its very cool that we have such packages.",
                                    br(),
                                    br(),
                                    HTML('<center><img src="meme3_brick_ray.png" width="575" height="325" ></center>')
                                    ),
                                box(width=6,
                                    plotlyOutput("plot_Tri8") %>% withSpinner(type = 6,color ="#A23333", 
                                                                              size=1.5)
                                    )
                                )
                        ),
                        tabItem(tabName = "line",
                        fluidRow( # adding content to the line submenu ----
                                box(width = 3, solidHeader = TRUE, status = "info",
                                    title = "Introduction",
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 900; 
                                    line-height: 22.8px; 
                                    text-align: justify;
                                    color: #304EA8",
                                    "Trivial Information regarding the South Park Tv series is
                                    considered here in the forms of Lines, Words, Special Words, Ratings
                                    and Votes, Sentiment Analysis and finaly Bigram and Trigram Analysis.",
                                    br(),
                                    br(),
                                    "Several Tabs have been created for this purpose and the tab you are 
                                    currently using is to understand how Lines characteristic in the Series.
                                    Top 10 of anything is worth while reading, therefore all of these plots
                                    will be full of information based on Character, Season and Episode.", 
                                    br(),
                                    br(),
                                    "Some of these information could be useful or basic, then again it could be
                                    interesting or just plain dull."
                                    ),
                                box(width = 4, solidHeader = TRUE, status = "info",
                                    title = "Who Spoke the Most Lines ?",
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22.8px;
                                    text-align: justify;
                                    color: #680000",
                                    "Not a Surprise who has watched all seasons of Southpark where Eric 
                                    Cartman is Leading the table with 10,872 lines spoken, while second 
                                    place goes to Stan with 8120 lines and following is Kyle in the third
                                    place  with 7725 lines.",
                                    br(),
                                    br(),
                                    "Kenny is not the fourth in this chart oddly it is Randy or Stan's dad
                                    speaking 3125 lines and fifth place is for Butters with 2940 lines.
                                    We have Mr. Garrison who just became president in sixth place with 1072 lines.
                                    Seventh place is held by Sharon (the only female in the Top 10) speaking
                                    980 lines as Stan's mom.",
                                    br(),
                                    br(),
                                    "Next three are well known characters. Finally, seeing Kenny
                                    in eigth place is a relief (even though he often mumbles) with 962 lines.
                                    Ninth place is for speaking 930 lines by Gerald or Kyle's dad.
                                    Unlike the other nine in tenth place is a long missed character Chef 
                                    with 918 Lines."
                                   ),
                                box(width = 5, solidHeader = TRUE, status = "info",
                                    title = "Top 10 Characters who Spoke the Most Lines",
                                    plotOutput("plot_L_1") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(
                                 box(width=6,solidHeader = TRUE,status = "info",
                                     title = "Which Season had the Most Lines ?",
                                     style = " font-family: Garamond; 
                                    font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 22.8px;
                                     text-align: justify;
                                     color: #680000",
                                     "Earlier Seasons had Episodes upto 18 but recently they have dropped significantly
                                     to 10 Episodes per Season.It is clear that the Top 9 seasons are before season 10.",
                                     br(),
                                     br(),
                                     "Top 3 places go to Season 2, 3 and 4 with number of Lines respectively 6416, 5798 
                                     and 5680. Next three places goes to season 6, 5 and 7 in the following order with number 
                                     of lines of 5131, 4414 and 4236.",
                                     br(),
                                     br(),
                                     "We can see that over the years less number of Lines are spoken by each season.
                                     Final four places go to Season 1, 8, 9 and 11 with lines count of 4170, 3601, 3526 
                                     and 3478. It could be possible to say that with less number of episodes for each season
                                     the number of lines are reduced.",
                                     br(),
                                     br(),
                                     "Which means less words also. Further, it could also mean that there are less 
                                      interruptions by characters while speaking a line. Comparing the 1st place and 10th,
                                      clearly the number of lines have been dropped by atleast 3000. Which is staggering and
                                      this could drop further while currently (Season 22) only 10 episodes per season."
                                     
                                     ),
                                 box(width = 6, solidHeader = TRUE,status = "info",
                                     title = "Top 10 Seasons which had the Most Lines",
                                     plotOutput("plot_L_2") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                     )
                              ),
                        fluidRow(
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Top 10 Episodes which had the Most Lines",
                                    plotOutput("plot_L_3") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    ),
                                box(width=6,solidHeader = TRUE,status = "info",
                                    title = "Which Episodes had the Most Lines ?",
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22.8px;
                                    text-align: justify;
                                    color: #680000",
                                    "This plot is a bit unfair, I agree with you. The reason is , as I mentioned above 
                                    over the years number of episodes per seasons have been reduced from 18 to 10. 
                                    In certain seasons it was 14 episodes per season.",
                                    br(),
                                    br(),
                                    "Yet, I am doing this to get a slight feel, We agree that all seasons have 10 episodes,
                                    therefore this is a competition among them. Clearly episode 10 has the highest number
                                    of lines with 6245. While second place goes to Episode 4 with 6105 lines and third 
                                    place goest to the first episode with 6039 lines. ",
                                    br(),
                                    br(),
                                    "Well, it seems that we speak alot in the last episode of the season (only in early 
                                    seasons). Fourth to seventh places goes to episodes 8, 6, 7 and 5 with number of 
                                    lines with 5812, 5789, 5765 and 5737. It seems the drop in number of lines 
                                    is simply small.",
                                    br(),
                                    br(),
                                    "Final three spots are for the episodes 2, 9 and 3 with the number of lines of 5641,
                                    5544 and 5451."
                                    
                                    )
                              ),
                        fluidRow(
                                box(width=6,solidHeader = TRUE,status = "info",
                                    title = "Which Unique Episode had the Most Lines ?",
                                    style = " font-family: Garamond; 
                                    font-size: 19px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22.8px;
                                    text-align: justify;
                                    color: #680000",
                                    "Above plots in this page are focusing on season and episode, and now this is 
                                    a combination of both where it will show the number of lines of a unique episode.
                                    Here, Most the episodes are from season 2 and all of them are before season 7.",
                                    br(),
                                    br(),
                                    "The episodes 12, 16, 18, 10 and 17 are from season 2 with respective number of lines
                                    of 444, 410, 397, 394 and 380. Further, they in the places of first, third, fourth,
                                    fifth and tenth.",
                                    br(),
                                    br(),
                                    "Three episodes from season 3 are in this plot as well, which are 5, 3 and 10. Also
                                    the number of lines are 418, 388 and 385 for the above order with the places second,
                                    seventh and eigth.",
                                    br(),
                                    br(),
                                    "Finally, we have episode 4 of season 6 has 382 lines and is in ninth place of the plot.
                                    This could simply explain why season 2 has the highest number of lines in the previous
                                    plot. The nuber of lines dropped from first place to tenth place is close to 60 lines."
                                    ),
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Top 10 Unique Episodes which had the Most Lines",
                                    plotOutput("plot_L_4") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    )
                              )
                                ),
                        tabItem(tabName = "words", 
                        fluidRow( # adding content to the words submenu -----
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "Top 10 Characters who Spoke the Most Amount of Words (including Stop Words) ?",
                                     plotOutput("plot_W_1") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "Who Spoke the Most Amount of Words?",
                                     style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 22.8px;
                                     text-align: justify;
                                     color: #680000",
                                     "Unsurprisingly we can see Cartman has spoken most amount of the words and the 
                                     count is very higher with stop words and without them. Clearly these counts 
                                     are twice than the second place holder, its Stan. Third place is for Kyle.",
                                     br(),
                                     br(),
                                     "The first three places are occupied by the most important characters an they have
                                     significantly higher counts than the rest. This difference is staggering.",
                                     br(),
                                     br(),
                                     "If we consider
                                     the with stop words section of first place its close to 14 times of the tenth place.
                                     If we consider the same places for the without stop words section it is also close 
                                     to 14 times.",
                                     br(),
                                     br(),
                                     "For both plots the order of characters are same for the first eight places. Odd
                                     to see Announcer in the without stop words section of the tenth place. Further,
                                     chef is in these plots holding the seventh place."
                                    ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "Top 10 Characters who Spoke the Most Amount of Words (without Stop Words) ?",
                                     plotOutput("plot_W_2") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 10 Seasons Which had the Most Amount of Words (including Stop Words) ?",
                                    plotOutput("plot_W_3") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Which Seasons had the Most Amount of Words?",
                                    style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 22.8px;
                                     text-align: justify;
                                     color: #680000",
                                    "Highest amount of words is for season 2 with 64180 for the with stop words 
                                    section, while the without stop words section has only 22487 words.",
                                    br(),
                                    br(),
                                    "Second and Third places go to seasons 4 and 3 with stop words section and for 
                                    without stop words section its season 3 and 4. The places fourth, fifth, sixth 
                                    and seventh are not different in both plots while occupied by the seasons 6, 5, 7 and 8.",
                                    br(),
                                    br(),
                                    "Ninth place is for the season 15 in both plots. Tenth place has 42384 words for
                                    the with stop words section of season 14 and for without stop words its season 13 
                                    with 14737 words.",
                                    br(),
                                    br(),
                                    "So season 1 is in seventh place with 15038 words for the without stop words 
                                    section, but season 1 is not in the section of with stop words."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 10 Seasons Which had the Most Amount of Words  (without Stop Words) ?",
                                    plotOutput("plot_W_4") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 10 Episodes Which had the Most Amount of Words (including Stop Words) ?",
                                    plotOutput("plot_W_5") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Which Episodes had the Most Amount of Words?",
                                    style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 22.8px;
                                     text-align: justify;
                                     color: #680000",
                                     "As the previous page before, these plots does not necessary provide any special 
                                     information and its just general information comparing different episodes.",
                                     br(),
                                     br(),
                                     "The decrease of count from first place to tenth place has a slight slope. First place
                                     goes to episode 1 with 71063 words and 24441 words recpectively for the sections with stop words 
                                     and without stop words.",
                                     br(),
                                     br(),
                                     "All the other places are occupied by episodes from 2 to 10. The places eigth and tenth
                                     are occupied with episodes 7 and 3 for both plots. For the tenth place we can see the word count of
                                     66600 and 22849 for the sections with stop words and without stop words.",
                                     br(),
                                     br(),
                                     "So obviously episodes higher than 10 are not in these plots, because there are only
                                     a few seasons which reached episodes higher than 10 or 14."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 10 Episodes Which had the Most Amount of Words (without Stop Words) ?",
                                    plotOutput("plot_W_6") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Top 10 Unique Episodes Which had the Most Amount of Words (including Stop Words) ?",
                                  plotOutput("plot_W_7") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                         color.background="#FFFFFF")
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Which Unique Episodes had the Most Amount of Words?",
                                  style = " font-family: Garamond; 
                                  font-size: 19px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 22.8px;
                                  text-align: justify;
                                  color: #680000",
                                  "Section of with stop words has episodes mostly from the second and fourth season 
                                  with 3 episodes each. While season 6 has two episodes which in first place and eigth 
                                  with 4302 words and 3946 words.",
                                  br(),
                                  br(),
                                  "Last place goes to episode 16 of season 2 with 3913
                                  words. We can see a slight drop in number of words over the last six places.",
                                  br(),
                                  br(),
                                  "Considering the without stop words section clearly the first has number of words of 1455
                                  and last place episode has 1360 words. Which is close to one third of the counts with stop words 
                                  plot.",
                                  br(),
                                  br(),
                                  "First place goes to the fifth episode of fifth season. Further, season 2 and 3
                                  has four episodes each in this plot. We can see a similar decrease in number of words
                                  from the highest count to the lowest than the with stop words section."
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Top 10 Unique Episodes Which had the Most Amount of Words(without Stop Words) ?",
                                  plotOutput("plot_W_8") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                         color.background="#FFFFFF")
                                  )
                              )
                                ),
                        tabItem(tabName = "swords",
                        fluidRow( # adding content to the special words submenu ----
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Most mentioned Top 10 Words (with Stop Words)",
                                  plotOutput("plot_SW_1") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                  ),
                              box(width=4,solidHeader = TRUE,status = "info",
                                  title = "Which 10 Words were Most Mentioned ?",
                                  style = " font-family: Garamond; 
                                  font-size: 19px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 22.8px;
                                  text-align: justify;
                                  color: #680000",
                                  "Now lets discuss which words are used frequently in Southpark. If we consider the stop 
                                  words clearly the word 'you' is used 29393 times, while 'the' is second.",
                                  br(),
                                  br(),
                                  "The gap between first and second places is close to 700. But third
                                  place goest for the word 'to' with 23182, which is very lower. This drop continues until
                                  sixth place and
                                  rest of the words are also stop words. Finally, tenth place is achieved by the word 'we'
                                  with the count of 10694.",
                                  br(),
                                  br(),
                                  "Similarly for without stop words section we can see the word 'yeah' in first place with 
                                  4186 counts, second place goes to 'gonna' with 2986 counts and 'uh' is in third place with 
                                  2610 counts.",
                                  br(),
                                  br(),
                                  "Characters of southpark frequently call 'Kyle' than 'god' and more than that they prefer
                                  using the words 'guys', 'dude' and 'time' with counts of 2282, 2239 and 2053."
                                 ),
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Most mentioned Top 10 Words (without Stop Words)",
                                  plotOutput("plot_SW_2") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                          color.background="#FFFFFF")
                                  )
                              
                              ),
                        fluidRow(
                              box(width = 4,solidHeader = TRUE,status = "info",
                                  title = "Swear Words from southparkr package",
                                  style = " font-family: Garamond; 
                                    font-size: 19px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 900; 
                                  line-height: 22.8px; 
                                  text-align: justify;
                                  color: #304EA8",
                                  tags$a(href='https://github.com/pdrhlik/southparkr',"southparkr"),
                                  " package was recently released in GitHub. Which has information regarding 
                                  Southpark from ",tags$a(href='https://www.imdb.com/title/tt0121955/',"IMDB"), "and",
                                  tags$a(href='https://southpark.fandom.com/wiki/South_Park_Archives',"Wikia."), 
                                  br(),
                                  br(),
                                  "IMDB contains information regarding ratings, votes and episode names, where 
                                  Wikia has information regarding dialogue lines spoken by our characters.",
                                  br(),
                                  br(),
                                  "Using this package I shall use the swear_words() function to find how often
                                  these words are used. There are 32 words classified as swear words.",
                                  br(),
                                  br(),
                                  " The southparkr package is developed by ",
                                  tags$a(href= 'https://patrio.blog/',"Patrik Drhlik"), 
                                  "I would like to thank him for developing this package. This package has information
                                  until season 22 for now, but we can update the information while its being released."
                                  ),
                              box(width = 3,solidHeader = TRUE,status = "info",
                                  title = "Swear Words Usage as an Overall", collapsible = TRUE,
                                  style = " font-family: Garamond; 
                                  font-size: 19px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 22.8px;
                                  text-align: justify;
                                  color: #680000",
                                  "Swearing in Southpark is an occurence, from this plot we can say if it is 
                                  often happens or not. Most mentioned swear word is 'damn' with 704 counts, while
                                  second place is for 'ass' with 665 counts. Third place goes to the word 'fuck' with 652 
                                  counts.",
                                  br(),
                                  br(),
                                  "Among the given 32 swear words only 26 have been spoken by our loved or hated
                                  characters of Southpark. The least spoken word is 'tit' with 2 counts. I am not going 
                                  to explain further about the distribution of words, because they are swear or curse words.
                                  Clearly we can see clear drop in the top 10 words spoken while they start from
                                  the count of 700's to drop to 180's."
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "info",collapsible = TRUE,
                                  title = "Distribution of Swear Words ",collapsed = TRUE,
                                  plotOutput("plot_SW_3") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                           color.background="#FFFFFF")
                                  )
                              )
                                ),
                        tabItem(tabName = "rate_vote",
                        fluidRow(# adding content to rate and voting submenu ----
                                 box(width = 6, solidHeader = TRUE,status = "info",
                                     title = "Ratings Changing over the Years",
                                     style = " font-family: Garamond; 
                                     font-size: 22px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 24px;
                                     text-align: justify;
                                     color: #680000",
                                     br(),
                                     "First season was aired in 1997 and the most recent season was aired in 2018 which
                                     is the 22nd season. The ratings information was only until season 21, but as IMDB has
                                     been updated we can download for season 22 as well. Clearly
                                     first five years have good rating in the range of above 7.5 but below 9.5.",
                                     br(),
                                     br(),
                                     "This is not the case from 5th year to tenth year, where now the range has increased for
                                     the top rating reaching the 9.5 scale and lowest rating of 6.5. 
                                     To be honest the year 2004 has the best ratings
                                     for all episodes because all episodes are above 8.0. There is more deviation in year
                                     2005 and 2006 than the previous years.",
                                     br(),
                                     br(),
                                     "In 2003 and 1997 there were two unaired pilots released they are not considered 
                                     for our analsysi at any point. Information related to this is only in the ratings 
                                     and votes data set only, not in the script data set.
                                     From year 2007 to 2011 we can see the ratings droping and a few bad episodes, which 
                                     means rating reaching the value of below than 7 but higher than 6.5. Previous behavior
                                     of higher variation in rating continues here as well.",
                                     br(),
                                     br(),
                                     "Between the years of 2012 to 2016 we can see a rise in rating but still that higher 
                                     variation continues. There are episodes which reach 6.5 in rating and some episodes
                                     which go up to 9.0 as well in rating."
                                    ),
                                 box( solidHeader = TRUE,status = "info", height = 625,
                                    title = "Ratings by Years from Season 1 to 21",
                                    plotOutput("plot_RV_1") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    )
                            ),
                        fluidRow(
                                 box(width = 6, solidHeader = TRUE,status = "info",
                                     title = "Ratings Change for all Episodes by Season",
                                     style = " font-family: Garamond; 
                                     font-size: 22px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 24px;
                                     text-align: justify;
                                     color: #680000",
                                     "Animated plot of of Ratings change over the season for episodes. No explanation here.
                                     I am just going to add a small meme here.",
                                     br(),
                                     br(),
                                     # adding meme 1 ----
                                     HTML('<center><img src="meme1.jpg" weight= 500 height="470"></center>')
                                    ),
                                box( solidHeader = TRUE,status = "info",height = 600,
                                    title = "Ratings by Episodes for All Seasons",
                                    # imageOutput("plot_RV_2") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                    #                                          color.background="#FFFFFF")
                                    HTML('<center><img src="plot_RV2.gif" width=750 height=525></center>')
                                    )
                            ),
                        fluidRow(
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = " Average Ratings for All Seasons",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "Previous two plots are quite information filled therefore here I have summarized it.
                                    Average of ratings in every season has been calculated with the number of episodes 
                                    mentioned too. ",
                                    br(),
                                    br(),
                                    "Highest average rating is for season eight with 8.63 while in season 1 its 8.08. 
                                    From season 1 to 8 we can see a clear increase in rating and in number of episodes
                                    too. Lowest rating is for season 21 (as of now) with 7.29 with 10 episodes. Second
                                    place goes to season 11 with 8.54 and 14 episodes. There is a clear oscillation 
                                    between season 8 and season 19, but after that there is a clear and strong decrease.
                                    ",
                                    br(),
                                    br(),
                                    "Highest number of episodes is for season 2 with 18 which is the only one. There 
                                    are few seasons with number of episodes with 17 and they are seasons 3, 4 and 6. While most
                                    of the seasons has number of episodes of 14 and recently its drop down to 10. This is only 
                                    after season 16."
                                    ),
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Average Ratings for All Seasons with Number of Episodes",
                                    plotOutput("plot_RV_3") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    )
                            ),
                        fluidRow(
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Votings Changing over the Years from Season 1 to 21",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    "My next focus is on voting and clearly overall understanding is that most of the times
                                    votes are in between 1500 and 3000. In a two rare occasions only it has reached beyond
                                    5000. These occurences happen for year 2001 and year 2006 but with early episodes.",
                                    br(),
                                    br(),
                                    "In between 2001 and 2007 there is slight variation in votes but this is no longer the 
                                    case where after 2015 its mostly stuck in between close to 1000 and 2000.",
                                    br(),
                                    br(),
                                    # adding meme2 -----
                                    HTML('<center><img src="meme7.jpg" width=600 height=320></center>')
                                    
                                    ),
                                box( solidHeader = TRUE,status = "info", height = 625,
                                    title = "Votes by Years",
                                    plotOutput("plot_RV_4") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                          )
                            ),
                        fluidRow(
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Votings Change for all Episodes by Season",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    "Again a wonderful animated plot without any explanation, but to make this interesting
                                    I shall add a meme. Yet please take your time to see how votes have sudden peaks
                                    on some episodes of certain seasons.",
                                    br(),
                                    br(),
                                    # adding meme3 -----
                                    HTML('<center><img src="meme4.jpg" width=600 height=460></center>')
                              
                                    ),
                                box( solidHeader = TRUE,status = "info",height = 600,
                                    title = "Votings by Episodes for All Seasons",
                                    # imageOutput("plot_RV_5") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                    #                                           color.background="#FFFFFF")
                                    HTML('<center><img src="plot_RV5.gif" width=750 height=525></center>')
                                    )
                            ),
                        fluidRow(
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = " Average Votings for All Seasons",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "Let me now describe the plot of average votes per season for all 22 seasons.
                                    It indicates that season 10 has the highest number of votes, which is 2289. Second
                                    place goes to season 11 with the count of 2241.",
                                    br(),
                                    br(),
                                    "Season 1 has votes upto 2114 which is 4th of Most voted seasons. 
                                    Most recent seasons 20 and 22 have the lowest votes respectively 1233 and 1048.",
                                    br(),
                                    br(),
                                    "Looking at the trend clearly there is drop of votes until season 4, then a sudden 
                                    increase in the next season. After this with certain ups and downs it reaches the peak
                                    in season 10.",
                                    br(),
                                    br(),
                                    "This success is shortlived where from season 12 there is down hill
                                    until 16. Well season 17 has recovered but not as the peak or before the peak. This
                                    continues for three years from seson 17 to 19. Again the drop in votes."
                                    ),
                                box(width = 6, solidHeader = TRUE,status = "info",
                                    title = "Average Votings for All Seasons with Number of Episodes",
                                    plotOutput("plot_RV_6") %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                            color.background="#FFFFFF")
                                    )
                            )
                                ),
                        tabItem(tabName = "sentiment_ana",
                        fluidRow(# adding content to sentiment analysis submenu ---- 
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "What is AFINN ?",
                                     style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 900; 
                                     line-height: 22.8px; 
                                     text-align: justify;
                                     color: #304EA8",
                                     " In order to do a sentiment analysis of how words affect the narrative we can use 
                                     this AFINN method. According to its", 
                                     tags$a(href='http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010', "website"),
                                     "it is a list of English words rated for valence with an integer between -5 and +5.",
                                     br(),
                                     br(),
                                     "While if we have the above words from the list in our lines we shall calculate
                                     a score based on how many times a word occur. Below are plots where the most positive and 
                                     negative words been shown."
                                     ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "What is bing ?",
                                     style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 900; 
                                     line-height: 22.8px; 
                                     text-align: justify;
                                     color: #304EA8",
                                     "bing is similar to AFINN but rather than providing a score it simply assess if a word 
                                     is positive or negative. This is the",
                                     tags$a(href='https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html',"website"),"to 
                                     understand how bing is used for sentiment analysis, basically its a list of words 
                                     classified positive or negative.",
                                     br(),
                                     br(),
                                     "Below are the plots where the most mentioned Top 25 positive words and negative words are 
                                     plotted with how many times have they been mentioned. "
                                     ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "What is nrc ?",
                                     style = " font-family: Garamond; 
                                     font-size: 19px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 900; 
                                     line-height: 22.8px;
                                     text-align: justify;
                                     color: #304EA8",
                                     "nrc method goes beyond bing technique and captures
                                     10 significant emotions and label each word according to them. These emotions are anger,
                                     anticipation, disgust, fear, joy, negative, positive, sadness, surprise and trust.",
                                     br(),
                                     br(),
                                     "To further understand this nrc technique refer this",
                                     tags$a(href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm","Link."),
                                     "Based on this information we have constructed one plot to indicate the Top 25 emotions
                                     and words related to them with their counts. Finally, these emotions are individually
                                     studied with respective to their words."
                                     )
                              ),
                        fluidRow( # AFINN analysis ----
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "Top 25 Words Based on AFINN",
                                     plotOutput("plot_SA_1")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "AFINN related Sentiment Analysis",
                                     style = " font-family: Garamond; 
                                     font-size: 22px; 
                                     font-style: normal; 
                                     font-variant: normal; 
                                     font-weight: 700; 
                                     line-height: 24px;
                                     text-align: justify;
                                     color: #680000",
                                     "Inside the bar I have generated the counts of words and outside the bar is the 
                                     sentiment score. We can see the highest score for positive goes to 'like' 7670 while its 
                                     been mentioned 3835 times. The last word in the positive section is 'awesome' with a score 
                                     of 1020 (255 counts)",
                                     br(),
                                     br(),
                                     "Similarly if we look at the negative side clearly the word 'no' has the highest score of -5366
                                     with 5366 counts and second place goes to 'hell' with -4340 score (1085 counts). The last place 
                                     in this plot goes to the word 'dick' with a score of -724 (181 counts).",
                                     br(),
                                     br(),
                                     "There are swear words in the negative side which makes sense but none of them have a score 
                                     better than -3000 in order to beat the top 2 words no and hell."
                                    ),
                                 box(width = 4,solidHeader = TRUE,status = "info",
                                     title = "Least 25 Words Based on AFINN",
                                     plotOutput("plot_SA_2")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(# bing analysis ----
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Positive Words Based on bing",
                                    plotOutput("plot_SA_3")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "bing related Sentiment Analysis",
                                    style = " font-family: Garamond; 
                                    text-align: justify;
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    color: #680000",
                                    "Top three positive words are 'well', 'right' and 'like' with counts higher than 3800 but less
                                    than 4000. The last places on this plot goes to the words 'hot' (260 counts), 'win' (273 counts)
                                    and 'welcome' (280 counts).",
                                    br(),
                                    br(),
                                    "Similarly the top three negative words are 'hell', 'sorry' and 'stupid'. Their counts are respectively
                                    1085, 1043 and 728. Further the last three negative words are 'poor', 'lost' and 'afraid' with counts
                                    in the following order 229, 230 and 230.",
                                    br(),
                                    br(),
                                    "We clearly see the difference in counts among positive and negative words at the top 3 places,
                                    but this is not the case while we reach the last 5 places of the plot."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Negative Words Based on bing",
                                    plotOutput("plot_SA_4")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    )                    
                              ),
                        fluidRow( #### nrc analysis ----
                                box(width = 6,solidHeader = TRUE,status = "info",
                                    title = "Top 25 emotions by nrc ",
                                    plotOutput("plot_SA_5")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 6,solidHeader = TRUE,status = "info",
                                    title = "nrc related Sentiment Analysis",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    "The word 'god' has highest number of emotions such as trust, positive, joy, fear and anticipation.
                                    Next place goes to the word 'good' while it has the same type of emotions except fear is replaced
                                    by surprise.",
                                    br(),
                                    br(),
                                    "The negative side also has such a power word which is 'hell' and it classified under the 
                                    emotions sadness, negative, fear, disgust and anger. Further there are strong words which
                                    indicate only one emotion. These words are 'stupid', 'show', 'school', 'time' and their emotions 
                                    are respectively negative, trust, trust, anticipation.",
                                    br(),
                                    br(),
                                    "Below are the plots which focuses on emotions individually and here we will have 
                                    more words than we are seeing here. Further it would be possible how some words are changing
                                    places under different emotions.",
                                    br(),
                                    br(),
                                    "Below I have considered adding two plots per row which would simplify the illustration."
                                    )
                              ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Anger",
                                    plotOutput("plot_SA_6")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Specific Emotions Anger and Disgust",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "'hell' has the highest count for the anger emotion with 1085 counts, while second place
                                    goes to 'money' with 669 and further third place is to 'bad' with 636 counts. The last three
                                    places are held by 'vote', 'cancer', 'bear' with the counts 107, 107 and 108. Odd words related anger
                                    to be honest. Few of these odd words are 'money', 'vote' and 'bear'.",
                                    br(),
                                    br(),
                                    "For the disgust emotion the plot indicates the first three places are occupied by words 'hell',
                                    'boy' and 'bad' with the counts 1085, 780 and 636. While the last three places are for the counts
                                    94, 96 and 98 with the words 'lying', 'horrible' and 'terrible'. Similarly here also we can some odd 
                                    choices related to disgust, some of them are 'boy', 'john' and 'lord'."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Disgust",
                                    plotOutput("plot_SA_7")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    )
                              ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Fear",
                                    plotOutput("plot_SA_8")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Specific Emotions Fear and Sadness",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "Fear emotion has top 3 places for the words 'god', 'hell' and 'bad' with the counts 1979,
                                    1085 and 636. While the last three words of the same plot is 'shoot' (145 counts), 
                                    'lose' (149 counts) and 'crazy' (153 counts). Here also odd choices of words related to fear
                                    such as 'watch', 'god' and 'police'. Where words 'kill', 'die' and 'fight' are suitable.",
                                    br(),
                                    br(),
                                    "For the emotion sadness clearly the highest count is received by 'hell' (1085 counts), 
                                    second place goes to 'bad' (636 counts) and third place to 'kill' (532 counts). Similarly
                                    as before the last three places are for the words 'sing', 'quiet' and 'fault' with respective
                                    similar counts of 124, 125 and 129. I feel words 'music' and 'mother' are odd for the sadness emotion 
                                    category."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Sadness",
                                    plotOutput("plot_SA_9")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    )
                             ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Negative",
                                    plotOutput("plot_SA_10")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Specific Emotions Negative and Anticipation",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "First place of the Negative emotion is also occupied by the word 'hell', second place
                                    goes to 'wait' and third place to 'boy'. The last three places are for the words 'war' (230 counts),
                                    'lost' (230 counts) and 'afraid' (230 counts). 'hell', 'stupid', 'wrong', 'war' and 'bad' some of the few 
                                    words which align with the negative concept here. While I still dont understand why 'boy' and 
                                    'wait' are in this list.",
                                    br(),
                                    br(),
                                    "Similaly for the Anctipation emotion we can see the top three places are held by words 'time',
                                    'god' and 'good'. While last three go to the words 'worry', 'pay' and 'white' with respective counts of
                                    214, 219 and 233. Words such as 'time', 'coming', 'ready', 'tomorrow' and 'start' does suit the emotion
                                    category of ancticipation."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Anticipation",
                                    plotOutput("plot_SA_11")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    )
                             ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Joy",
                                    plotOutput("plot_SA_12")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Specific Emotions Joy and Surprise",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "Emotion Joy has top three places held by the words 'god', 'good' and 'love'. Further, their counts 
                                    are 1979, 1703 and 789. Last three spots of the plot are occupied by 'safe' (190 counts), 
                                    'mother' (206 counts) and 'food' (225 counts). I understand that words such as 'love', 'money', 'friend',
                                    'pretty' and 'sex' can bring joy. While how does the word 'white' bring joy!.",
                                    br(),
                                    br(),
                                    "Similarly for the emotion surprise we can see the words 'good', 'money' and 'guess' hold the top 
                                    three places according to the counts 1703, 669, and 599. Last three places on the plot goes
                                    to the words 'cream', 'luck' and 'tree' with the corresponding counts 86, 88 and 91. While 
                                    words such as 'vote', 'young' and 'tree' being part of this category baffles me."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Surprise",
                                    plotOutput("plot_SA_13")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    )
                             ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Trust",
                                    plotOutput("plot_SA_14")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Specific Emotions Trust and Positive",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 24px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "For the emotion trust the first place goes to the word 'god' with 1979 counts, second place 
                                    to 'good' with 1703 counts and finally third place to the word 'school' 1019. It should be noted that
                                    'good' and 'god' were repeated in few other emotions as well, and only in a positive point of view. 
                                    Last three places of the plot goes to the words 'pay' (229 counts), 'white' (233 counts) and 
                                    'important' (234 counts).",
                                    br(),
                                    br(),
                                    "Final emotion in need of illustration is the category of positive. While here also the 
                                    words 'god' and 'good' are in first and second places respectively with counts of 1979, 1703.
                                    Third place goes to 'love' with 789 counts. Again words such as 'garrison' and 'sir' being positive
                                    confuses me. I get that 'love', 'money', 'friend', 'talk' and 'kind' being positive words."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Top 25 Words for Positive",
                                    plotOutput("plot_SA_15")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                                    )
                             )
                                ),
                        tabItem(tabName = "ngram",
                        fluidRow( # adding content to ngrams submenu ----
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Most Spoken Bi-grams",
                                    plotOutput("plot_ng_1")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                               color.background="#FFFFFF")
                                    
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Bi-grams and Tri-grams Analysis",
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22px;
                                    text-align: justify;
                                    color: #680000",
                                    "Sentiment analysis was done by considering the smallest unit as a word. Now we are focusing
                                    on more than one word such as bigram (two words) and trigram (three words). The two plots
                                    indicate the most frequently used two words without breaking order of a sentence.",
                                    br(),
                                    br(),
                                    "According the plot most used bigram is in-the for 2036 counts, while second place is for 
                                    have-to with 2012 counts. Further third place goes to this-is with 1799 counts. It does not
                                    make any special revelation while looking at this plot.",
                                    br(),
                                    br(),
                                    "For the most mentioned trigram, first place goes to oh-my-god with 638 counts, second place is
                                    for i-dont-know of 603 counts and third place is what-the-hell with 569 counts. Obviously you 
                                    should be able to remember highest mentioned trigram and in which situation it was mentioned. 
                                    If you do not know as a hint watch the first few seasons."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Most Spoken Tri-grams",
                                    plotOutput("plot_ng_2")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                               color.background="#FFFFFF")
                                    )    
                             ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Most Spoken Bi-grams by Character",
                                    plotOutput("plot_ng_3")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Bi-grams and Tri-grams by Character",
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22px;
                                    text-align: justify;
                                    color: #680000",
                                    br(),
                                    "Among the top 25 bigrams most of them are spoken by Cartman and most count is for you-guys.
                                    While kyle has spoken a few but not more than Stan. Further Stan also has frequently used the
                                    bigram you-guys.",
                                    br(),
                                    br(),     
                                    "Looking at the trigrams clearly Cartman leads by saying stuff such as oh-my-god, what-the-hell,
                                    and few others. While Kyle has said what-the-hell alot, but for Stan most used trigram is
                                    we-have-to. Oddly we have this one time situation where a bunch of singers saying dumb-dumb-dumb.",
                                    br(),
                                    br(),
                                    "In any of these situations for bigram the counts does not go higher than 400 except for Cartman with
                                    the bigram you-guys (709 counts). This is not the case for trigrams where the highest count is 120."
                                    ),
                                box(width = 4,solidHeader = TRUE,status = "info",
                                    title = "Most Spoken Tri-grams by Character",
                                    plotOutput("plot_ng_4")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                              color.background="#FFFFFF")
                          )
                             ),
                        fluidRow(
                                box(width=5,solidHeader = TRUE,status = "info", 
                                    title = "Most Spoken Bi-grams by Season",
                                    plotOutput("plot_ng_5")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    ),
                                box(width = 2,solidHeader = TRUE,status = "info",
                                    title = "Bi-grams and Tri-grams by Season",
                                    style = " font-family: Garamond; 
                                    font-size: 20px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 22px;
                                    text-align: justify;
                                    color: #680000",
                                    "Season 2 has the most amount of bigrams, while second place goes to season 6. None of
                                    these counts exceed 180. Nine seasons have these most common bigrams.",
                                    br(),
                                    br(),
                                    "Trigrams analysis for season illustrates that season 2 has the mount of trigram, while second
                                    place goes to season 3. While none of these counts exceed 90. Only eight seasons have these 
                                    kind of Trigrams",
                                    br(),
                                    br()
                                    ),
                                box(width=5,solidHeader = TRUE,status = "info", 
                                    title = "Most Spoken Tri-grams by Season",
                                    plotOutput("plot_ng_6")  %>% withSpinner(type = 3,color ="#099643" ,size=1.5,
                                                                             color.background="#FFFFFF")
                                    )
                            )
                              ),
                        tabItem(tabName = "seasons", # adding content to seasons menu ----
                                h2("Compare Two Seasons of Your Choice"),
                        fluidRow(# adding choices ----         
                                box(width = 2,solidHeader = TRUE,status="info",
                                    title = "Your Choice",
                                    selectInput("season1","First Season :",sort(unique(Updatedseason$Season)),1),
                                    selectInput("season2","Second Season :",sort(unique(Updatedseason$Season)),2),
                                    submitButton("Lets See")
                                    ),
                                box(width = 3,solidHeader = TRUE,status = "warning",
                                    title = "Purpose",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "1. Select 'First Season' from 1 to 22 from the drop down list.",
                                    br(),
                                    br(),
                                    "2. Select 'Second Season' from 1 to 22 from the drop down list.",
                                    br(),
                                    br(),
                                    "3. Press the button 'Lets See' to see the magic happen (be patient until the plots load).",
                                    br(),
                                    br(),
                                    "Plots related to Words, Special words, Sentiment Analysis (AFINN, bing and nrc) 
                                    are shown as necessarily between two seasons of your choice.",
                                    br()
                                    ),
                                box(width = 7,solidHeader = TRUE,status = "success",
                                    title= "1. Most Spoken People",
                                    plotOutput("plot1_1") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                                ),
                                h3("Most Spoken Words"),
                        fluidRow(# Most Spoken words ----
                                box(width = 5,solidHeader = TRUE,status = "success",
                                    title= "2. Most Spoken Words with Stop words",
                                    plotOutput("plot1_2") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 5,solidHeader = TRUE,status = "success",
                                    title= "3. Most Spoken Words without Stop words",
                                    plotOutput("plot1_3") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 2,solidHeader = TRUE, status="warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "Stop words considered here are from the package tidytext. It is a collection of 1142 words
                                    but three types of lexicons.",
                                    br(),br(),
                                    "tidytext is collection of other packges useful for text analysis.",
                                    br(),br(),
                                    "Link for tidytext:",tags$a(href='https://github.com/juliasilge/tidytext',"GitHub"),
                                    br(),
                                    tags$a(href='https://cran.r-project.org/web/packages/tidytext/index.html',"CRAN"),
                                    br()
                                    )
                              ),
                              h3("AFINN related Sentiment Analysis"),
                        fluidRow(# AFINN analysis ----
                                box(width = 2,solidHeader = TRUE, status="warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "AFINN technique is focused on using a long list of words and values assigned to it
                                    to analyze the given text data. Assigned values could be in the range of -5 and +5 but
                                    integer.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010',"Link")
                                    ),
                                box(width = 5,solidHeader = TRUE,status = "success",
                                    title= "4. Sentiment Analysis for AFINN",
                                    plotOutput("plot1_4") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 5,solidHeader = TRUE,status = "success",
                                    title= "5. Sentiment Analysis for AFINN",
                                    plotOutput("plot1_5") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                              ),
                             h3("bing related Sentiment Analysis"),
                        fluidRow(# bing analysis ----
                                box(width = 5,solidHeader = TRUE,status = "success",
                                    title = "6. Sentiment Analysis for bing",
                                    plotOutput("plot1_6") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 5, solidHeader = TRUE, status = "success",
                                    title = "7. Sentiment Analysis for bing",
                                    plotOutput("plot1_7") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 2,solidHeader = TRUE, status="warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "bing technique is focused on using a long list of words where emotions assigned to it
                                    to analyze the given text data. Assigned emotions are negative and positive.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html',"Link")
                                    )                                
                              ),
                            h3("nrc related Sentiment Analysis and Swear Words"),
                        fluidRow(# nrc analysis ----
                                box(width = 5, solidHeader = TRUE, status="success",
                                    title = "8. Sentiment Analysis for nrc : All Emotions",
                                    plotOutput("plot1_8") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 3,solidHeader = TRUE, status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "nrc technique is focused on using a long list of words where emotions assigned to it
                                    to analyze the given text data. Assigned emotions are anger,anticipation, disgust,
                                    fear, joy, negative, positive, sadness, surprise and trust.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    br(),
                                    tags$a(href='http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm',"Link")
                                    ),
                                box(width = 4, solidHeader = TRUE, status = "success",
                                    title = "9. Swear Words",
                                    plotOutput("plot1_9") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                              ),
                        fluidRow(
                                box(width = 4,solidHeader = TRUE, status = "success",
                                    title = "10. Sentiment Analysis for nrc : Anger",
                                    plotOutput("plot1_10") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 4, solidHeader = TRUE, status="success",
                                    title = "11. Sentiment Analysis for nrc : Disgust",
                                    plotOutput("plot1_11") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 4, solidHeader = TRUE, status = "success",
                                    title = "12. Sentiment Analysis for nrc : Fear",
                                    plotOutput("plot1_12") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                              ),  
                        fluidRow(
                                box(width = 4,solidHeader = TRUE, status = "success",
                                    title = "13. Sentiment Analysis for nrc : Sadness",
                                    plotOutput("plot1_13") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 4, solidHeader = TRUE, status="success",
                                    title = "14. Sentiment Analysis for nrc : Negative",
                                    plotOutput("plot1_14") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 4, solidHeader = TRUE, status = "success",
                                    title = "15. Sentiment Analysis for nrc : Anticipation",
                                    plotOutput("plot1_15") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                              ),
                        fluidRow(
                                box(width = 3,solidHeader = TRUE, status = "success",
                                    title = "16. Sentiment Analysis for nrc : Joy",
                                    plotOutput("plot1_16") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 3, solidHeader = TRUE, status="success",
                                    title = "17. Sentiment Analysis for nrc : Surprise",
                                    plotOutput("plot1_17") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 3, solidHeader = TRUE, status = "success",
                                    title = "18. Sentiment Analysis for nrc : Trust",
                                    plotOutput("plot1_18") %>% withSpinner(color = "#099643",size=1.5)
                                    ),
                                box(width = 3,solidHeader = TRUE,status="success",
                                    title = "19. Sentiment Analysis for nrc : Positive",
                                    plotOutput("plot1_19") %>% withSpinner(color = "#099643",size=1.5)
                                    )
                              )
                                ),
                        tabItem(tabName = "characters", # adding content to Character menu ----
                                h2("Compare Two Characters of Your Choice"),
                        fluidRow(# adding choices   ----     
                                box(width=2, solidHeader = TRUE, status="info",
                                  title = "Your Choice",
                                  selectInput("character1","First Character :",unique(CharacterList),"Stan"),
                                  selectInput("character2","Second Character :",unique(CharacterList),"Kyle"),
                                  submitButton("Lets See")
                                  ),
                                box(width=5, solidHeader = TRUE,status = "success",
                                    title = "1. Number of Lines Spoken in Every Season",
                                    plotOutput("plot2_1") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                   ),
                                box(width=5, solidHeader = TRUE,status = "success", 
                                    title = "2. Highest Number of Lines Spoken by Episode",
                                    plotOutput("plot2_2") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                   )
                              ),
                              h3("Most Spoken Words"),
                        fluidRow(# Most Spoken Words -----
                                box(width=5, solidHeader = TRUE, status="success",
                                    title = "3. Most Frequent words with Stop Words",
                                    plotOutput("plot2_3") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=5, solidHeader = TRUE,status = "success",
                                    title = "4. Most Frequent words without Stop Words",
                                    plotOutput("plot2_4") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=2, solidHeader = TRUE,status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "Stop words considered here are from the package tidytext. It is a collection of 1142 words
                                    but three types of lexicons.",
                                    br(),br(),
                                    "tidytext is collection of other packges useful for text analysis.",
                                    br(),br(),
                                    "Link for tidytext:",tags$a(href='https://github.com/juliasilge/tidytext',"GitHub"),
                                    br(),
                                    tags$a(href='https://cran.r-project.org/web/packages/tidytext/index.html',"CRAN"),
                                    br()
                                    )
                              ),
                              h3("AFINN related Sentiment Analysis"),
                        fluidRow(# AFINN analysis ----
                                box(width=5, solidHeader = TRUE, status="success",
                                    title = "5. Sentiment Analysis for AFINN",
                                    plotOutput("plot2_5") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=2, solidHeader = TRUE,status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "AFINN technique is focused on using a long list of words and values assigned to it
                                    to analyze the given text data. Assigned values could be in the range of -5 and +5 but
                                    integer.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010',"Link")
                                    ),
                                box(width=5, solidHeader = TRUE,status = "success",
                                    title = "6. Sentiment Analysis for AFINN",
                                    plotOutput("plot2_6") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    )
                              ),
                              h3("bing related Sentiment Analysis"),
                        fluidRow(# bing analysis ----
                                box(width=5, solidHeader = TRUE, status="success",
                                    title = "7. Sentiment Analysis for bing",
                                    plotOutput("plot2_7") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=5, solidHeader = TRUE,status = "success",
                                    title = "8. Sentiment Analysis for bing",
                                    plotOutput("plot2_8") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=2, solidHeader = TRUE,status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "bing technique is focused on using a long list of words where emotions assigned to it
                                    to analyze the given text data. Assigned emotions are negative and positive.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html',"Link")
                                    )
                              ),
                              h3("nrc related Sentiment Analysis and Swear Words"),
                        fluidRow(# nrc analysis ----
                                box(width=5, solidHeader = TRUE, status="success",
                                    title = "9. Sentiment Analysis for nrc : All Emotions",
                                    plotOutput("plot2_9") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=2, solidHeader = TRUE,status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    "nrc technique is focused on using a long list of words where emotions assigned to it
                                    to analyze the given text data. Assigned emotions are anger,anticipation, disgust,
                                    fear, joy, negative, positive, sadness, surprise and trust.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm',"Link")
                                    ),
                                box(width=5, solidHeader = TRUE,status = "success",
                                    title = "10. Swear Words",
                                    plotOutput("plot2_10") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    )
                              ),
                        fluidRow(
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "11. Sentiment Analysis for nrc : Anger",
                                    plotOutput("plot2_11") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "12. Sentiment Analysis for nrc : Disgust",
                                    plotOutput("plot2_12") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "13. Sentiment Analysis for nrc : Fear",
                                    plotOutput("plot2_13") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    )
                              ),
                        fluidRow(
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "14. Sentiment Analysis for nrc : Sadness",
                                    plotOutput("plot2_14") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "15. Sentiment Analysis for nrc : Negative",
                                    plotOutput("plot2_15") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=4, solidHeader = TRUE, status="success",
                                    title = "16. Sentiment Analysis for nrc : Anticipation",
                                    plotOutput("plot2_16") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    )
                              ),
                        fluidRow(
                                box(width=3, solidHeader = TRUE, status="success",
                                    title = "17. Sentiment Analysis for nrc : Joy",
                                    plotOutput("plot2_17") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=3, solidHeader = TRUE, status="success",
                                    title = "18. Sentiment Analysis for nrc : Surprise",
                                    plotOutput("plot2_18") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=3, solidHeader = TRUE, status="success",
                                    title = "19. Sentiment Analysis for nrc : Trust",
                                    plotOutput("plot2_19") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    ),
                                box(width=3, solidHeader = TRUE, status="success",
                                    title = "20. Sentiment Analysis for nrc : Positive  ",
                                    plotOutput("plot2_20") %>% withSpinner(type = 8,color = "#099643",size=1.5)
                                    )
                              )
                                ),
                        tabItem(tabName = "character_by_season", # adding content to character by season menu ----
                                h2("Compare Two Characters of Your Choice for A Season"),
                        fluidRow(# adding choices ----  
                                box(width = 2, solidHeader = TRUE,status="info",
                                  title = "Your Choice",
                                  selectInput("character3","First Character :",unique(CharacterList),"Cartman"),
                                  selectInput("character4","Second Character :",unique(CharacterList),"Kyle"),
                                  selectInput("season3","Season :",sort(unique(Updatedseason$Season)),1),
                                  submitButton("Lets See")
                                  ),
                                box(width = 7,solidHeader = TRUE,status = "success",
                                    title= "1. Lines Spoken in Every Episode of one Season",
                                    plotOutput("plot3_1") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                                box(width = 3,solidHeader = TRUE,status = "warning",
                                    title = "Purpose",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    "1. Select 'First Character' from the drop down list.",
                                    br(),
                                    br(),
                                    "2. Select 'Second Character' from the drop down list.",
                                    br(),
                                    br(),
                                    "3. Select 'Season' from the drop down List",
                                    br(),
                                    br(),
                                    "4. Press the button 'Lets See' to see the magic happen (be patient until the plots load).",
                                    br(),
                                    br(),
                                    "Plots related to Words, Special words, Sentiment Analysis (AFINN, bing and nrc) 
                                    are shown as necessarily between two seasons of your choice.",
                                    br()
                                  )
                              ),
                              h3("Most Spoken Words"),
                        fluidRow(# Most Spoken words ----
                                box(width = 2, solidHeader = TRUE, status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "Stop words considered here are from the package tidytext. It is a collection of 1142 words
                                    but three types of lexicons.",
                                    br(),br(),
                                    "tidytext is collection of other packges useful for text analysis.",
                                    br(),br(),
                                    "Link for tidytext:",tags$a(href='https://github.com/juliasilge/tidytext',"GitHub"),
                                    br(),
                                    tags$a(href='https://cran.r-project.org/web/packages/tidytext/index.html',"CRAN"),
                                    br()
                                  ),
                                box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "2. Most Frequent Words with Stop words",
                                  plotOutput("plot3_2") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                                box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "3. Most Frequent Words without Stop words",
                                  plotOutput("plot3_3") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              ),
                              h3("AFINN related Sentiment Analysis"),
                        fluidRow(# AFINN analysis ----
                                box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "4. Sentiment Analysis for AFINN",
                                  plotOutput("plot3_4") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                                box(width = 2, solidHeader = TRUE, status = "warning",
                                    title = "Trivial Information",
                                    style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                    br(),
                                    "AFINN technique is focused on using a long list of words and values assigned to it
                                    to analyze the given text data. Assigned values could be in the range of -5 and +5 but
                                    integer.",
                                    br(),
                                    br(),
                                    "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                    br(),
                                    tags$a(href='http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010',"Link")
                                  ),
                                box(width = 5, solidHeader = TRUE, status = "success",
                                    title = "5. Sentiment Analysis for AFINN",
                                    plotOutput("plot3_5") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              ),
                              h3("bing related Sentiment Analysis"),
                        fluidRow(# bing analysis ----
                              box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "6. Sentiment Analysis for bing",
                                  plotOutput("plot3_6") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "7. Sentiment Analysis for bing",
                                  plotOutput("plot3_7") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 2, solidHeader = TRUE, status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                    font-size: 22px; 
                                    font-style: normal; 
                                    font-variant: normal; 
                                    font-weight: 700; 
                                    line-height: 23px;
                                    text-align: justify;
                                    color: #006400",
                                  br(),
                                  "bing technique is focused on using a long list of words where emotions assigned to it
                                    to analyze the given text data. Assigned emotions are negative and positive.",
                                  br(),
                                  br(),
                                  "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                    this sentiment analysis.",
                                  br(),
                                  tags$a(href='https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html',"Link")
                                  )
                              ),
                              h3("Swear Words and nrc related Sentiment Analysis"),
                        fluidRow(# nrc analysis ----
                              box(width = 2, solidHeader = TRUE, status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                  font-size: 22px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 23px;
                                  text-align: justify;
                                  color: #006400",
                                  "nrc technique is focused on using a long list of words where emotions assigned to it
                                  to analyze the given text data. Assigned emotions are anger,anticipation, disgust,
                                  fear, joy, negative, positive, sadness, surprise and trust.",
                                  br(),
                                  br(),
                                  "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                  this sentiment analysis.",
                                  br(),
                                  tags$a(href='http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm',"Link")
                                  ),
                              box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "8. Swear Words",
                                  plotOutput("plot3_8") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 5, solidHeader = TRUE, status = "success",
                                  title = "9. Sentiment Analysis for nrc : All Emotions",
                                  plotOutput("plot3_9") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              ),
                        fluidRow(
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "10. Sentiment Analysis for nrc : Anger",
                                  plotOutput("plot3_10") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "11. Sentiment Analysis for nrc : Disgust",
                                  plotOutput("plot3_11") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "12. Sentiment Analysis for nrc : Fear",
                                  plotOutput("plot3_12") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              ),
                        fluidRow(
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "13. Sentiment Analysis for nrc : Sadness",
                                  plotOutput("plot3_13") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "14. Sentiment Analysis for nrc : Negative",
                                  plotOutput("plot3_14") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 4, solidHeader = TRUE, status = "success",
                                  title = "15. Sentiment Analysis for nrc : Anticipation",
                                  plotOutput("plot3_15") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              ),
                        fluidRow(
                              box(width = 3, solidHeader = TRUE, status = "success",
                                  title = "16. Sentiment Analysis for nrc : Joy",
                                  plotOutput("plot3_16") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 3, solidHeader = TRUE, status = "success",
                                  title = "17. Sentiment Analysis for nrc : Surprise",
                                  plotOutput("plot3_17") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 3, solidHeader = TRUE, status = "success",
                                  title = "18. Sentiment Analysis for nrc : Trust",
                                  plotOutput("plot3_18") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  ),
                              box(width = 3, solidHeader = TRUE, status = "success",
                                  title = "19. Sentiment Analysis for nrc : Positive",
                                  plotOutput("plot3_19") %>% withSpinner(type = 5,color = "#099643",size=1.5)
                                  )
                              )
                                ),
                        tabItem( # adding contents to season by character menu ----
                                tabName = "seasons_by_character",
                              h2("Compare Two Seasons of Your Choice for A Character"),
                        fluidRow(# adding choices ----
                              box(width = 2, solidHeader = TRUE,status="info",
                                  title = "Your Choice",
                                  selectInput("character5","Character :",unique(CharacterList),"Cartman"),
                                  selectInput("season4","First Season :",sort(unique(Updatedseason$Season)),3),
                                  selectInput("season5","Second Season :",sort(unique(Updatedseason$Season)),6),
                                  submitButton("Lets See")
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "1. Number of Lines Spoken in different Seasons for same Character",
                                  plotOutput("plot4_1") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "2. Number of Words Spoken in different Seasons for same Character",
                                  plotOutput("plot4_2")%>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  )
                            ),
                            h3("Most Spoken Words"),
                        fluidRow(# Most Spoken words ----
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "3. Most Frequent Words with stop words",
                                  plotOutput("plot4_3")%>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "4. Most Frequent Words without stop words",
                                  plotOutput("plot4_4")%>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 2,solidHeader = TRUE,status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                  font-size: 22px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 23px;
                                  text-align: justify;
                                  color: #006400",
                                  br(),
                                  "Stop words considered here are from the package tidytext. It is a collection of 1142 words
                                  but three types of lexicons.",
                                  br(),br(),
                                  "tidytext is collection of other packges useful for text analysis.",
                                  br(),br(),
                                  "Link for tidytext:",tags$a(href='https://github.com/juliasilge/tidytext',"GitHub"),
                                  br(),
                                  tags$a(href='https://cran.r-project.org/web/packages/tidytext/index.html',"CRAN"),
                                  br()
                                  )
                            ),
                            h3("AFINN related Sentiment Analysis"),
                        fluidRow(# AFINN analysis ----
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "5. Sentiment Analysis for AFINN",
                                  plotOutput("plot4_5") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 2,solidHeader = TRUE,status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                  font-size: 22px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 23px;
                                  text-align: justify;
                                  color: #006400",
                                  br(),
                                  "AFINN technique is focused on using a long list of words and values assigned to it
                                  to analyze the given text data. Assigned values could be in the range of -5 and +5 but
                                  integer.",
                                  br(),
                                  br(),
                                  "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                  this sentiment analysis.",
                                  br(),
                                  tags$a(href='http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010',"Link")
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "6. Sentiment Analysis for AFINN",
                                  plotOutput("plot4_6") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  )
                            ),
                            h3("bing related Sentiment Analysis"),
                        fluidRow(# bing analysis -----
                              box(width = 2,solidHeader = TRUE,status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                  font-size: 22px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 23px;
                                  text-align: justify;
                                  color: #006400",
                                  br(),
                                  "bing technique is focused on using a long list of words where emotions assigned to it
                                  to analyze the given text data. Assigned emotions are negative and positive.",
                                  br(),
                                  br(),
                                  "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                  this sentiment analysis.",
                                  br(),
                                  tags$a(href='https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html',"Link")
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "7. Sentiment Analysis for bing",
                                  plotOutput("plot4_7") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "8. Sentiment Analysis for bing",
                                  plotOutput("plot4_8") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                              )
                            ),
                            h3("nrc related Sentiment Analysis and Swear Words"),
                        fluidRow(# nrc analysis -----
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "9. Sentiment Analysis for nrc : All Emotions",
                                  plotOutput("plot4_9") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 5,solidHeader = TRUE,status = "success",
                                  title= "10. Swear Words",
                                  plotOutput("plot4_10") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 2,solidHeader = TRUE,status = "warning",
                                  title = "Trivial Information",
                                  style = " font-family: Garamond; 
                                  font-size: 22px; 
                                  font-style: normal; 
                                  font-variant: normal; 
                                  font-weight: 700; 
                                  line-height: 23px;
                                  text-align: justify;
                                  color: #006400",
                                  "nrc technique is focused on using a long list of words where emotions assigned to it
                                  to analyze the given text data. Assigned emotions are anger,anticipation, disgust,
                                  fear, joy, negative, positive, sadness, surprise and trust.",
                                  br(),
                                  br(),
                                  "Using the 'sentiments' data set provided by the tidytext package we can complete 
                                  this sentiment analysis.",
                                  br(),
                                  tags$a(href='http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm',"Link")
                                  )
                            ),
                        fluidRow(
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="11. Sentiment Analysis for nrc : Anger",
                                  plotOutput("plot4_11") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="12. Sentiment Analysis for nrc : Disgust",
                                  plotOutput("plot4_12") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="13. Sentiment Analysis for nrc : Fear",
                                  plotOutput("plot4_13") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  )
                            ),
                        fluidRow(
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="14. Sentiment Analysis for nrc : Sadness",
                                  plotOutput("plot4_14") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="15. Sentiment Analysis for nrc : Negative",
                                  plotOutput("plot4_15") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 4,solidHeader = TRUE,status = "success",
                                  title="16. Sentiment Analysis for nrc : Anticipation",
                                  plotOutput("plot4_16") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  )
                            ),
                        fluidRow(
                              box(width = 3,solidHeader = TRUE,status = "success",
                                  title="17. Sentiment Analysis for nrc : Joy",
                                  plotOutput("plot4_17") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 3,solidHeader = TRUE,status = "success",
                                  title="18. Sentiment Analysis for nrc : Surprise",
                                  plotOutput("plot4_18") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 3,solidHeader = TRUE,status = "success",
                                  title="19. Sentiment Analysis for nrc : Trust",
                                  plotOutput("plot4_19") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  ),
                              box(width = 3,solidHeader = TRUE,status = "success",
                                  title="20. Sentiment Analysis for nrc : Positive",
                                  plotOutput("plot4_20") %>% withSpinner(type = 7,color = "#099643",size=1.5)
                                  )
                            )
                                )
                        )
               )                 
              )
