library(shiny)

load("Data/southparkData.RData")

# Define server logic 
shinyServer(
  function(input, output) 
    {
    output$plot1_1<-renderPlot({ # adding plots for season menu ----
                              subset(Updatedseason,Season==input$season1| Season==input$season2) %>%
                              group_by(Season) %>%
                              count(Character,sort = TRUE) %>%
                              top_n(15) %>%
                              ggplot(.,aes(fct_inorder(Character),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              geom_text(aes(label=n),hjust="inward")+
                              facet_grid(~Season)+
                              ylab("No of Lines")+xlab("People")+
                              ggtitle("Who Spoke most frequently ?")+
                              theme_southpark_bp3
                              
                              })
    output$plot1_2<-renderPlot({
                              subset(Updatedseason,Season==input$season1| Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              count(word,sort = TRUE) %>%
                              top_n(15) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              geom_text(aes(label=n),hjust="inward")+
                              facet_grid(~Season)+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("What Words were Spoken most frequently ?")+
                              theme_southpark_bp3
                              })
    output$plot1_3<-renderPlot({
                              subset(Updatedseason, Season == input$season1 | Season == input$season2) %>%
                              mutate(Line = as.character(Line)) %>%
                              unnest_tokens(word, Line) %>%
                              anti_join(stop_words) %>%
                              group_by(Season) %>%
                              count(word, sort = TRUE) %>%
                              top_n(15) %>%
                              ggplot(., aes(fct_inorder(word), n)) +
                              geom_col(fill="#31773c",color="white") + coord_flip() +
                              geom_text(aes(label = n), hjust = "inward") +
                              facet_grid( ~ Season) +
                              ylab("No of Times") + xlab("Words") +
                              ggtitle("What Words were Spoken most frequently ?")+
                              theme_southpark_bp3
                              })
    output$plot1_4<-renderPlot({
                              subset(Updatedseason,Season == input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("afinn")) %>%
                              count(word,score,sort = TRUE) %>%
                              mutate(sentiment=score * n ) %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),sentiment)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=sentiment),hjust="inward")+
                              ylab("Sentiment Score")+xlab("Words")+
                              ggtitle("Sentiment Scores for Different Seasons",
                                      subtitle = "Most 25")+theme_southpark_bp3
                              })
    output$plot1_5<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("afinn")) %>%
                              count(word,score,sort=TRUE) %>%
                              mutate(sentiment=score * n ) %>%
                              top_n(-25) %>%
                              ggplot(.,aes(fct_inorder(word),sentiment)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=sentiment),hjust="inward")+
                              ylab("Sentiment Score")+xlab("Words")+
                              ggtitle("Sentiment Scores for Different Seasons",
                                      subtitle = "Least 25")+theme_southpark_bp3
                              })
    output$plot1_6<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("bing")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="positive") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+ coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons",
                                      subtitle = "Positive 25")+theme_southpark_bp3
                              })
    output$plot1_7<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("bing")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="negative") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons",
                                      subtitle = "Negative 25")+theme_southpark_bp3
                              })
    output$plot1_8<-renderPlot({
                              subset(Updatedseason,Season== input$season1 | Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n,fill=sentiment)) +
                              geom_col()+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust=+1,position="stack",color="#ff0000")+
                              ylab("No of Times")+xlab("Words")+
                              scale_fill_brewer(palette = "PuOr")+
                              ggtitle("Sentiment for Different Seasons",subtitle = "Top 25")+
                              theme(legend.position = "bottom")+
                              theme_southpark_bp4
                              })
    output$plot1_9<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2) %>%
                              group_by(Season) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              inner_join(tibble(word=swear_words())) %>%
                              count(word,sort = TRUE) %>%
                              ggplot(.,aes(fct_inorder(word),n))+
                              geom_col(fill="#31773c",color="white")+
                              geom_text(aes(label=n),hjust="inward")+
                              coord_flip()+ facet_wrap(~Season)+
                              xlab("Swear Word")+ylab("Count")+
                              ggtitle("Most Spoken Swear Words for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_10<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="anger") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_11<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="disgust") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_12<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="fear") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_13<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="sadness") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_14<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="negative") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_15<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2 ) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="anticipation") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_16<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season==input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="joy") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_17<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="surprise") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_18<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="trust") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
    output$plot1_19<-renderPlot({
                              subset(Updatedseason,Season== input$season1| Season== input$season2) %>%
                              mutate(Line=as.character(Line)) %>%
                              unnest_tokens(word,Line) %>%
                              group_by(Season) %>%
                              inner_join(get_sentiments("nrc")) %>%
                              count(word,sentiment,sort=TRUE) %>%
                              filter(sentiment=="positive") %>%
                              top_n(25) %>%
                              ggplot(.,aes(fct_inorder(word),n)) +
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Season)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Times")+xlab("Words")+
                              ggtitle("Sentiment for Different Seasons")+
                              theme_southpark_bp3
                              })
      
    output$plot2_1<-renderPlot({ # adding plots for character menu ----
                              subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                              group_by(Character) %>%
                              count(Season,sort=TRUE) %>%
                              ggplot(.,aes(factor(Season),n))+
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              geom_text(aes(label=n),hjust="inward")+
                              facet_grid(~Character)+
                              ylab("No of Lines")+xlab("Season")+
                              ggtitle("No of Lines spoken by selected Characters")+
                              theme_southpark_bp3
                              })
    output$plot2_2<-renderPlot({
                              subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                              group_by(Character,Season,Episode) %>%
                              count(Season,sort=TRUE) %>%
                              unite("Sea_Epi",Season,Episode) %>%
                              top_n(10) %>%
                              ggplot(.,aes(fct_inorder(Sea_Epi),n))+
                              geom_col(fill="#31773c",color="white")+coord_flip()+
                              facet_grid(~Character)+
                              geom_text(aes(label=n),hjust="inward")+
                              ylab("No of Lines")+xlab("Season_Episode")+
                              ggtitle("No of Lines by Selected Character")+
                              theme_southpark_bp3
                              })
    output$plot2_3<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            group_by(Character,Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line)  %>%
                            group_by(Character) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot2_4<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            group_by(Character,Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line)  %>%
                            anti_join(stop_words) %>%
                            group_by(Character) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot2_5<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores between two Characters",
                                    subtitle = "Most 25")+theme_southpark_bp3
                            })
    output$plot2_6<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(-25) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores between two Characters",
                                    subtitle = "Least 25")+theme_southpark_bp3
                            })
    output$plot2_7<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+ coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between two Characters",
                                    subtitle = "Positive 25")+theme_southpark_bp3
                            })
    output$plot2_8<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character",
                                    subtitle = "Negative 25")+theme_southpark_bp3      
                            })
    output$plot2_9<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n,fill=sentiment)) +
                            geom_col()+coord_flip()+
                            geom_text(aes(label=n),position="stack",hjust=1,color="#ff0000")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character",subtitle = "Top 25")+
                            theme(legend.position = "bottom")+
                            scale_fill_brewer(palette = "PuOr")+theme_southpark_bp4
                            })
    output$plot2_10<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            group_by(Character) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(tibble(word=swear_words())) %>%
                            count(word,sort = TRUE) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),hjust="inward")+
                            coord_flip()+ facet_wrap(~Character)+
                            xlab("Swear Word")+ylab("Count")+
                            ggtitle("Most Spoken Swear Words for Different Characters")+
                            theme_southpark_bp3
                            })
    output$plot2_11<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anger") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_12<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="disgust") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_13<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="fear") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_14<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="sadness") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_15<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_16<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anticipation") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_17<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="joy") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_18<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="surprise") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_19<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="trust") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot2_20<-renderPlot({
                            subset(Updatedseason,Character==input$character1| Character==input$character2) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_1<-renderPlot({ # adding plots for character by season ----
                            subset(Updatedseason,Character==input$character3 & Season==input$season3| 
                            Character==input$character4 & Season ==input$season3) %>%
                            group_by(Character) %>%
                            count(Episode,sort=TRUE) %>%
                            ggplot(.,aes(factor(Episode),n))+
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Character)+
                            ylab("No of Lines")+xlab("Episodes")+
                            ggtitle("No of Lines spoken by selected Characters")+
                            theme_southpark_bp3
                            })
    output$plot3_2<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            group_by(Character,Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line)  %>%
                            group_by(Character) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot3_3<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            group_by(Character,Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line)  %>%
                            anti_join(stop_words) %>%
                            group_by(Character) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot3_4<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores between two Characters",
                                    subtitle = "Most 15")+theme_southpark_bp3
                            })
    output$plot3_5<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(-15) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores between two Characters",
                                    subtitle = "Least 15")+theme_southpark_bp3
                            })
    output$plot3_6<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+ coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between two Characters",
                                    subtitle = "Positive 15")+theme_southpark_bp3
                            })
    output$plot3_7<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character",
                                    subtitle = "Negative 15")+theme_southpark_bp3
                            })
    output$plot3_8<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            group_by(Character,Season) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(tibble(word=swear_words())) %>%
                            count(word, sort=TRUE) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),hjust="inward")+
                            coord_flip()+ facet_wrap(~Character)+
                            xlab("Swear Word")+ylab("Count")+
                            ggtitle("Most Spoken Swear Words for Different Characters", 
                                    subtitle = " The Same Season")+theme_southpark_bp3
                            
                            })
    output$plot3_9<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n,fill=sentiment)) +
                            geom_col()+coord_flip()+
                            geom_text(aes(label=n),hjust=1,position="stack",color="#ff0000")+
                            facet_grid(~Character)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character",subtitle = "Top 15")+
                            theme(legend.position = "bottom")+
                            scale_fill_brewer(palette = "PuOr")+
                            theme_southpark_bp4
                            })
    output$plot3_10<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anger") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_11<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="disgust") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_12<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="fear") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_13<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="sadness") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_14<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_15<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anticipation") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_16<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="joy") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_17<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="surprise") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_18<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="trust") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot3_19<-renderPlot({
                            subset(Updatedseason,Character==input$character3 & Season == input$season3| 
                            Character==input$character4 & Season == input$season3) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Character) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Character)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment between Character")+
                            theme_southpark_bp3
                            })
    output$plot4_1<-renderPlot({# adding plots for season by character ----
                            subset(Updatedseason,Season==input$season4 & Character == input$character5| 
                                   Season==input$season5 & Character == input$character5) %>%
                            group_by(Season,Episode) %>%
                            count(Character,sort = TRUE) %>%
                            ggplot(.,aes(factor(Episode),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Season)+
                            ylab("No of Lines")+xlab("Episode")+
                            ggtitle("No of Lines for all episodes of two different seasons ?")+
                            theme_southpark_bp3
                            })
    output$plot4_2<-renderPlot({
                            subset(Updatedseason,Season==input$season4 & Character == input$character5| 
                                   Season==input$season5 & Character == input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season,Episode) %>%
                            count(Character,sort = TRUE) %>%
                            ggplot(.,aes(factor(Episode),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Season)+
                            ylab("No of Words")+xlab("Episode")+
                            ggtitle("No of words for all episodes of two different seasons ?")+
                            theme_southpark_bp3
                            })
    output$plot4_3<-renderPlot({
                            subset(Updatedseason,Season==input$season4 & Character ==input$character5| 
                                         Season==input$season5 & Character == input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Season)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot4_4<-renderPlot({
                            subset(Updatedseason,Season==input$season4 & Character ==input$character5 | 
                                         Season==input$season5 & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            group_by(Season) %>%
                            count(word,sort = TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            facet_grid(~Season)+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("What Words were Spoken most frequently ?")+
                            theme_southpark_bp3
                            })
    output$plot4_5<-renderPlot({
                            subset(Updatedseason,Season==input$season4 & Character ==input$character5| 
                                    Season==input$season5 & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores for Different Seasons",
                                    subtitle = "Most 15")+theme_southpark_bp3
                            })
    output$plot4_6<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort=TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(-15) %>%
                            ggplot(.,aes(fct_inorder(word),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=sentiment),hjust="inward")+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores for Different Seasons",
                                    subtitle = "Least 15")+theme_southpark_bp3
                            })
    output$plot4_7<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                     Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+ coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons",
                                    subtitle = "Positive 15")+theme_southpark_bp3
                            })
    output$plot4_8<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons",
                                    subtitle = "Negative 15")+theme_southpark_bp3
                            })
    output$plot4_9<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            group_by(Season) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(tibble(word=swear_words())) %>%
                            count(word,sort = TRUE) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),hjust="inward",color="#ff0000")+
                            coord_flip()+ facet_wrap(~Season)+
                            xlab("Swear Word")+ylab("Count")+
                            ggtitle("Most Spoken Swear Words for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_10<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n,fill=sentiment)) +
                            geom_col()+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust=+1,position="stack")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons",
                                    subtitle = "Most 15")+
                            theme(legend.position = "bottom")+
                            scale_fill_brewer(palette = "PuOr")+
                            theme_southpark_bp4
                            })
    output$plot4_11<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                  Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anger") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_12<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="disgust") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_13<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="fear") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_14<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="sadness") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_15<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_16<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                   Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anticipation") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_17<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="joy") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_18<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                   Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="surprise") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_19<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                    Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="trust") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot4_20<-renderPlot({
                            subset(Updatedseason,Season==input$season4  & Character ==input$character5| 
                                   Season==input$season5  & Character ==input$character5) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            group_by(Season) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(15) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            facet_grid(~Season)+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment for Different Seasons")+
                            theme_southpark_bp3
                            })
    output$plot_L_1<-renderPlot({ # adding plots for Lines sub menu ----
                            Updatedseason %>%
                            count(Character,sort = TRUE) %>%
                            top_n(10) %>%
                            ggplot(.,aes(fct_inorder(Character),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Who Spoke the Most Lines?")+
                            xlab("Character")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_L_2<-renderPlot({
                            Updatedseason %>%
                            count(Season,sort = TRUE) %>%
                            top_n(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Season)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Season had the Most Lines Spoken?")+
                            xlab("Season")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_L_3<-renderPlot({
                            Updatedseason %>%
                            count(Episode,sort = TRUE) %>%
                            top_n(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Episode)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Episode had the Most Lines Spoken?")+
                            xlab("Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_L_4<-renderPlot({
                            Updatedseason %>%
                            group_by(Season,Episode) %>%
                            count(Episode,sort = TRUE) %>%
                            unite("Sea_Epi",Season,Episode) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(Sea_Epi),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Unique Episode had the Most Lines Spoken?")+
                            xlab("Season_Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_1<-renderPlot({ # adding plots for words submenu ----
                            Updatedseason %>%
                            group_by(Character) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            count(Character,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(Character),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Who Spoke the Most amount of Words?",
                                    subtitle = "With Stop Words")+
                            xlab("Character")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_2<-renderPlot({
                            Updatedseason %>%
                            group_by(Character) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            count(Character,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(Character),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Who Spoke the Most amount of Words?",
                                    subtitle = "Without Stop Words")+
                            xlab("Character")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_3<-renderPlot({
                            Updatedseason %>%
                            group_by(Season) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            count(Season,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Season)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Season had the Most amount of Words?",
                                    subtitle = "With Stop Words")+
                            xlab("Season")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_4<-renderPlot({
                            Updatedseason %>%
                            group_by(Season) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            count(Season,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Season)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Season had the Most amount of Words?",
                                    subtitle = "Without Stop Words")+
                            xlab("Season")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_5<-renderPlot({
                            Updatedseason %>%
                            group_by(Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            count(Episode,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Episode)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Episode had the Most amount of Words?",
                                    subtitle = "With Stop Words")+
                            xlab("Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_6<-renderPlot({
                            Updatedseason %>%
                            group_by(Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            count(Episode,sort = TRUE) %>%
                            head(10) %>%
                            ggplot(.,aes(fct_inorder(factor(Episode)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Episode had the Most amount of Words?",
                                    subtitle = "Without Stop Words")+
                            xlab("Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_7<-renderPlot({
                            Updatedseason %>%
                            group_by(Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            count(Episode,sort = TRUE) %>%
                            head(10) %>%
                            unite("Sea_Epi",Season,Episode) %>%
                            ggplot(.,aes(fct_inorder(factor(Sea_Epi)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Unique Episode had the Most amount of Words?",
                                    subtitle = "With Stop Words")+
                            xlab("Season_Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_W_8<-renderPlot({
                            Updatedseason %>%
                            group_by(Season,Episode) %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            count(Episode,sort = TRUE) %>%
                            head(10) %>%
                            unite("Sea_Epi",Season,Episode) %>%
                            ggplot(.,aes(fct_inorder(factor(Sea_Epi)),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Which Unique Episode had the Most amount of Words?",
                                    subtitle = "Without Stop Words")+
                            xlab("Season_Episode")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_SW_1<-renderPlot({ # adding plots for swear words submenu ----
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            count(word,sort = TRUE) %>%
                            top_n(10) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Most Mentioned Words?",
                                    subtitle = "With Stop Words")+
                            xlab("Word")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_SW_2<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            anti_join(stop_words) %>%
                            count(word,sort = TRUE) %>%
                            top_n(10) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),vjust="inward")+
                            ggtitle("Most Mentioned Words?",
                                    subtitle = "Without Stop Words")+
                            xlab("Word")+ylab("Count")+
                            theme_southpark_bp
                            })
    output$plot_SW_3<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(tibble(word=swear_words())) %>%
                            count(word,sort = TRUE) %>%
                            ggplot(.,aes(fct_inorder(word),n))+
                            geom_col(fill="#31773c",color="white")+
                            geom_text(aes(label=n),hjust="inward")+
                            coord_flip()+ 
                            xlab("Swear Word")+ylab("Count")+
                            ggtitle("Most Spoken Swear Words")+
                            theme_southpark_bp2
                            })
    output$plot_SA_1<-renderPlot({ # adding plots for sentiment analysis sub menu ----
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(factor(word)),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=sentiment),hjust=-0.15)+
                            geom_text(aes(label=n),hjust=1)+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores")+theme_southpark_bp2
                            })
    output$plot_SA_2<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("afinn")) %>%
                            count(word,score,sort = TRUE) %>%
                            mutate(sentiment=score * n ) %>%
                            top_n(-25) %>%
                            ggplot(.,aes(fct_inorder(factor(word)),sentiment)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=sentiment),hjust=1)+
                            geom_text(aes(label=n),hjust=-0.15)+
                            ylab("Sentiment Score")+xlab("Words")+
                            ggtitle("Sentiment Scores")+theme_southpark_bp2
                            })
    output$plot_SA_3<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="positive") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+ coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_4<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("bing")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="negative") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_5<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n,fill=sentiment)) +
                            geom_col(color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust=1,position="stack",color="#ff0000")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+
                            scale_fill_brewer(palette = "PuOr")+
                            theme(legend.position = "bottom")+theme_southpark_bp4
                            })
    output$plot_SA_6<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="anger") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_7<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="disgust") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_8<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="fear") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_9<-renderPlot({
                            Updatedseason %>%
                            mutate(Line=as.character(Line)) %>%
                            unnest_tokens(word,Line) %>%
                            inner_join(get_sentiments("nrc")) %>%
                            count(word,sentiment,sort=TRUE) %>%
                            filter(sentiment=="sadness") %>%
                            top_n(25) %>%
                            ggplot(.,aes(fct_inorder(word),n)) +
                            geom_col(fill="#31773c",color="white")+coord_flip()+
                            geom_text(aes(label=n),hjust="inward")+
                            ylab("No of Times")+xlab("Words")+
                            ggtitle("Sentiment")+theme_southpark_bp2
                            })
    output$plot_SA_10<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="negative") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_SA_11<-renderPlot({
                          Updatedseason %>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="anticipation") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_SA_12<-renderPlot({
                          Updatedseason %>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="joy") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_SA_13<-renderPlot({
                          Updatedseason %>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="surprise") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_SA_14<-renderPlot({
                          Updatedseason %>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="trust") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_SA_15<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(word,Line) %>%
                          inner_join(get_sentiments("nrc")) %>%
                          count(word,sentiment,sort=TRUE) %>%
                          filter(sentiment=="positive") %>%
                          top_n(25) %>%
                          ggplot(.,aes(fct_inorder(word),n)) +
                          geom_col(fill="#31773c",color="white")+coord_flip()+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Words")+
                          ggtitle("Sentiment")+theme_southpark_bp2
                          })
    output$plot_ng_1<-renderPlot({ # adding plots for ngrams ----
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(bigram,Line, token = "ngrams", n = 2) %>%
                          count(bigram,sort = TRUE) %>%
                          head(25) %>%
                          ggplot(.,aes(fct_inorder(bigram),n))+
                          geom_col(fill="#31773c",color="white")+coord_flip()+  
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Bi-grams")+
                          ggtitle("Most Spoken Bi-grams")+theme_southpark_bp2
                          })
    output$plot_ng_2<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(Trigram,Line, token = "ngrams", n = 3) %>%
                          count(Trigram,sort = TRUE) %>%
                          remove_missing() %>%
                          head(25)  %>%
                          ggplot(.,aes(fct_inorder(Trigram),n))+
                          geom_col(fill="#31773c",color="white")+coord_flip()+  
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Tri-grams")+
                          ggtitle("Most Spoken Tri-grams")+theme_southpark_bp2
                          })
    output$plot_ng_3<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(bigram,Line, token = "ngrams", n = 2) %>%
                          group_by(Character) %>%
                          count(bigram,sort = TRUE) %>%
                          head(25) %>%
                          ggplot(.,aes(fct_inorder(bigram),n))+
                          geom_col(fill="#31773c",color="white")+
                          coord_flip()+  facet_wrap(~Character)+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Bi-grams")+
                          ggtitle("Most Spoken Bi-grams by Character")+
                          theme_southpark_bp3
                          })
    output$plot_ng_4<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(Trigram,Line, token = "ngrams", n = 3) %>%
                          group_by(Character) %>%
                          count(Trigram,sort = TRUE) %>%
                          head(25) %>%
                          ggplot(.,aes(fct_inorder(Trigram),n))+
                          geom_col(fill="#31773c",color="white")+
                          coord_flip()+  facet_wrap(~Character)+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Tri-grams")+
                          ggtitle("Most Spoken Tri-grams by Character")+
                          theme_southpark_bp3
                          })
    output$plot_ng_5<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(bigram,Line, token = "ngrams", n = 2) %>%
                          group_by(Season) %>%
                          count(bigram,sort = TRUE) %>%
                          head(25) %>%
                          ggplot(.,aes(fct_inorder(bigram),n))+
                          geom_col(fill="#31773c",color="white")+
                          coord_flip()+ facet_wrap(~Season)+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Bi-grams")+
                          ggtitle("Most Spoken Bi-grams by Season")+
                          theme_southpark_bp3
                          })
    output$plot_ng_6<-renderPlot({
                          Updatedseason%>%
                          mutate(Line=as.character(Line)) %>%
                          unnest_tokens(Trigram,Line, token = "ngrams", n = 3) %>%
                          group_by(Season) %>%
                          count(Trigram,sort = TRUE) %>%
                          remove_missing() %>%
                          head(25)  %>%
                          ggplot(.,aes(fct_inorder(Trigram),n))+
                          geom_col(fill="#31773c",color="white")+
                          coord_flip()+ facet_wrap(~Season)+
                          geom_text(aes(label=n),hjust="inward")+
                          ylab("No of Times")+xlab("Tri-grams")+
                          ggtitle("Most Spoken Tri-grams by Season")+
                          theme_southpark_bp4
                          })
    output$plot_RV_1<-renderPlot({ # adding plots for rating and votes ----
                          imdb_ratings %>%
                          subset(season_number!=23 & episode_name !="Unaired Pilot") %>%
                          ggplot(.,aes(season_number,user_rating,color=season_episode_number))+
                          geom_jitter()+
                          facet_wrap(~air_date)+
                          theme(legend.position = "bottom")+
                          labs(color="Epi No")+
                          scale_y_continuous(breaks = seq(6.5,10,.5),labels = seq(6.5,10,.5))+
                          xlab("Season")+ylab("User Rating")+
                          ggtitle("Ratings changing over the Years for South Park",
                                  subtitle = "by Season")+theme_southpark_sp
                          },height = 550)
    output$plot_RV_2<-renderImage({
                          plot_RV2<-tempfile(fileext = '.gif')                    
      
                          p<-imdb_ratings %>%
                             subset(season_number!= 23 & episode_name !="Unaired Pilot") %>%
                             ggplot(.,aes(factor(season_episode_number),user_rating))+
                             geom_col(fill="#31773c",color="white")+
                             geom_text(aes(label=user_rating),vjust="inward")+
                             transition_states(season_number,transition_length = 2,state_length = 3)+
                             enter_fade()+exit_fade()+ theme_southpark_bp+
                             xlab("Episode Number")+ylab("User Rating")+
                             ggtitle("SouthPark Rating",subtitle="Season : {closest_state}")
      
                             anim_save("plot_RV2.gif",animate(p))  
                             
                             list(src = "plot_RV2.gif",
                                  contentType = 'image/gif',
                                   width = 750,
                                   height = 525
                                  # alt = "This is alternate text"
                             )}, deleteFile = TRUE
                          )
    output$plot_RV_3<-renderPlot({
                          imdb_ratings %>%
                          subset(season_number!= 23 & episode_name !="Unaired Pilot") %>%
                          group_by(season_number) %>%
                          summarise(Mean=mean(user_rating),Count=n()) %>%
                          ggplot(.,aes(factor(season_number),Mean))+
                          geom_col(fill="#31773c",color="white")+
                          xlab("Season")+ylab("User Rating")+
                          ggtitle(" User Rating for South Park", 
                                  subtitle = "Averaging by Season with No of Episodes")+
                          geom_text(aes(label=round(Mean,2)),vjust=-0.5)+
                          geom_text(aes(label=Count),vjust="inward")+
                          theme_southpark_bp
                          })
    output$plot_RV_4<-renderPlot({
                          imdb_ratings %>%
                          subset(season_number!=23 & episode_name !="Unaired Pilot") %>%
                          ggplot(.,aes(season_number,user_votes,color=season_episode_number))+
                          geom_jitter()+
                          facet_wrap(~air_date)+
                          theme(legend.position = "bottom")+
                          labs(color="Epi No")+ 
                          xlab("Season")+ylab("User Votes")+
                          ggtitle("Votes changing over the Years for South Park",
                                  subtitle = "by Season")
                          },height = 550)
    output$plot_RV_5<-renderImage({
                          plot_RV5<-tempfile(fileext = '.gif')
      
                          p<-imdb_ratings %>%
                             subset(season_number!= 23 & episode_name !="Unaired Pilot") %>%
                             ggplot(.,aes(factor(season_episode_number),user_votes))+
                             geom_col(fill="#31773c",color="white")+
                             geom_text(aes(label=user_votes),vjust="inward")+
                             transition_states(season_number,transition_length = 2,state_length = 3)+
                             enter_fade()+exit_fade()+ theme_southpark_bp+
                             xlab("Episode Number")+ylab("User Votes")+
                             ggtitle("SouthPark Votes",subtitle="Season :{closest_state}")
                          
                          anim_save("plot_RV5.gif",animate(p))  
                          
                          list(src = "plot_RV5.gif",
                               contentType = 'image/gif',
                               width = 750,
                               height = 525
                               # alt = "This is alternate text"
                          )}, deleteFile = TRUE
                         )
    output$plot_RV_6<-renderPlot({
                          imdb_ratings %>%
                          subset(season_number!=23 & episode_name !="Unaired Pilot") %>%
                          group_by(season_number) %>%
                          summarise(Mean=mean(user_votes),Count=n()) %>%
                          ggplot(.,aes(factor(season_number),Mean))+
                          geom_col(fill="#31773c",color="white")+
                          xlab("Season")+ylab("User Votes")+
                          ggtitle("User Votes for South Park", 
                                  subtitle = "Averaging by Season with No of Episodes")+
                          geom_text(aes(label=Count),vjust=1.25)+
                          geom_text(aes(label=round(Mean)),vjust=-0.5)+
                          theme_southpark_bp
                         })
                   Southpark_Summary<-tibble(
                                             Trivia=c("No of Seasons","No of Episodes",
                                                       "No of Characters","No of Lines",
                                                       "No of Words","All Stop Words","Stop Words in ",
                                                       "No of Stop Words","Per of Stop Words","All Swear Words",
                                                       "Swear Words in","No of Swear Words",
                                                       "Per of Swear Words"),
                                               Values=c(NROW(unique(Updatedseason$Season)),
                                                        NROW(unique(Updatedseason %>% 
                                                        select(Season,Episode) %>%
                                                        group_by(Season,Episode))),
                                                        NROW(unique(Updatedseason$Character)),
                                                        NROW(Updatedseason),
                                                        Updatedseason %>% mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>% 
                                                        select(word) %>% NROW(),
                                                        NROW(unique(stop_words$word)),
                                                        Updatedseason %>% mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=unique(stop_words$word))) %>%
                                                        count(word,sort = TRUE) %>% NROW(),
                                                        Updatedseason %>% mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=unique(stop_words$word))) %>%
                                                        count(word,sort = TRUE) %>% select(n) %>% sum(),
                                                        paste0(round(((Updatedseason %>% 
                                                        mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=unique(stop_words$word))) %>%
                                                        count(word,sort = TRUE) %>% select(n) %>% sum())/
                                                        (Updatedseason %>%  mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>% 
                                                        select(word) %>% NROW()))*100,2),"%"),
                                                        NROW(swear_words()),
                                                        Updatedseason %>% mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=swear_words())) %>%
                                                        count(word,sort = TRUE) %>% NROW(),
                                                        Updatedseason %>% mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=swear_words())) %>%
                                                        count(word,sort = TRUE) %>% select(n) %>% sum(),
                                                        paste0(round(((Updatedseason %>% 
                                                        mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>%
                                                        inner_join(tibble(word=swear_words())) %>%
                                                        count(word,sort = TRUE) %>% select(n) %>% sum())/
                                                        (Updatedseason %>%  mutate(Line=as.character(Line)) %>%
                                                        unnest_tokens(word,Line) %>% 
                                                        select(word) %>% NROW()))*100,2),"%")  
                                                        )
                                                )
                   ## Trivia information plots and tables ----
                   
          output$table1<-function()
                                   { 
                                    Southpark_Summary  %>% knitr::kable("html") %>%
                                    kable_styling("striped", full_width = F) %>%
                                    column_spec(2,background = "#E69138",color="white",
                                                border_right = TRUE,width = "4cm") %>%
                                    column_spec(1,background = "blue",color="white",width = "5cm") %>%
                                    row_spec(c(9,13),background = "#CC0000",color="white") %>%
                                    row_spec(0,background = "yellow")
                                    }
          p1<-imdb_ratings %>%
            rename("Season"=season_number,"Episode"=season_episode_number,
                   "Rating"=user_rating,"Name"=episode_name) %>%
            subset(Season!=23 & Name!="Unaired Pilot") %>%
            arrange(Season,Episode) %>%
            mutate(Epi=seq_along(Season)) %>%
            ggplot(.,aes(label1=Season,label2=Episode,label3=Name,group=1))+
            geom_point(aes(x=Epi,y=Rating,color="#8B0000"))+
            geom_smooth(aes(x=Epi,y=Rating,color="#000080"))+
            theme(legend.position =  "none")+
            scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
            scale_y_continuous(breaks = seq(6,10,0.5),labels = seq(6,10,0.5))+
            xlab("Episodes")+ylab("Rating")+theme_southpark_sp
          
         output$plot_Tri1<-renderPlotly({
                                    plotly::plotly_build(p1)
                                    })
         
         p2<-imdb_ratings %>%
           rename("Season"=season_number,"Episode"=season_episode_number,
                  "Votes"=user_votes,"Name"=episode_name) %>%
           subset( Season!=23 & Name!="Unaired Pilot") %>%
           arrange(Season,Episode) %>%
           mutate(Epi=seq_along(Season)) %>%
           ggplot(.,aes(label1=Season,label2=Episode,label3=Name,group=1))+
           geom_point(aes(x=Epi,y=Votes,color="#8B0000"))+
           geom_smooth(aes(x=Epi,y=Votes,color="#000080"))+
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(0,6100,250),labels =seq(0,6100,250))+
           xlab("Episodes")+ylab("Votes")+theme_southpark_sp
         
         output$plot_Tri2<-renderPlotly({
                                  plotly::plotly_build(p2)  
                                  })
         q<-imdb_ratings %>%
           rename("Season"=season_number,
                  "Episode"=season_episode_number,
                  "Name"=episode_name) %>%
           subset( Season!=23 & Name!="Unaired Pilot") %>%
           arrange(Season,Episode) %>%
           unite("Sea_Epi",Season,Episode) %>%
           mutate(Epi=seq_along(Sea_Epi)) %>% 
           select(Name,Sea_Epi,Epi)
         
         p3<-Updatedseason %>%
           arrange(Season,Episode) %>%
           unite("Sea_Epi",Season,Episode,remove=FALSE) %>%
           group_by(Season,Episode) %>%
           count(Sea_Epi) %>%
           rename("Total"=n) %>%
           inner_join(q) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=Total,color="#000080")) + 
           geom_point(aes(x=Epi,y=Total,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(125,450,25),labels =seq(125,450,25))+
           xlab("Episodes")+ylab("Total No of Lines")+theme_southpark_sp
         
         output$plot_Tri3<-renderPlotly({
                                  plotly::plotly_build(p3)  
                                  })
         
         p4<-Updatedseason %>%
           arrange(Season,Episode) %>%
           group_by(Season,Episode) %>%
           count(Character) %>%
           unite("Sea_Epi",Season,Episode,remove = FALSE) %>%
           count(Sea_Epi) %>%
           rename("Total"=n) %>%
           inner_join(q) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=Total,color="#000080")) + 
           geom_point(aes(x=Epi,y=Total,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(0,80,5),labels =seq(0,80,5))+
           xlab("Episodes")+ylab("Total No of Unique Characters")+theme_southpark_sp
         
         output$plot_Tri4<-renderPlotly({
                                  plotly::plotly_build(p4)  
                                  })
         
         p5<-Updatedseason %>%
           arrange(Season,Episode) %>%
           group_by(Season,Episode) %>%
           mutate(Line=as.character(Line)) %>%
           unnest_tokens(word,Line) %>%
           count(word) %>%
           unite("Sea_Epi",Season,Episode,remove = FALSE) %>%
           count(Sea_Epi) %>%
           rename("Total"=n) %>%
           inner_join(q) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=Total,color="#000080")) + 
           geom_point(aes(x=Epi,y=Total,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(550,1050,50),labels =seq(550,1050,50))+
           xlab("Episodes")+ylab("Total No of Unique Words")+theme_southpark_sp
         
         output$plot_Tri5<-renderPlotly({
                                  plotly::plotly_build(p5)  
                                  }) 
         
         q2<-imdb_ratings %>%
           rename("Season"=season_number,
                  "Episode"=season_episode_number,
                  "Name"=episode_name) %>%
           subset( Season!=23 & Name!="Unaired Pilot") %>%
           arrange(Season,Episode) %>%
           unite("Sea_Epi",Season,Episode,remove = FALSE) %>%
           select(Season,Episode,Name,Sea_Epi) %>%
           mutate(Epi=seq_along(Sea_Epi))
         
         p6<-Updatedseason %>%
           mutate(Line=as.character(Line)) %>%
           unnest_tokens(word,Line) %>%
           inner_join(unique(stop_words)) %>%
           group_by(Season,Episode) %>%
           count(word) %>%
           summarise(Total= sum(n)) %>%
           inner_join(q2) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=Total,color="#000080")) + 
           geom_point(aes(x=Epi,y=Total,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(3300,7500,200),labels =seq(3300,7500,200))+
           xlab("Episodes")+ylab("Total No of Stop Words")+theme_southpark_sp
         
         output$plot_Tri6<-renderPlotly({
                                  plotly::plotly_build(p6)  
                                  }) 
         
         
         p7<-Updatedseason %>%
           mutate(Line=as.character(Line)) %>%
           unnest_tokens(word,Line) %>%
           inner_join(unique(tibble(word=swear_words()))) %>%
           group_by(Season,Episode) %>%
           count(word) %>%
           summarise(Total= sum(n)) %>%
           inner_join(q2) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=Total,color="#000080")) + 
           geom_point(aes(x=Epi,y=Total,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(0,175,25),labels =seq(0,175,25))+
           xlab("Episodes")+ylab("Total No of Swear Words")+theme_southpark_sp
         
         output$plot_Tri7<-renderPlotly({
                                  plotly::plotly_build(p7)  
                                  }) 
      
         
         p8<-Updatedseason %>%
           arrange(Season,Episode) %>%
           mutate(Line=as.character(Line)) %>%
           unnest_tokens(word,Line) %>%
           inner_join(get_sentiments("afinn")) %>%
           group_by(Season,Episode) %>%
           count(word,score) %>%
           mutate(sentiment=score * n ) %>%
           summarise(TotalScore= sum(sentiment)) %>%
           unite("Sea_Epi",Season,Episode) %>%
           mutate(Epi=seq_along(Sea_Epi)) %>%
           inner_join(q2) %>%
           ggplot(.,aes(label=Season,label2=Episode,label3=Name,group=1)) +
           geom_smooth(aes(x=Epi,y=TotalScore,color="#000080")) + 
           geom_point(aes(x=Epi,y=TotalScore,color="#8B0000")) +
           theme(legend.position =  "none")+
           scale_x_continuous(breaks=seq(0,300,10),labels =seq(0,300,10))+
           scale_y_continuous(breaks = seq(-675,350,50),labels =seq(-675,350,50))+
           geom_hline(yintercept = 0,color="black",size=.5)+
           xlab("Episodes")+ylab("Sentiment Score")+theme_southpark_sp
         
         output$plot_Tri8<-renderPlotly({
                                  plotly::plotly_build(p8)  
                                  }) 
    }
          )
