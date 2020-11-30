library(shiny)
library(shinythemes)
library(data.table)
library(plyr)
library(readr)
library(tidyverse)
library(lubridate)
library(naniar)
library(roll)
library(caret)
library(activityCounts)
library(zoo)
library(dplyr)

#############################################################################
####   CODE TO BE RUN OUTSIDE SERVER 
############################################################################


Test_set <- read.csv(url("https://raw.githubusercontent.com/SBeairsto/Avalanche_project/main/Data/Avi_test_data.csv"))

rf_customsearch <- readRDS("rds/rf_customsearch.rds")
rf_confusion <-  readRDS("rds/rf_confusion.rds")
rf_trained <-  readRDS("rds/rf_trained.rds")
rf_feature_imp <- read.csv("feature_imp.csv")

rf_customsearch_ran <- readRDS("rds/rf_customsearch_ran.rds")
rf_confusion_ran <-  readRDS("rds/rf_confusion_ran.rds")
rf_trained_ran <-  readRDS("rds/rf_trained_ran.rds")


svm_gridsearch_ran <- readRDS("rds/svm_gridsearch_rand.rds")
svm_confusion_ran <-  readRDS("rds/svm_confusion_rand.rds")
svm_trained_ran <-  readRDS("rds/svm_trained_rand.rds")

raw_data<- read.csv("raw_dat.csv",header = TRUE)
raw_data$Date <- as.POSIXct(raw_data$Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")


########################################
## Jonathan's Code for Exploring Dataset 

raw_danger_data <- read.csv("https://raw.githubusercontent.com/SBeairsto/Avalanche_project/main/Avi_data_unscaled.csv",header = TRUE)

#Rename variables with descriptive names
names(raw_danger_data)[1] <- "Date"
names(raw_danger_data)[2] <- "Danger"
names(raw_danger_data)[4] <- "Tot_Precip"


#convert data to appropriate format (imported as char)
raw_danger_data <- transform(raw_danger_data, Danger = as.numeric(Danger))
raw_danger_data <- transform(raw_danger_data, Tot_Precip = as.numeric(Tot_Precip))
#raw_danger_data$Date <- as.POSIXct(raw_danger_data$Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")

#  Convert to date if not already
raw_danger_data$Date <- as.Date(raw_danger_data$Date, "%Y-%m-%d")

#  Get months
raw_danger_data$Month <- months(raw_danger_data$Date)


#### Get Average danger rating by month 
danger_month <- aggregate( Danger ~ Month , raw_danger_data , mean )
names(danger_month)[1] <- "Month"
names(danger_month)[2] <- "Danger"
target <- c("November", "December", "January", "February", "March", "April")
danger_month$Month <- factor(danger_month$Month,levels = target)



#### Get Average daily precip by month 
precip_month <- aggregate( Tot_Precip ~ Month , raw_danger_data , mean )
names(precip_month)[1] <- "Month"
names(precip_month)[2] <- "Tot_Precip"
target <- c("November", "December", "January", "February", "March", "April")
precip_month$Month <- factor(precip_month$Month,levels = target)


#### Max wind speed from danger rating 
maxwind_danger <- aggregate( Daily_max_wind ~ Danger , raw_danger_data , mean )
names(maxwind_danger)[1] <- "Danger"
names(maxwind_danger)[2] <- "Max_Wind"



#Get 1,2,3 day precip average by danger rating 
precip_danger <- aggregate( Nos_Daily_tot_precip ~ Danger , raw_danger_data , mean )
names(precip_danger)[1] <- "Danger"
names(precip_danger)[2] <- "Tot_Precip"
precip_danger_prev <- aggregate( Nos_Two_day_tot_precip ~ Danger , raw_danger_data , mean )
names(precip_danger_prev)[1] <- "Danger"
names(precip_danger_prev)[2] <- "Tot_Precip_Prev"
precip_danger_three <- aggregate( Nos_Three_day_tot_precip ~ Danger , raw_danger_data , mean )
names(precip_danger_three)[1] <- "Danger"
names(precip_danger_three)[2] <- "Tot_Precip_Three"

precip_danger <- left_join(precip_danger, precip_danger_prev)
precip_danger <- left_join(precip_danger, precip_danger_three)


#Get change in temperature by danger rating 
raw_danger_data$temp_delta = raw_danger_data$squam_daily_max_temp -  raw_danger_data$squam_daily_min_temp

temp_delta_danger <- aggregate( temp_delta ~ Danger , raw_danger_data , mean )
names(temp_delta_danger)[1] <- "Danger"
names(temp_delta_danger)[2] <- "Tempt_Delta"

#Get max temp by danger rating 
max_temp_danger <- aggregate( squam_daily_max_temp ~ Danger , raw_danger_data , mean )
names(max_temp_danger)[1] <- "Danger"
names(max_temp_danger)[2] <- "Max_tempt"


########################################





############################################################################
###   UI
############################################################################

ui <- 
  
  navbarPage("Sea-to-Sky Avalanches",
             theme = shinytheme("flatly"),
             tabPanel('About',
                      img(src='Cover_Image.png', hight='300', width ='100%', align = "left"),                  
                      
                      fluidRow(
                        shinydashboard::box(width = 6, 
                          p('This project looks at building an avalanche danger predictor using machine learning methods. Currently avalanche danger forecasts are produced around the world as a tool for recreational backcountry users to help in assessing the risk of an avalanche. In Canada most of these forecasts are produced by Avalanche Canada. The cornerstone of these forecasts are a danger rating (Low, Moderate, Considerable, High, Extreme)  at three elevation bands. The reports also include information about likely avalanche problems, as well as detailed information about the snowpack.'),
                          p('Avalanche forecasters are professionals who rely on years of training and experience, reports from the field, a knowledge of the current snowpack and weather forecasts to create these reports. This project creates a model to predict only the danger rating in the ‘Alpine’ elevation band given a range of recent weather data. '),
                          p('There are many factors that affect avalanche danger, some of which are a consequence of recent weather. In general, avalanche are caused by different layers of poorly bonded snow sliding on top of one another. Wet slab avalanche are caused when surface snow warms up and slides on the colder snow beneath. Wind slabs are caused when wind redistributes snow that does not bond well with the snow it has fallen on. Storm slabs are caused when a large amount of fresh snow falls, heavily loading the snow and creating a slide.'),
                          p('Other kinds of avalanches, however, are not caused by immediate weather. Namely, dormant weak layers can be buried under meters of snow only to cause an avalanche weeks later. These persistent weak layers generally require backcountry users to dig in the snow to find, and are typically hard to predict.'),
                          p('Since our model uses only recent weather data, it can only be expected to reflect avalanche danger caused by recent weather. Luckily, on the warmer west coast deep persistent slabs are far less common than in the colder inland regions. Therefore before starting the project we had hope that our model could work. This sentiment was backup by conversations with avalanche professionals.'),
                          ),
                        ), 
                      fluidRow(
                        shinydashboard::box(width = 6, 
                        hr(), 
                        p('This project is the work of Seamus Beairsto and Jonathan Skinnider. The github containing many datasets can
                              be found ', a("Here", href = "https://github.com/SBeairsto/Avalanche_project")),
                        )
                      ),
                      ),
  navbarMenu("Available Data",
        tabPanel("Precipitation",
               fluidRow(
                   shinydashboard::box(width = 6,
                       h1('Precipitation'),
                       p('Precipitation plays an essential role in avalanche danger, with 
                         snowfall playing a particularly important in avalanche danger espcially on the wet west coast.
                         Heavy snow loading can produce snow slabs, and large amounts of rain can weaken the upper snow pack
                         producing wet slabs. 
                         Available measurements of 
                         precipitation in the Sea-to-Sky corridor include cumulative 
                         precipitation (PC), snow water equivalent (SWE), and snow depth
                         (SD). SWE and PC are used to measure precipitation weight and liquid volume respectively, which 
                         do not have a one-to-one ratio with snowfall volume. However, with snow density being 5%-20% that of water, 
                         depending on temperature, these measurements make a relatively good proxy for snowfall volume.'),
                       
                       h3('Snow Water Equivalent'),
                       includeMarkdown("SWE.md"),
                       
                       h3('Cumulative Precipitation'),
                       includeMarkdown("PC.md"),
                       
                       h3('Snow Depth'),
                       includeMarkdown("SD.md"),
                       
                       
                       p('Below we have a plot of the raw precipiation data from the',
                       a(href = 'https://aqrt.nrs.gov.bc.ca/Data/DataSet/Summary/Location/3A25P/DataSet/PC/Telemetry/Interval/AllData',
                         'Squamish River Upper station'), 'provided by the BC government.'),
                 
                       dateRangeInput("daterange", "Date range:",
                                     start  = "2011-11-01",
                                     end    = "2012-05-01",
                                     min    = "2011-01-01",
                                     max    = "2019-09-01",
                                     format = "mm/dd/yy",
                                     separator = " - "),
                      
                       checkboxInput("SWE_box", "SWE" , value = TRUE),
                       checkboxInput("PC_box", "PC" , value = FALSE),
                       checkboxInput("SD_box", "SD" , value = FALSE),
      
        
                      #DTOutput("table"),
                      plotOutput("graph_SWE"),
                      
                      hr(),
       
                      p('*Thanks Tony at the BC Ministry of Environment and Climate Change Strategy 
                        for the help understanding the precipitation measurements.')
                   )
                 )
          ),
 
      tabPanel('Wind', 
          fluidRow(
            shinydashboard::box(width = 6,
                 p("Wind plays a vital role in avalanche danger, particularly in its role in
                 forming wind slabs. Blah, blah, blah, the theory of wind and wind slabs. 
                 Unfortunately, high elevation wind data in the Sea-to-Sky region is hard to 
                 come by. There is now a station on Mt. Cayley (1588m); however, the station's
                 wind data only goes as far back as 2015. There must be historical wind data 
                 from Whistler-Blackcomb, but it does not appear to be publicly available. 
                 The highest elevation station with data dating back to 2011 is the Callaghan
                 Valley station (884m)."),
                 
                 p("The Callaghan Valley data is available from", 
                 a(href = 'https://drive.google.com/drive/folders/1WJCDEU34c60IfOnG4rv5EPZ4IhhW9vZH',
                             'Environment Canada'),
                 "and contains hourly wind speed, which is measured in km/h, as well as wind
                 direction in tens of degrees (0,10,20 degrees etc.). The current hurdle with
                 wind direction is it holds an NA value when wind speed is zero 
                 (which occurs quite frequently). A possible solution would be to bin wind 
                 directions into classes, S, SE, E, etc. and include an NA class for when 
                 wind speed is zero. We would appreciate any input here! For the moment, 
                 however, our model takes into account only the wind speed."),
                 
                 dateRangeInput("wind_daterange", "Date range:",
                                start  = "2011-11-01",
                                end    = "2012-05-01",
                                min    = "2011-01-01",
                                max    = "2019-09-01",
                                format = "mm/dd/yy",
                                separator = " - "),
                 
                 checkboxInput("Raw_wind", "Raw wind speed data" , value = TRUE),
                 checkboxInput("mean_wind", "Daily mean wind" , value = FALSE),
                 checkboxInput("max_wind", "Daily max wind" , value = FALSE),
                 
                 plotOutput("graph_wind")
              )
           )
       ),
 
 tabPanel('Temperature',)
  ),
 tabPanel('Previous Work',
          fluidRow(
            shinydashboard::box(width = 6, 
                                p('This project is a direct continuation of the final project for the University of Victoria\'s
                                  SENG474 class, taught by Dr. Nishant Mehta in spring 2020. The original project was completed by
                                  Joel Kerfoot, Tyler Harnadek, Rowan Burns-Kirkness and Jonathan Skinnider. Click the button bellow to 
                                  download the final report from that project.'),
                                hr(), 
                                sidebarLayout(
                                  sidebarPanel(
                                    actionButton("generate", "Download Report")
                                  ),
                                  
                                  mainPanel(
                                    uiOutput("pdfview")
                                  )
                                ), 
                                hr(), 
                                p('This original project has some relatively promising results, however all the results seemed to indicate
                                  a large amount of overfitting. The project was able to conclude that the Nearest Neighbour classifier 
                                  seemed very unsuitable for this project. Random Forests and SVM both performed pretty well.'),
                                
                                p('The most promissing results seemed to indicate 67.5% accuracy for the SVM and 61% for the Random Forest. 
                                  These results, however, should be taken with a grain of salt because of the extremely small test set of only 5%
                                  that was used. Indeed the biggest take away from the project was the need for more data, because of the high 
                                  varience in performance between cross-validation score when tuning for hyper parameters, and final test 
                                  score.'),
                                hr(),
                                p('A similar project was undertaken by Pozdnoukhov et all in "pplying machine learning methods to avalanche 
                                  forecasting". However they where simply forescasting wheather or not an avalanche would occur given the weather
                                  data. They had success using an SVM, and was used as inspiration for the project mentioned above.'),
                                
                                
                                
                                
                                 ), 
            
            ),
          ),
 tabPanel('Selecting and Cleaning Data',
          fluidRow(
            shinydashboard::box(width = 6, 
                h3("Selecting and Cleaning Data"),
                p("Once we determine what data was available, we have several choices and steps to make before feeding it to our models."),
                p("Our first significant choice is to use the larger Callaghan Valley (884m) wind data set or, the smaller but more accurate Mt. Cayley (1588m) data set. What tips the scales is the high quality of the Mt. Cayley wind direction measurement, which allows us to include it as a feature in our models. As a result, our first step in data cleaning is to cut out any data from before the 2015/16 season (which is when the Mt. Cayley data starts). The Mt. Cayley data is quite complete and does not require any further cleaning. "),
                hr(),
                p("The next choice we make is what precipitation data to use, snow water equivalent (SWE), cumulative precipitation (PC), snow depth (SD), or a combination thereof. Perhaps the best choice would be to emulate the ", a(href = 'https://www.wcc.nrcs.usda.gov/snow/','SNOTEL')," system of our southern neighbours, which interrelates the three measurements, and checks against nearby stations to detect and fill in erroneous measurements. Unfortunately, given the poor quality of some of our PC and SD data, we decide this approach would require more work than we currenlty have time for. We may explore the SNOTEL approach in a later version of this project. Our final decision is to solely use the SWE data to measure precipitation, given its relatively good quality and completeness. "),
                p("The two elements that required cleaning in the SWE data are gaps and erroneous spikes. To fill in gaps (of which none were larger than 24hrs), we take the values of the SWE on either edge of the gap and add linearly increasing hourly data points to connect either edge of the gap."),
                img(src='gap_fill.png', hight='500', width ='450', align = "left"),
                p("In a somewhat similar procedure, we detect erroneous spikes by first measuring the hourly change in SWE. We replace any values greater in magnitude than 50mm/hr by the average hourly change on either side of the spike. Re-integrating the hourly changes over the data set leaves us with spike-less data. "),
                img(src='spike_fill.png', hight='500', width ='450', align = "left"),
                p("The primary element of interest to us in the SWE data is the daily precipitation. We sum all positive values of hourly change in SWE over a 24hr period to get this measurement. For all measurements, we calculate daily min/max/mean/precipitation by considering data from the 24hrs prior to 11pm. We make this choice due to daily weather forecasts playing a significant role in avalanche danger predictions. "),
                hr(),
                p("With our data clean, our last step is to consider that avalanche risk depends on the daily weather as well as the weather over the past few days. There is little science involved in our choice of the time window, but we have to balance giving the models as much relevant data as possible while not creating too many features. We choose to include the previous two day's data along with the daily data. As we did not have any strict reasoning for this choice, we may tweak the time window size in the future. We cut out class 5 danger ratings as there are only two ", em("extreme")," danger rating events in this data window, January 21st, 2016 and January 3rd, 2019. Two points are not enough data to accurately train our classifying models."),
                p("We are left with the following features, where for the precipitation and temperature features there is a feature corresponding to both the Squamish station, with the tag '_squam', and the Nostetuko station, with the tag '_nos.' As we can see, the number of features increases rapidly as we include data from the previous few days."),
                tags$div(tags$ul(
                  tags$li(tags$span("daily precipitation: daily_tot_precip")),
                  tags$li(tags$span("precipitation over the past 2 days: two_day_tot_precip")),
                  tags$li(tags$span("precipitation over the past 3 days: three_day_tot_precip")),
                  tags$li(tags$span("Snow water equivalent: SWE")),
                  tags$li(tags$span("daily max windspeed: wind_daily_max")),
                  tags$li(tags$span("daily average windspeed: wind_daily_avg")),
                  tags$li(tags$span("daily average wind direction: wind_avg_dir")),
                  tags$li(tags$span("daily max temperature: daily_max_temp")),
                  tags$li(tags$span("daily minimum temperature: daily_min_temp")),
                  tags$li(tags$span("daily average temperature: daily_mean_temp")),
                  tags$li(tags$span("max temperature over past 2 days: two_day_max_temp")),
                  tags$li(tags$span("minimum temperature over past 2 days: two_day_min_temp")),
                  tags$li(tags$span("average temperature over past 2 days: two_day_mean_temp")),
                  tags$li(tags$span("max temperature over past 3 days: daily_max_temp")),
                  tags$li(tags$span("minimum temperature over past 3 days: daily_min_temp")),
                  tags$li(tags$span("average temperature over past 3 days: daily_mean_temp")),
                  tags$li(tags$span("month: month "))
                 )),
                p("The data can be downloaded HERE.")
                
            )
          )
          ),
 tabPanel('Exploring the Data Set',
          fluidRow(
            shinydashboard::box(width = 6, 
                                plotOutput("danger_month"),
                                p('Avalanche danger seems to be very consistent throughout the year. Keep in mind that the recorded
                                  avalanche danger is for the "Alpine" elevation band, which is the most affected by all major avalanche
                                  problems throughout the year. That being said, the usual wisdom that springtime is typically safer does 
                                  not seem to be reflected in our dataset'),
                                hr(),
                                plotOutput("precip_month"),
                                p('Our dataset certainly reflects that earlier in the season is when you are more likely to encounter high
                                  daily precipitation. Note that the units for daily precipitation are SWE (snow water equivalency), which 
                                  is not the same as snow depth but is highly correlated.'), 
                                hr(),
                                plotOutput("maxwind_danger"),
                                p('This plot shows the affect of high maximum winds on danger rating. Indeed higher winds produce wind slabs
                                  which in turn increases avalanche danger. Note that the wind speeds on the y-axis are all quite low, the 
                                  reason for which is most likely due to the fact that the Callaghan weather station used for wind is bellow
                                  treeline.'),
                                hr(),
                                plotOutput("precip_danger"),
                                p('Again, higher daily precip (over the previous one to three days) are correlated with higher avalanche 
                                  danger. What is curious, however, is the fact that the difference between a low rating (1) and moderate 
                                  rating (2) is essentially zero. It would make sense that storm slabs are not a significant contributor 
                                  to avlanche danger when it is relatively safe out. It would seem that if there is enough snow to form 
                                  storm slabs, the danger is typically quite high'),
                                hr(),
                                plotOutput("temp_delta_danger"),
                                p('This is one of the most curious charts. It implies that a lower danger rating is assosiated with a larger
                                  change in temperature over the course of a day. There does not seem to be a clear avalanche factor which explains
                                  why this correlation exists.'),
                                hr(),
                                plotOutput("max_temp_danger"),
                                p('This chart is similarly curious, however a reasonable explanation can be provided. It would make sense that
                                  low danger days typically arise from spring like conditions. From personal expirience I know that when warm fronts
                                  move through the west coast, avalanche danger can rapidly stabilize after a freeze/thaw cycle. Most likely having 
                                  a "Low" danger rating for the alpine usually happens at these times.'),
                                hr(),
                                )
          )
         
          
          ),
 
 navbarMenu("Standard Machine Learning Methods",
      
      tabPanel('Support Vector Machine',
              fluidRow(
                shinydashboard::box(width = 6,    
                  h1("Support Vector Machine"),
                  h3("Theory"),
                  p("Support vector machines (SVMs) are machine learning models primarily used in classification problems, and in some instances, in regression problems (but we won't worry about how regression works). In essence, an SVM is a binary classifier, and the idea is to find a hyperplane in an N-dimensional space that best separates the two classes, where N is the number of features. For example, if we only have two features, we deal with a two-dimensional space, and the hyperplane is a line separating the two classes. In three-dimensions, the hyperplane is a plane. We also refer to the hyperplane as the ", strong("decision boundary"),"."),
                  img(src='SVM_linear.png', hight='700', width ='550', align = "left"),
                  p("The SVM finds the optimal hyperplane by maximizing margins around the hyperplane. Building the margins using every available piece of data would be insanely computationally heavy and would offer little reward. The standard procedure uses a few of the closest data points from each group, a.k.a, the ", strong("support vectors"),", to build and maximize the margins.  This method's weakness is that it is very susceptible to outliers, which will wreak havoc on the margins and hyperplane. The solution is to include a hyperparameter ", em("C")," in the optimization function, which tunes outliers' effect on the margins. As every data set and problem is different, it is necessary to experimentally find ", em("C"),"'s value that best suits your problem through hyperparameter tuning."),
                  hr(),
                  p("The question then arises, what if the data is not linearly separable? The standard solution is to project the data into a higher dimension where it ", em("is")," linearly separable."),
                  img(src='SVM_nl1.png', hight='700', width ='550', align = "left"),
                  p("For example, if we have two-dimensional data that can be separated by a circular line, we project the data onto a sphere, where we can then split the data with a plane."),
                  img(src='SVM_nl2.png', hight='700', width ='550', align = "left"),
                  p("Projecting back onto two-dimensions, we are left with a circular decision boundary. We refer to this method as the ", strong("kernel trick.")," We implement a ", em("radial basis function")," as our kernel, a generally well-performing kernel that we tune with the hyperparameter ", em("gamma."),""),
                  img(src='SVM_nl3.png', hight='700', width ='550', align = "left"),
                  hr(),
                  p("So, we have a machine that can separate data with non-linear decision boundaries, but it can only separate ", em("binary")," data, i.e, data with only two classes. Our data has four classes. The method implemented by SVMs for multi-class classification is called one-vs.-one classification. Here the data is split into binary datasets for each class vs. every other class. In other words, the SVM divides our data into six datasets, 1 vs. 2, 1 vs. 3, 1 vs. 4, 2 vs. 3, 2 vs. 4, and 3 vs. 4. A separate SVM is then trained on each dataset, and to classify new data, each sub-SVM votes, and the class with the most votes becomes the final classification."),
                  hr(),
                  h3("Our Model"),
                  p("As mentioned, the two hyperparameters used in tuning our SVM are ", em("C")," and ", em("gamma"),". Using a grid-search method with 5-fold cross-validation, we find ", em("C")," and ", em("gamma"),"'s optimized values to be 3.5 and 0.025, respectively, achieving an accuracy of 69%. (Note that the the plot below not is of accuracy, but of percent error, i.e. 1-accuracy.)"),
                  plotOutput("svm_gridsearch_ran"),
                  hr(),
                  p("From the confusion matrix below, we see the SVM achieves an accuracy of 66% on the test data. We see that the SVM over-predicts class 1 and 2 danger ratings, while slightly under-predicting class 3 and 4 danger ratings."),
                  verbatimTextOutput("svm_confusion_ran"),
                  )
               )
              ),
      
      tabPanel('Random Forest',
              fluidRow(
                shinydashboard::box(width = 6,
                    h1("Random Forest Classifier"),
                    h3("Theory"),
                    p("Random forest models are popular machine learning algorithms, capable of both classification and regression. Their popularity arises from their ease of use, general high performance, and intuitive design. In essence, random forest models are a collection of decision trees, each trained on a random sample of the training data, who collectively vote on the model's output.  The use of many (minimally correlated) trees minimizes the pitfalls of decision tree models. So what is a decision tree, and why is it not good enough on its own?"),
                    p(" A ", strong("decision tree")," algorithm is based around at each step splitting the data into two groups that are as different from each other as possible. The tree splits the data by first looking through all features and determining which will best split the data. The tree then chooses a decision boundary within that feature. For example, the tree may find that the", em("daily precipitation"),"can best divide the danger ratings, with 1s and 2s in one group and 3s and 4s in the other. It then determines that the best decision boundary is at 5mm. The data is then split, with any data with ", em("daily precipitation")," < 5mm going into one branch and data with ", em("daily precipitation")," > 5mm going into another. This procedure is repeated on each branch, selecting the best feature on each branch, and splitting the data again. The process stops when either the data has been successfully divided into pure groups or the model reaches a preset minimum number of data points per branch. This process results in a tree-like structure similar to the one shown below."),
                    hr(),
                    img(src='Decision_Tree.png', hight='700', width ='550', align = "left"),
                    hr(),
                    p("The drawback to decision trees is that the models are highly sensitive to the dataset used in training. Slightly different datasets may result in vastly different models. This variability is not a desirable trait if we are looking for robust predictions."),
                    p("This is where the random forest approach comes in. Many trees, each trained on slightly different data operating as a committee, will out-perform any individual tree. Two techniques are implemented to minimize the correlation between trees in the forest. First, using a method called ", strong("bagging"),", each tree is trained on a random sample of the training data, with each element chosen with replacement. Second, implementing ", strong("feature randomness"),", each tree is only allowed to use a random subset of features when splitting the data. For predictions, the data is run through each tree and they vote on how to classify the data. Together these methods produce a significantly more robust predictor than a single decision tree."),
                    hr(),
                    h3("Our Model"),
                    p("The two primary hyperparameters in the R", em("caret "),"implementation of a random forest are ",em("ntree"),", which is the number of trees to grow, and ",em("mtry"),", which is the number of random features (also referred to as predictors) each tree has access to. In the figure below we apply a grid search to find the optimized values for", em("mtry")," and ",em("ntree")," which are 19 and 2000 respectively, achieving an accuracy of 69%."), 
                    hr(),
                    plotOutput("rf_customsearch_ran"),
                    hr(),
                    p("We then use our optimized model to predict the danger forecast for our test set. As we can see in the confusion matrix below, our RF classifier preformes slighty better than training suggested, with an accuracy of 71%, which is 1% lower than the SVM. We can see that the model overpredicts class 1 and 2 danger ratings, and underpredicted class 3 and 4 danger ratings."),
                    verbatimTextOutput("rf_confusion_ran"),
                    hr(),
                    p("A final bit of information we can extract from our RF model is feature importance, as the RF model needs to determine feature importance to do its splitting. The feature importances are given in the table below."),
                    dataTableOutput("rf_importance"),
                    p("Not surprisingly, we see the most important features are the precipitation values.")
                )
            )
        )
     ),
 
 tabPanel('Ordinal Random Forest')
)





############################################################################
###   SERVER
############################################################################

server <- function(input, output){
  
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="SENG474_Final_Project.pdf")
    })
  })
  
  output$danger_month <- renderPlot({
    ggplot(data=danger_month, aes(x=as.factor(Month), y=Danger, group=1)) +
      geom_line(linetype = "dashed")+
      ggtitle("Average Avalanche Danger vs Month")+
      xlab("Month") + ylab("Average Danger Rating") +
      geom_point()
  })
  
  output$precip_month <- renderPlot({
    ggplot(data=precip_month, aes(x=as.factor(Month), y=Tot_Precip, group=1)) +
      geom_line(linetype = "dashed")+
      ggtitle("Average Daily Precipitation vs Month")+
      xlab("Month") + ylab("Average Daily Precip (SWE)") +
      geom_point()
    
  })
  
  output$maxwind_danger <- renderPlot({
    ggplot(data=maxwind_danger, aes(x=Danger, y=Max_Wind, group=1)) +
      geom_line(linetype = "dashed")+
      ggtitle("Average Maximum Same Day Daily Wind vs Avalanche Danger")+
      xlab("Danger Rating") + ylab("Max Wind (km/h)") +
      geom_point()
  })
  
  output$precip_danger <- renderPlot({
    ggplot(precip_danger, aes(Danger)) + 
      # geom_line(aes(y = Tot_Precip, colour = "Precipitation")) +
      ggtitle("Average Multi Day Precipitation vs Avalanche Danger")+
      xlab("Danger Rating") + ylab("Daily Precip (SWE)") +
      geom_line(aes(y = Tot_Precip, colour = "One Day Precipitation")) +
      geom_line(aes(y = Tot_Precip_Prev, colour = "Two Day Precipitation")) +
      geom_line(aes(y = Tot_Precip_Three, colour = "Three Day Precipitation"))
    
  })
  output$temp_delta_danger <- renderPlot({
    ggplot(data=temp_delta_danger, aes(x=Danger, y=Tempt_Delta, group=1)) +
      geom_line(linetype = "dashed")+
      ggtitle("Change in Temperature Over One Day vs Avalanche Danger")+
      xlab("Danger Rating") + ylab("Change in Temperature (C)") +
      geom_point()
  })
  
 output$max_temp_danger <- renderPlot({
    ggplot(data=max_temp_danger, aes(x=Danger, y=Max_tempt, group=1)) +
      geom_line(linetype = "dashed")+
     ggtitle("Maximum Daily Temperature vs Avalanche Danger")+
     xlab("Danger Rating") + ylab("Maximum Temperature (C)") +
      geom_point()
   })
 
 output$rf_customsearch_ran <- renderPlot({
   ggplot(rf_customsearch_ran) +
     ggtitle("Random forest hyperparameter tuning")
 })
 
 output$rf_confusion_ran <- renderPrint({rf_confusion_ran})
 output$rf_importance <- renderDataTable({rf_feature_imp})
 
 output$svm_gridsearch_ran <- renderPlot({
   plot(svm_gridsearch_ran)
 })
 
 output$svm_confusion_ran <- renderPrint({svm_confusion_ran})
  
  
  #
  data_table <- reactive({ raw_data[raw_data$Date >= as.POSIXct(input$daterange[1])
                                    & raw_data$Date <= as.POSIXct(input$daterange[2]),]})
  
  output$graph_SWE <- renderPlot({
    if(input$SWE_box & input$PC_box & input$SD_box){
      ggplot(data_table(), aes(Date)) + 
        geom_line(aes(y = PC, colour = "PC")) +
        geom_line(aes(y = SWE, colour = "SWE"))+
        geom_line(aes(y = SD, colour = "SD"))} 
    else if(input$SWE_box & input$PC_box & !input$SD_box){
      ggplot(data_table(), aes(Date)) + 
        geom_line(aes(y = PC, colour = "PC")) +
        geom_line(aes(y = SWE, colour = "SWE"))}
        #geom_line(aes(y = SD, colour = "SD")) 
    else if(input$SWE_box & !input$PC_box & !input$SD_box){
        ggplot(data_table(), aes(Date)) + 
          #geom_line(aes(y = PC, colour = "PC")) +
          geom_line(aes(y = SWE, colour = "SWE"))}
    else if(input$SWE_box & !input$PC_box & input$SD_box){
      ggplot(data_table(), aes(Date)) + 
        #geom_line(aes(y = PC, colour = "PC")) +
        geom_line(aes(y = SWE, colour = "SWE"))+
        geom_line(aes(y = SD, colour = "SD"))}  
    else if(!input$SWE_box & input$PC_box & input$SD_box){
      ggplot(data_table(), aes(Date)) + 
        geom_line(aes(y = PC, colour = "PC")) +
        #geom_line(aes(y = SWE, colour = "SWE"))}+
        geom_line(aes(y = SD, colour = "SD")) }
    else if(!input$SWE_box & input$PC_box & !input$SD_box){
        ggplot(data_table(), aes(Date)) + 
          geom_line(aes(y = PC, colour = "PC")) }
          #geom_line(aes(y = SWE, colour = "SWE"))}+
          #geom_line(aes(y = SD, colour = "SD")) }
    else if(!input$SWE_box & !input$PC_box & input$SD_box){
        ggplot(data_table(), aes(Date)) + 
          #geom_line(aes(y = PC, colour = "PC")) }
          #geom_line(aes(y = SWE, colour = "SWE"))}+
        geom_line(aes(y = SD, colour = "SD")) }
    else{ggplot(data_table(), aes(Date))}
  })
  
  #
  wind_data_table <- reactive({ wind_data[wind_data$Date >= as.POSIXct(input$wind_daterange[1])
                                    & wind_data$Date <= as.POSIXct(input$wind_daterange[2]),]})
  
  output$graph_wind <- renderPlot({
    if(input$mean_wind & input$raw_wind & input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        geom_line(aes(y = Wind.Spd..km.h, colour = "PC")) +
        geom_line(aes(y = Daily_mean_wind, colour = "SWE"))+
        geom_line(aes(y = Daily_max_wind, colour = "SD"))} 
    else if(input$mean_wind & input$raw_wind & !input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        geom_line(aes(y = Wind.Spd..km.h, colour = "PC")) +
        geom_line(aes(y = Daily_mean_wind, colour = "SWE"))}
    #geom_line(aes(y = SD, colour = "SD")) 
    else if(input$mean_wind & !input$raw_wind & !input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        #geom_line(aes(y = PC, colour = "PC")) +
        geom_line(aes(y = Daily_mean_wind, colour = "SWE"))}
    else if(input$mean_wind & !input$raw_wind & input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        #geom_line(aes(y = PC, colour = "PC")) +
        geom_line(aes(y = Daily_mean_wind, colour = "SWE"))+
        geom_line(aes(y = Daily_max_wind, colour = "SD"))}  
    else if(!input$mean_wind & input$raw_wind & input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        geom_line(aes(y = Wind.Spd..km.h, colour = "PC")) +
        #geom_line(aes(y = SWE, colour = "SWE"))}+
        geom_line(aes(y = Daily_max_wind, colour = "SD")) }
    else if(!input$mean_wind & input$raw_wind & !input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        geom_line(aes(y = Wind.Spd..km.h, colour = "PC")) }
    #geom_line(aes(y = SWE, colour = "SWE"))}+
    #geom_line(aes(y = SD, colour = "SD")) }
    else if(!input$mean_wind & !input$raw_wind & input$max_wind){
      ggplot(wind_data_table(), aes(Date)) + 
        #geom_line(aes(y = PC, colour = "PC")) }
        #geom_line(aes(y = SWE, colour = "SWE"))}+
        geom_line(aes(y = Daily_max_wind, colour = "SD")) }
    else{ggplot(data_table(), aes(Date))}
  })

  
}

############################################################################

shinyApp(ui = ui, server = server)