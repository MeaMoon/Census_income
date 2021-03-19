library(shinydashboard)
library(randomForest)
library(ggplot2)
library(ROCR)
library(shiny)
library(arules)
library(igraph)
library(maptree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(partykit)
library(class)
library(rpart)
library(graphics)
library(gplots)
library(plyr)
library(dplyr)
library(ROCR)
library(expss)
library(caret)
library(rpart)
library(e1071)


dataseturl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data' 
datanameurl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names' 
census = read.table(dataseturl,sep = ',', fill = F, strip.white = T)
censusnames = readLines(datanameurl)[97:110]
censusnames = as.character(lapply(strsplit(censusnames,':'), function(x) x[1])) 
censusnames = c(censusnames,'income')
colnames(census) = censusnames 
names(census) = make.names(names(census))
census$education = NULL
census$fnlwgt = NULL
census$workclass = gsub('^Federal-gov', 'Government', census$workclass)
census$workclass = gsub('^Local-gov', 'Government', census$workclass)
census$workclass = gsub('^State-gov', 'Government', census$workclass) 
census$workclass = gsub('^Self-emp-inc', 'Self-Employed', census$workclass)
census$workclass = gsub('^Self-emp-not-inc', 'Self-Employed', census$workclass)
census$workclass = gsub('^Never-worked', 'Other', census$workclass)
census$workclass = gsub('^Without-pay', 'Other', census$workclass)
census$workclass = gsub('^Other', 'Other', census$workclass)
census$workclass = gsub('^Unknown', 'Other', census$workclass)
census$workclass = as.factor(census$workclass)
levels(census$workclass)[1] = 'Unknown'
census$workclass = gsub('Unknown', 'Other', census$workclass)
census$workclass = as.factor(census$workclass)
census$occupation = gsub('Adm-clerical', 'White-Collar', census$occupation)
census$occupation = gsub('Craft-repair', 'Blue-Collar', census$occupation)
census$occupation = gsub('Exec-managerial', 'White-Collar', census$occupation)
census$occupation = gsub('Farming-fishing', 'Blue-Collar', census$occupation)
census$occupation = gsub('Handlers-cleaners', 'Blue-Collar', census$occupation)
census$occupation = gsub('Machine-op-inspct', 'Blue-Collar', census$occupation)
census$occupation = gsub('Other-service', 'Service', census$occupation)
census$occupation = gsub('Priv-house-serv', 'Service', census$occupation)
census$occupation = gsub('Prof-specialty', 'Professional', census$occupation)
census$occupation = gsub('Protective-serv', 'Service', census$occupation)
census$occupation = gsub('Tech-support', 'Service', census$occupation)
census$occupation = gsub('Transport-moving', 'Blue-Collar', census$occupation)
census$occupation = gsub('Unknown', 'Other', census$occupation)
census$occupation = gsub('Armed-Forces', 'Other', census$occupation)
census$occupation = as.factor(census$occupation)
levels(census$occupation)[1] = 'Unknown'
census$occupation = gsub('Unknown', 'Other', census$occupation)
census$occupation = as.factor(census$occupation)
census$marital.status = gsub('Married-AF-spouse', 'Married', census$marital.status)
census$marital.status = gsub('Married-civ-spouse', 'Married', census$marital.status)
census$marital.status = gsub('Married-spouse-absent', 'Married', census$marital.status)
census$marital.status = gsub('Never-married', 'Single', census$marital.status)
census$marital.status = as.factor(census$marital.status)
census$race = gsub('Amer-Indian-Eskimo', 'Other', census$race)
census$race = gsub('Asian-Pac-Islander', 'Other', census$race)
census$race = as.factor(census$race)
census$capital.change = census$capital.gain - census$capital.loss
census$capital.gain = NULL
census$capital.loss = NULL
census = census[c(1,2,3,4,5,6,7,8,9,10,12,11)]
census$native.country = NULL
census$relationship = NULL
census$sex = as.factor(census$sex)
census$income = as.factor(census$income)

ui =  dashboardPage(skin = "green",
                    dashboardHeader(title = "Census Income", titleWidth=300),
                    dashboardSidebar(width = 300, 
                                     sidebarMenu(
                                         menuItem("Overall info", tabName = "info"),
                                         menuItem("Categorical Graphs", tabName = "categ"),
                                         menuItem("Numeric Graphs", tabName = "graphs"),
                                         menuItem("Click Graphs", tabName = "point"),
                                         menuItem("Insane Graphs", tabName = "else"),
                                         menuItem("Predict your income with Random Forest", tabName = "logic"),
                                         menuItem("Extra", tabName = "cool")
                                     )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(
                                tabName = "categ",
                                fluidRow(
                                    box(
                                        plotOutput("yep", height = 300), status = "primary"
                                    ),
                                    box(
                                        plotOutput("bar", height = 300), status = "success"
                                    ),
                                    box(
                                        selectInput("choice", "Select a category:", c("Workclass" = "workclass",
                                                                                      "Marital Status" = "marital.status",
                                                                                      "Occupation" = "occupation",
                                                                                      "Race" = "race",
                                                                                      "Sex" = "sex"),
                                                    selected = "Workclass"
                                        )
                                    ),
                                    box(
                                        title = "Hint",
                                        h4("Try selecting different categories to view their relations between the income attribute."),
                                        solidHeader = TRUE,
                                        background = "olive"
                                    )
                                    
                                )
                            ),
                            tabItem(tabName = "graphs",
                                    fluidRow(
                                        column(width = 4,
                                               box(
                                                   plotOutput("age", height = 300), status = "primary", width = NULL),
                                               box(
                                                   title = "Change the input age", 
                                                   sliderInput("slider_1", "Age", 17, 90, 25),
                                                   width = NULL,
                                                   status = "primary")
                                        ),
                                        column(width = 4,
                                               box(
                                                   plotOutput("edu", height = 300), status = "warning", width = NULL
                                               ),
                                               box(
                                                   title = "Change the education",
                                                   sliderInput("slider_2", "Education", 1, 16, 8),
                                                   width = NULL,
                                                   status = "warning") 
                                        ),
                                        column(width = 4,
                                               box(
                                                   plotOutput("hpw", height = 300), status = "info", width = NULL
                                               ),
                                               box(
                                                   title = "Hours per week",
                                                   sliderInput("slider_3", "Hours", 1, 99, 20),
                                                   width = NULL,
                                                   status = "info"
                                               )
                                        )
                                        
                                    )),
                            tabItem(tabName = "info",
                                    fluidRow(
                                        box(
                                            title = "Understanding the Data", height = 300, background = "olive", solidHeader = TRUE,
                                            h4("Census Income is a multivariate data set, which consists out of categorical and numeriical data. The main idea is to classify whether the income exceeds above 50k/year or not."),
                                            
                                            h4("The data shown here are preprocessed the same way as in the RMarkdown documentation")
                                        ),
                                        box(
                                            title = "Why Census Income?", height = 300, background = "olive", solidHeader = TRUE,
                                            h4("This data set was chosen for a student project. The main reason why is because it can represent how well can 'tree' type models classify the last attribute(income).")
                                        )
                                        
                                    ),
                                    fluidRow(
                                        box(
                                            title = "RShiny App", height = 300, background = "olive", solidHeader = TRUE,
                                            h4("This App will describe the data set in more interactive way, than RMarkdown analog. Each tab consists of various plots, which are divided by groups. Every attribute is described separately.")
                                        ),
                                        box(
                                            title = "Random Forest Classifier", height = 300, background = "olive", solidHeader = TRUE,
                                            h4("In RMarkdown documentation it was visible that Random Forest was the most suitable classifier. To prove how well can it classify the income attribute, the last tab will predict the user input data by selecting every possible value, that this data set has.")
                                        )
                                        
                                    )
                            ),
                            tabItem(
                                tabName = "point",
                                fluidRow(
                                    box(
                                        plotOutput(
                                            "bruh", click = "bruh_click"), status = "danger"
                                    ),
                                    box(
                                        br(), br(), br(),
                                        htmlOutput("x_value"),
                                        verbatimTextOutput("selected_rows"),
                                        box(
                                            selectInput("selected", "Change the category: ", c("Workclass" = "workclass",
                                                                                               "Marital Status" = "marital.status",
                                                                                               "Occupation" = "occupation",
                                                                                               "Race" = "race",
                                                                                               "Sex" = "sex"),
                                                        selected = "Workclass"
                                            )
                                        ),
                                        box(
                                            title = "Hint", background = "olive", solidHeader = TRUE,
                                            h4("Click on the single graph columns to view the rows, which match their category.")
                                        )
                                        
                                    )
                                )
                            ),
                            tabItem(
                                tabName = "else",
                                fluidRow(
                                    box(
                                        plotOutput("omg",
                                                   hover = hoverOpts(id = "omg_hover"),
                                                   brush = brushOpts(id = "omg_brush"))
                                    ),
                                    box(
                                        verbatimTextOutput("hover_info"), width = 3
                                    ),
                                    box(
                                        verbatimTextOutput("brush_info"), width = 3
                                    ),
                                    box(
                                        title = "Hint", background = "olive", solidHeader = TRUE,
                                        h4("Hold and drag with your left mouse button to create a custom field to view. Or just hover on the dots.")
                                    )
                                )
                            ),
                            tabItem(
                                tabName = "logic",
                                fluidRow(
                                    box(
                                        sliderInput("zero", "Train/Test split", min = 0.6, max = 0.9, value = 0.7, step = 0.1),
                                        sliderInput("one","Age",min=17,max = 90, value = 19, step = 1),
                                        selectInput("two","Workclass",choices = c("Government", "Private", "Self-Employed", "Other"), selected = "Goverment"),
                                        sliderInput("three","Education",min = 1, max = 16, value = 10, step = 1),
                                        selectInput("four","Marital Status", choices = c("Divorced","Married","Separated","Single","Widowed"), selected = "Single"),
                                        selectInput("five", "Occupation", choices = c("Blue-Collar","White-Collar","Service", "Sales", "Professional", "Other"), selected = "Professional"),
                                        selectInput("six", "Race", choices = c("White", "Black", 'Other'), selected = "White"),
                                        selectInput("seven", "Sex", choices = c("Male", "Female"), selected = "Male"),
                                        sliderInput("eight", "Hours per Week",min = 1, max = 99, value = 33, step = 1),
                                        sliderInput("nine", "Capital Change", min = 1, max = 99999, value = 1488, step = 20),
                                        actionButton("run_this", "Initiate"),
                                        actionButton("lmao", "Auto")
                                    ),
                                    box(
                                        title= "Hint", background = "olive", solidHeader = TRUE,
                                        h4("Predict your own income by choosing the input attribute values. Click on Initiate first to create the test data frame. To view the dataset structure and make predictions update automatically, click on Auto.")
                                    ),
                                    infoBoxOutput("accuracy_output"),
                                    box(
                                        verbatimTextOutput("result"),
                                        title = "Your predicted result", solidHeader = TRUE, status = "success"
                                    ),
                                    box(
                                        verbatimTextOutput("proof"),
                                        title = "Your input structure",solidHeader = TRUE, status = "success"
                                    )
                                    
                                )
                            ),
                            tabItem(
                                tabName = "cool",
                                fluidRow(
                                    box(
                                        plotOutput("logically")
                                    ),
                                    box(
                                        checkboxGroupInput("puttt", "Complicate your model by choosing multiple attributes", choices = c("age", "workclass", "education.num", "marital.status", "occupation", "race", "sex", "hours.per.week", "capital.change"), 
                                                           selected = c("workclass","education.num","marital.status","occupation"))
                                    )
                                    
                                )
                            )
                        )
                    )
)



server = function(input, output){
    
    studyModel = reactive({
        rpart(as.formula(paste("income ~ ",paste0(input$puttt,collapse="+"))),data = census, method = 'class', cp = 1e-3)
    })
    
    output$logically = renderPlot({
        prp(studyModel(),varlen=8, cex = 0.7)
    })
    
    output$age = renderPlot({
        data = census$age[seq_len(input$slider_1)]
        hist(data, main = "Age", col = "#0000FF")
    })
    
    output$edu = renderPlot({
        data = census$education.num[seq_len(input$slider_2)]
        hist(data, main = "Education", col = "orange")
    })
    
    output$hpw = renderPlot({
        data = census$hours.per.week[seq_len(input$slider_3)]
        hist(data, main = "Hours per Week", col = "cyan")
    })
    
    
    output$yep = renderPlot({
        qplot(income, data = census, fill = census[,input$choice]) + facet_grid (. ~ census[,input$choice])
    })
    
    output$bar = renderPlot({
        barplot(table(census[,input$choice]),main=input$choice,col = 'orange')
    })
    
    output$bruh = renderPlot({
        plot(census[,input$selected],census$age)
    })
    
    output$x_value = renderText({
        if (is.null(input$bruh_click$x)) return("")
        else {
            lvls = levels(census[,input$selected])
            name = lvls[round(input$bruh_click$x)]
            HTML("You've selected <code>", name, "</code>",
                 "<br><br>Here are the first 10 rows that ",
                 "match that category:")
        }
    })
    
    
    output$selected_rows = renderPrint({
        if (is.null(input$bruh_click$x)) return()
        else {
            keeprows = round(input$bruh_click$x) == as.numeric(census[,input$selected])
            head(census[keeprows, ], 10)
        }
    })
    
    output$omg = renderPlot({
        ggplot(census, aes(age, hours.per.week)) + geom_point()
        
    })
    
    output$brush_info = renderPrint({
        cat("input$omg_brush:\n")
        str(input$omg_brush)
    })
    
    
    output$hover_info = renderPrint({
        cat("input$omg_hover:\n")
        str(input$omg_hover)
    })
    
    set.seed(322)
    
    
    observe({ 
        
        hope = input$zero
        
        size_this = round(hope * dim(census)[1])
        census_tr = census[1:size_this,]
        census_ts = census[-(1:size_this),]
        
        model = randomForest(income ~ .,  data = census_tr, ntree=100)
        
        
        age = as.integer(input$one)
        workclass = as.factor(input$two)
        education.num = as.integer(input$three)
        marital.status = as.factor(input$four)
        occupation = as.factor(input$five)
        race = as.factor(input$six)
        sex = as.factor(input$seven)
        hours.per.week = as.integer(input$eight)
        capital.change = as.integer(input$nine)
        
        test = data.frame(age,workclass,education.num,marital.status,occupation,race,sex,hours.per.week,capital.change)
        test$income = NA
        
        
        levels(test[,2]) = levels(census_tr[,2])
        levels(test[,4]) = levels(census_tr[,4])
        levels(test[,5]) = levels(census_tr[,5])
        levels(test[,6]) = levels(census_tr[,6])
        levels(test[,7]) = levels(census_tr[,7])
        levels(test[,10]) = levels(census_tr[,10])
        
        
        
        observeEvent(input$run_this, {
            pred_k =  predict(model, newdata = test, type = 'class')
            output$result = renderPrint(pred_k[1])
        })
        
        
        observeEvent(input$lmao,{
            output$proof = renderPrint(str(test))
        }
        
        )
        
    })
    
    observe({
        
        pray = input$zero
        
        size_this_b = round(pray * dim(census)[1])
        census_tr_b = census[1:size_this_b,]
        census_ts_b = census[-(1:size_this_b),]
        
        model_b = randomForest(income ~ .,  data = census_tr_b, ntree=100)
        
        pred_b =  predict(model_b, newdata = census_ts_b, type = 'class')
        mat_b = confusionMatrix(pred_b, census_ts_b$income)
        accuracy_b = mat_b$overall['Accuracy']
        output$accuracy_output  = renderInfoBox({
            infoBox("Accuracy of the generated model",  paste0(accuracy_b, " %"),
                    icon = icon("ok", lib = "glyphicon"),
                    color = "green")
        })
    })   
   
    
}

shinyApp(ui = ui, server = server)
