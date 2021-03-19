# Census_income
This project aims on analyzing and "cleaning" the Census Income data set in order to fit the modified data into various models, where the main task is to classify whether the yearly income is higher or lower than 50K. This includes descriptive, predictive and prescriptive analysis presented in a form of RMarkdown document(which also comes with a compiled .html version), where the code is fully written in R. There's also a dynamic RShiny documentation which allows the user to input their own data and predict the outcome based on the model which has performed best.  
## RMarkdown contents
This documentation contains all of the analysis in order as follows:
* 1. Introduction
* 2. Data Understanding
* 3. Data Preparation
* 4. Modeling
* 5. Association Rules
* 6. Final Rating
* 7. Conclusion
## RShiny contents
This dashboard was created using the [shinydashboard](https://rstudio.github.io/shinydashboard/) package. This documentation contains several interactive tabs, such as:
* Overall info
* Categorical Graphs
* Numeric Graphs
* Click Graphs
* Insane Graphs
* Predict your income with Random Forest
* Extra
## Relevant info
The main idea was to try out a lot of different models in order to find the best performing one. Although, only accuracy metric was monitored during this research. All models were created using 80%/20%, 70%/30%, 60%/40% train/test splits in order to compare their performance. Full list of used models:
* Logistic regression
* Classification and Regression Tree(CART)
* Random Forest
* Naive Bayes Classification
* K-nearest Neighbours(KNN)
* Classification Based On Association Rules Algorithm(CBA)
## Source
This data set is available freely on the official [UCI Machine Learning](https://archive.ics.uci.edu/ml/datasets/Census+Income) page.
