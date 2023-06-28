<h2 align = "center">Loan-Default-Prediction</h2>

### Abstract
A project for predicting whether a Loan can be processed to the customer based on his data. An analysis is performed using Random Forests, Logistic Regression, Decision Trees, and Naive Bayes to find which performs the best for the chosen dataset using R language.

### Why Predict the Loan Eligibility?
 * Financial institutions incur significant losses due to default on vehicle loans. This 
has led to the tightening of vehicle loan underwriting and increased vehicle loan 
rejection rates. 
 * The need for a better credit risk scoring model is also raised by these institutions. This 
warrants a study to estimate the determinants of vehicle loan default. 
 * A financial institution has hired you to accurately predict the probability of 
loanee/borrower defaulting on a vehicle loan in the first EMI (Equated to Monthly 
Installments) on the due date.
 * Doing so will ensure that clients capable of repayment are not rejected and important 
determinants can be identified which can be further used for minimizing the default 
rates.

### Implementation

<img src="https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Implementation.png"/>

Initially, the data is cleaned making sure that there are no null values and any other unwanted columns that will not contribute to the results of predicting whether the applicant is eligible for the loan are removed a part of feature selection. To find the most useable columns, the Boruta library is used. The feature selection is as follows:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Feature%20Selection%20using%20Boruta.png" />

The obtained output of the feature selection is as follows:
<img src= "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Output%20of%20Boruta.png" />

Then the data is split into test and train data. Then this training data is used for training the Random Forest, Logistic Regression, Decision Tree, and Naive Bayes. The obtained results are compared with the actual results of the training data for obtaining validation accuracy and error. Then the test data is processed through the trained models and the obtained results are compared with the hidden test data results for obtaining the test accuracy and error. Based on these results Area under the curve and other graphs are obtained. The whole process is repeated for the selected best 20 features, 27 features, and also the whole features or columns present in the dataset. All the results of the models are compared for obtaining the best-performing model for the dataset. 

### Results

All the models are tested over three types of data which have 20, 27, and all features respectively. The below image shows the results of the Random forest with 20 selected features:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Random%20forest%20with%2020%20features.png" />

The below image shows the results of the Random forest with 27 selected features:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Random%20Forest%20with%2027%20features.png"/>

The below image shows the results of the Random forest with all the features:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Random%20Forest.png"/>

After obtaining the results of all the models, the accuracies are as shown below:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/Accuracy%20of%20all%20features.png" />

The comparison of the Area Under the Curve for all the features is as follows: 
<img src= "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/AUC%20of%20all%20features.png"/>

The comparison of the F1 Scores of all the models is as follows:
<img src = "https://github.com/msc-1729/Loan-Default-Prediction/blob/main/assets/F1%20score%20.png" />



