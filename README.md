# SAT-score-prediction
#### Executive summary
SAT Exam is the most important examination for college admissions in USA. Here we aim to analyse the factors influencing the SAT score, starting from expenditure per pupil to student participation rate. We approached the problem with one Bayesian regression model along with noninformative priors and another along with informative priors. We found that both the models showed good convergence but the second model performed slightly better than the first one according to the model comparison method, namely deviance information criterion. The residuals were +-10% of the mean SAT scores and did not show any specific pattern. We found salary to be highest positive influencer for SAT score whereas number of test takers to be highest negative influencer. Current expenditure showed only 70% chance that it positively impacts SAT scores.

#### Introduction
Our objective is to identify external factors responsible for the students’ performance in SAT examination. Here we are going to address such independent factors, their correlation with the SAT score. We will address the following issues for our analysis:
\
• Whether higher current expenditure leads to better students’ performance
\
• SAT score variation due to change in pupil/teacher ratio
\
• Whether salary increment of teachers causes increase in SAT scores
\
• Influence of student participation rate on individual test scores
\
• Most influential explanatory variable
\
• Comparison of baseline model (with non-informative priors) and final model (with
informative priors)

#### Data
The popular dataset is taken from the paper **‘Getting What You Pay For: The Debate over Equity in Public School Expenditures’**. It contains data that describes the per-pupil expenditure on education and the education outcomes (SAT scores) in various states from 1994–95. 

#### Results
We have observed from the posterior distribution that there is only 69% chance that
there is a positive correlation between current expenditure and total SAT score.
\
• There is a 94% chance that increase in pupil/teacher ratio will enhance individual
student SAT score implying that more students in a class will benefit them for SAT.
\
• There is a 96% chance of a positive correlation between teacher’s salary and total
SAT score. Increment in salary will give rise to quality teaching, thus preparing
students for better performance in examination.
\
• There is a 100% chance that greater student participation rate in SAT examination
will degrade individual student SAT score.
\
• Among the explanatory variables considered, salary has highest positive correlation
and number of test takers has highest negative correlation against SAT scores.
\
• Top 5 countries in the dataset showing highest residuals are - North Dakota, New
Hampshire, Iowa, Massachusetts, Utah and South Dakota.
\
For more details please checkout the following file:
\
Bayesian Linear Regression for SAT exam.pdf
