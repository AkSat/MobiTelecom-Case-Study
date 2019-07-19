# MobiTelecom-Case-Study
At Mobicom, you are a business analyst and have been urgently called into a meeting with the marketing head and retention manager. The agenda of this meeting is to discuss the results of Industry survey reports that have been just released. In response to these reports, senior management at Mobicom is concerned that the market environment of rising churn rates and declining ARPU will hit them even harder as churn rate at Mobicom is relatively high. Currently they have been focussing on retaining their customers on a reactive basis when the subscriber calls in to close the account. But this alone does not seem to be enough and the management team is keen to take more initiatives on this front. One of these is to roll out targeted proactive retention programs, which include usage enhancing marketing programs to increase minutes of usage (MOU), rate plan migration, and a bundling strategy among others. To be able to effectively drive these retention strategies, a few key questions of interest require urgent attention and you been given the task of showcasing data based insights and recommendations relating to subscriber churn


Solution:

Variables with a lot of missing values are omitted from the analysis. Considering the ideal 70% figure, variables having over 70% missing values will be removed from our analysis data.


Data Profiling is performed on categorical and numeric variables to :
-> understand significance of each variable
-> have any transformations on the variable
-> remove outliers
-> create derived variables
-> delete missing values 
-> remove anamolies 

From data profiling we understand :
a. The variables change_mou, totmrc_Mean, rev_Mean, eqpdays have negative minimum.

b. The variable csa is removed from analysis as it does not give much information

c. The following variables are interaction variables.
-> ATTEMPT_MEAN =	PLCD_DAT_MEAN + PLCD_VCE_MEAN
-> COMPLETE_MEAN	= COMP_DAT_MEAN + COMP_VCE_MEAN
-> DROP_BLK_MEAN	 = BLCK_DAT_MEAN + BLCK_VCE_MEAN + DROP_DAT_MEAN + DROP_VCE_MEAN
-> OVRREV_MEAN	= DATOVR_MEAN + VCEOVR_MEAN
Hence we remove the inidividual variables.

d. The variable retdays is an important factor and cannot be deleted due to its increased number of missing values
As per data documentation, missing values for this variable can be assumed to mean there have been no retention calls made by the customer. Hence we replace the missing values with 0 retention calls

e. The churn rates are different for the variable hnd_webcap, hence we replace NA with "Missing".

f. The churn rate for missing values is same as that of level 'T'. Hence, imputing.

The data is partitioned into 70:30 ratio for analysis.


On applying logistic regression, we get an accuracy of 0.5902 and TPR > FPR. The model looks good.

Area under curve(AUC) value is 62.6341% , which is above the Random Chance Line.



Learnings :

I] Top 5 factors driving likelihood of churn :
Below are the top five factors driving lieklihood of churn. 
             factorNames     Coeff
1           uniqsubs_1_9 1.2864100
2 hnd_price_1239.9899902 1.0922820
3           uniqsubs_1_7 0.7334257
4           uniqsubs_1_4 0.2291406
5           refurb_new_R 0.1701710

Since these factors have all positive coefficients, a 1 unit increase in these factors may lead to an average increase in the churn 
rate.


II] Cost and Billing is not an important factor influencing churn behaviour.

 ------------------------------ Cost -------------------------------
 
   TOTMRC_MEAN	 : Monthly Recurring Charge / base cost of the calling 
                  plan regardless of actual minutes used.

 From the model, we deduce that "totmrc_Mean" has a negative impact with
 coefficient value of "-0.005065220"
 Hence, a unit increase in the cost and billing factor will result in a unit
 decrease in the churn rate


------------------------------ Billing -------------------------------

 Factors of Billing earned through revenue : 

   DATOVR_MEAN  :	Mean revenue of data overage
  ..... Overage represents calls or minutes of use over the number of minutes 
  allowed by that customer's calling plan.

   DATOVR_RANGE :	Range of revenue of data overage
   REV_MEAN	   :  Mean monthly revenue (charge amount)
   REV_RANGE	   :  Range of revenue (charge amount)
   RMREV	       :  Total revenue of roaming calls
   OVRREV_MEAN	 :  Mean overage revenue
   OVRREV_RANGE :  Range of overage revenue
   TOTREV	     :  Total revenue
   VCEOVR_MEAN	 :  Mean revenue of voice overage
   VCEOVR_RANGE :  Range of revenue of voice overage



 The model gives us a positive coefficient "0.002260397" for the factor
 "REV_RANGE"
 ..........this implies it has a postitive impact,
 i.e. a unit increase in the revenue will have a unit increase in the churn 
 behaviour, thus leading to churn


 Factor "OVRREV_MEAN" gives us a coefficient of value "0.008319130"
 ..........this implies it has a postitive impact,
 i.e. a unit increase in the overage revenue will have a unit increase in the 
 churn behaviour, thus leading to churn


 Factor "TOTREV" gives us a coefficient of value "0.0001651900"
 ..........this implies it has a postitive impact,
 i.e. a unit increase in the total revenue earned,will have a unit increase in 
 the churn behaviour, thus leading to churn



III] The company should provide better service and network quality to the customers to reduce the churn rate and increase the revenue.
To support the claim, we have below findings:
Considering the below factors :
1. 98% of the data shows over 3 months since last retention call
2. only 19% of the total calls are placed data calls and placed voice calls.
3. Over 60% shows no change in MOU's were provided to the customer.
4. only 14% of the total calls are completed calls, which is less
5. only 19% of the total calls are placed data calls and placed voice calls.
6. number of off-peak calls made are very less
7. number of wireless calls made are very less

Coefficient and significance is verified in the model :
Increase in the number of dropped (failed) voice calls, results in high churn rate
Increase in the number of dropped or blocked calls, results in high churn rate
Decrease in providing use of off-peak voice calls, will increase the churn rate
Decrease in the number of completed calls will increase the churn rate
Decrease in the number of inbound wireless voice calls will increase churn rate



IV] There might have been data connectivity issues, which has resulted in less number of completed calls,data and voice calls,
off-peak calls and wireless calls
To suppprot the claim :
1. only 14% of the total calls are completed calls, which is less
2. only 19% of the total calls are placed data calls and placed voice calls.
3. number of off-peak calls made are very less
4. number of wireless calls made are very less


V]  There is less overage revenue obtained.Hence, number of customers having overage charges are less, i.e. the overage usage is less.
In this case, rate plan migration cannot be used as a proactive retention strategy.

----------------------- |   Revenue

Probability Churn Score |  High  Low Medium

                 High   | 1177  902   1808
                 
                 Low    | 1112 3090   2536
                 
                 Medium | 1596 3777   3424

There are 1112 customers with low churn rate and giving higher revenue.




