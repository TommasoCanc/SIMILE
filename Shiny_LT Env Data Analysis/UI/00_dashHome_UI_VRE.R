fluidRow(

#############
# Left side #
#############

column(width = 4,
box(width = 12,
HTML("

<h3>The XXX App</h3>
<p align='justify'>
The <b>XXX App</b> is an user-friendly toolbox for exploring long-term physical-chemical and biological datasets. The App also provides several conditions to detect anomalies in your dataframe. Anomalies spread inside a dataset may affect data robustness, limiting the model performance and the understanding of specific patterns.
</p>
")
# hr(),
# img(src='Simile.png')
),

box(title = "Type of data", width = 12, collapsible = TRUE, collapsed = TRUE,
HTML("
<p align='justify'>
...
</p>
")
),

box(title = "What we define as anomalies?", width = 12, collapsible = TRUE, collapsed = TRUE,
HTML("
<p align='justify'>
Anomalies are occurrences characterized by an unpredictable error and behaviour inconsistent compared to the entire dataset.

<br>
<br>

Broadly, anomalies can be divided into three main categories:
<br><br>
<b>- Point Anomaly </b>: It is an occurrence with different behaviour compared to the rest of the data. This kind of unusual data is the simplest type of anomaly to detect inside a dataset.

<br><br>


<figure align='center'>
  <img src='Point Anomaly_2.png' width='80%'> <br>
  <figcaption> Point Anomaly. The red dots are anomalies. </figcaption>
</figure>

<br>

<b>- Contextual Anomaly </b>: It is an unusual occurrence only in a specific context determined by the composition of the dataset used for the study. 

<br><br>

<figure align='center'>
  <img src='Contextual Anomaly_2.png' width='80%'> <br>
  <figcaption> Contextual Anomaly. The red circles identify the anomalies. </figcaption>
</figure>

<br>

<b>- Collective Anomaly </b>: They are a series of occurrences that collectively have an anomalous trend compared to the rest of the data. Although one by one these occurrences may be not recognized as anomalies, their inconsistent succession can justify their designation as outliers.  

<br><br>

<figure align='center'>
  <img src='Collective Anomaly_2.png' width='80%'> <br>
  <figcaption> Collective Anomaly. The red square identify the anomalies. </figcaption>
</figure>

<br>

<i>Source: A Comprehensive Beginner’s Guide to the Diverse Field of Anomaly Detection. [https://towardsdatascience.com/a-comprehensive-beginners-guide-to-the-diverse-field-of-anomaly-detection-8c818d153995]</i>
</p>")
),

box(title = "Why R and Shiny", width = 12, collapsible = TRUE, collapsed = TRUE,
HTML("

<p align='right'>
<i>'Take a fresh, interactive approach to telling your data story with Shiny. Let users interact with your data and your analysis. And do it all with R.'</i>
- Shyny R -
</p>

<br>

<p align='justify'>
Shiny is a web application framework developed with R language. With Shiny, it is possible to build a web interface using R commands, and web developer skills are not required. In addition, Shiny allows for handling sophisticated data manipulation and plots attractive graphs efficiently. 
</p>
")
)

),

##############
# Right side #
##############

column(width = 8,
box(width = 12,
HTML("
<h3>Tutorial</h3>

Before using the application, please make sure you have read the tutorial carefully.
<br>
For any questions, do not hesitate to contact the authors (<a href = 'mailto: tommaso.cancellario@gmail.com'>E-Mail</a>).
"
)),

box(title = "Load your data", width = 12, collapsible = TRUE, collapsed = TRUE,
HTML("
<h4>Load data</h4>

<p align='justify'>
To load your data, you need to open the left item, 'Load data' (<b>1</b>), and in the text box 'Main Path' (<b>2</b>), insert the path of the folder containing your data. By default, the primary path is set on CNR-VRE.
<br>
Once set the main path, in the 'Select file(s)' box (<b>3</b>) will appear the list of files contained in the chosen folder. Next, click on 'Load Data' (<b>4</b>) to upload the files. You can also choose the data type (<b>5</b>) and column separator (<b>6</b>). Data type is helpful to rename the file for the download.
</p>

<br>

<figure align='center'>
  <img src='01_Load_data_2.png' width='60%'> <br>
</figure>

<br>
<br>

<h4>Filter columns and rows</h4>

<p align='justify'>
To filter the columns of your dataset, you need to open the specific box and select the columns of your interest in the dedicated dropdown menu (<b>7</b>). After that, click on the button 'View Selection' (<b>8</b>) and open the panel on the right 'Column Filtered Table' to disply the filtered table (<b>9</b>).

<br>

<figure align='center'>
  <img src='02_Filter_columns.png' width='90%'> <br>
</figure>

<br>

To filter the rows, you need to select a specific time range changing the values of 'Date range' (<b>10</b>) in the box 'Filter Rows'. The filtered data are shown in panel 'Row Filtered Table' (<b>11</b>).

<br>

<figure align='center'>
  <img src='03_Filter_rows.png' width='90%'> <br>
</figure>

<br>

In case you need to use the filtered data, please select the checkbox 'Use filtered data'.
</p>

<br>
<br>

<h4>Data Aggregation</h4>
<p align='justify'>
You can aggregate your data by selecting the aggregation time in the panel 'Data aggregation'. You can choose between four aggregation times: Month, Day, Hour, and Minute (<b>12</b>). To see the aggregation outcomes open the panel 'Agr Data Table' (<b>13</b>).
<br>
Aggregate data are obtained by the average of the records grouped by the aggregation time.

<br>

<figure align='center'>
  <img src='04_Aggregation.png' width='90%'> <br>
</figure>

<br>

</p>

<h4>Plot sunrise/sunset</h4>
<p align='justify'>
To plot your data witht sunrise/sunset time, you can use the panel 'Plot sunrise/sunset' (<b>14</b>). To set precisely the sunrise/sunset intervals, you can specify the exact coordinates where your data have been collected. In this panel, you also can modify the plot title and use the aggregated and/or filtered data.
<br>
The plot will be shown clicking on the checkbox 'Plot' (<b>15</b>) and opening the panel item 'Plot' (<b>16</b>).

<br>

<figure align='center'>
  <img src='05_Sunrise_Sunset.png' width='90%'> <br>
</figure>

<br>

</p>

"
)),

box(title = "Check data", width = 12, collapsible = TRUE, collapsed = TRUE,
HTML("

<h4>Select condition(s)</h4>

<p align='justify'>
To detect anomalous data, we created six conditions available in the panel 'Condition(s)' (<b>1</b>). These conditions can be applied singularly or collectively in order to your needs. Once you selected the condition(s), click on the checkbox 'Run' (<b>2</b>). 
<br>
To detect possible anomalies, you need to inspect the resulting table. The data that satisfy the condition(s) are classified with 1, while the other with 0. In the final table, you can find the results for every single condition per parameter and a global aggregation per parameter (it is obtained by multiplying the single condition of each parameter).

<br><br>

<b>Condition description</b> <br>
<b>- Condition 1</b>: Verify the presence of NA <br>
<b>- Condition 2</b>: Verify the presence of 0 <br>
<b>- Condition 3</b>: Select condition thresholds <br>
<b>- Condition 4</b>: Calculete 3 times standard deviation compared to the mean <br>
<b>- Condition 5</b>: Calculete 3 times standard deviation compared to the median (mad) <br>
<b>- Condition 6</b>: Outlier R function <br>

<br>

<figure align='center'>
  <img src='06_Condition.png' width='90%'> <br>
</figure>

<br>

</p>


<h4>Condition Plot</h4>

<p align='justify'>
To visualize the condition results open the panel 'Condition plot' on the left side. You can plot the graph for every condition or their ensemble by moving across the panel items and selecting the specific checkbox. (<b>3</b>) <br>
In the plot, blue points represent the data satisfying the condition, whereas the red ones those that do not fulfil the requirements (<b>4</b>).

<br>

<figure align='center'>
  <img src='07_Condition_plot.png' width='90%'> <br>
</figure>

<br>

</p>
"
))


)
)