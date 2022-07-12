fluidRow(

##############
# Right side #
##############

column(width = 4,
box(width = 12,
HTML("
<h3>The XXX App</h3>

<p align='justify'>
The <b>XXX App</b> is a user-friendly toolbox helpful for exploring long-term physical-chemical datasets providing useful tools to detect anomalies inside them. The presence of anomalies spread inside a dataset may directly affect data robustness, limiting the model performance and understanding of specific patterns.
</p>

<br>

<h3>What we define as anomalies?</h3>

<p align='justify'>
Anomalies as those occurrences characterized by an unpredictable error and behaviour totally inconsistent compared to the rest of the data.

<br>
<br>

Broadly, anomalies can be divided into three main categories:
<br>
<b>- Point Anomaly </b>: It is an occurrence with different behaviour compared to the rest of the data. This kind of data is the simplest type of anomaly to detect inside a dataset.<br>

<p align='center'>
<img src='Point Anomaly.png' width='50%'> <br>
</p>


<b>- Contextual Anomaly </b>: It is an unusual occurrence only in a specific context determined by the composition of the dataset used for the study. <br>

<p align='center'>
<img src='Contextual Anomaly.png' width='50%'> <br>
</p>

<b>- Collective Anomaly </b>: They are a series of occurrences that collectively have an anomalous trend compared to the rest of the data. Although one by one these occurrences may be not recognized as anomalies, their inconsistent succession can justify their designation as outliers.  <br>

<p align='center'>
<img src='Collective Anomaly.jpg' width='50%'> <br>
</p>

<br>

<i>Source: A Comprehensive Beginner’s Guide to the Diverse Field of Anomaly Detection. [https://towardsdatascience.com/a-comprehensive-beginners-guide-to-the-diverse-field-of-anomaly-detection-8c818d153995]</i>

</p>

")
# hr(),
# img(src='Simile.png')
)
),

#############
# Left side #
#############
column(width = 8,
box(title = "Tutorial", width = 12,
HTML("Hello world!"))
)
)