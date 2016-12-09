# # DESCRIPTION
# # - In R we use a Package with name - AnomalyDetection
# # - It is only used for time series data
# # - It is used to identify sudden spikes(identified as trends in twitter) or dips(may be failure in backend services)
# # - It is generally used to identify 4 types of anomalies:
# # 	~ Global Anomalies - Extends above/below the expected seasonal trends
# # 	~ Local Anomalies - Anomalies within or inside the seasonal patterns (hard to detect)
# # 	~ Positive Anomalies - Point in time increase in # of tweets during Super bowl
# # 	~ Negative Anomalies - Point in time decrease in Queries per second (mostly hardware or data collection issues)
# # - Inside the Package (Working Procedure):
# # 	~ Decomposes the time series using STL (Seasonal + Trend + Leftover)
# # 	~ Uses SHESD (Seasonal hybrid Extreme Studentized Deviate)
# # 	~ To identify local/global anomalies
# # - Some useful Concepts used in the package:
# # 	~ Student t-distribution: When we sample a normal distribution with unknown variance
# # 	~ ESD (Extreme Studentized Deviate): Same as Z-Score (Denoted by G). If G > certain value then Outlier
# # 	~ Generalized ESD: Assumes 'r' outliers. Calculates 'G' and removes the point with high 'G' value calculated with new mean and SD
# # 	~ Decomposing Time Series/Removing Seasonality:
# # 		+ It is done to make the data normally distributed
# # 		+ We use STL (Decomposition using LOESS [Local Regression])
# # 		+ It decomposes data into seasons + trends + leftover and then fits a low order polynomial to a subset of the data using LOESS and then stitches them together by weighting them.
# # 		+ Once the season/trend are removed - we can apply ESD on the left over data - to identify anomalies.
# # 		+ https://www.otexts.org/sites/default/files/resize/fpp/images/elecequip_stl2-600x596.png
# # - Works on most of the data and doesn't work on few types of data: (such as)
# #   ~ https://anomaly.io/anomaly-detection-twitter-r/
# # - Implementation in R:
# # 	~ https://github.com/martin-magakian/Anomaly-Detection-test/blob/master/AnomalyTest.R
# # 	~ https://github.com/pablo14/anomaly_detection_post/blob/master/anomaly_detection.R
# # - or the code below:
library(AnomalyDetection)
data(raw_data)
# max_anoms = 0.02 is Maximum anomalies that we need = 0.02% of the data
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
