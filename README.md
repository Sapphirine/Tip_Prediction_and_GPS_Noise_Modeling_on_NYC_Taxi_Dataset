# Tip Prediction and GPS Noise Modeling on NYC Taxi Dataset
## Group ID: 201612-11
## Group: Transportation
## UNI: yt2558, cq2192

## Contents

Our codebase can be divided into two categories: One for preliminary analysis (R/, EDA/, and sql/ directories), and the other for production analysis (SPARK/ directory).

+ sql/ : sql files for adding new tables in Postgre SQL database.
         (you need to first prepare Postgre SQL database of toddwschneider/nyc-taxi-data).
+ R/ : R scripts for preliminary analysis. Assume scripts under sql/ are executed.
+ EDA/ : Figures and graphs of preliminary analysis (exploratory data analysis). You can query this repository by the filenames for how each file was generated.
+ SPARK/ : Python scripts for executing spark (by spark-submit or PySpark).
