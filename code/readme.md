This folder includes the scripts required to analyze this environmental data. 

1. Run the `processingscipt.R` script in the `processing_code` subfolder. This code will inputs the data saved in the `raw_data` subfolder and produces the processed data under the `processed_data` subfolder.

2. Run the `univariate_analysis.R` script in the `analysis_code` subfolder. The analysis script will take the processed data and produce the results of the univariate analysis to be included in the manuscript, including _Vibrio_ abundance and environmental variables over time. The results are saved in the `results` folder, either under 'all', 'irl', or 'sle'.

3. Run the `descriptive_statistics.R` script in the `analysis_code` subfolder. The analysis script will take the processed data and produce descriptive statistics to be included in the manuscript.

4. Run the `bivariate_analysis.R` script in the `analysis_code` subfolder. The analysis script will take the processed data and produce the results of the bivariate analysis to be included in the manuscript. The results are saved in the `results` folder.

5.  Run the `full_analysis.R` script in the `analysis_code` subfolder. The analysis script will take the processed data and produce the results of the full analysis using statistical modeling/machine learning to be included in the manuscript. The results are saved in the `results` folder.