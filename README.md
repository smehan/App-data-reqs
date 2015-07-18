# App-data-reqs
R to analyze cp app data requests

The data is being pulled from JIRA and now has multiple empty columns
due to lack of data quality control on entry. These are being cleansed
in a pre-processor in log-preprocessor.py. The first R class to run is Load.R.
