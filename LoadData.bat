cd data/
curl -o countries-aggregated.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv
curl -o key-countries-pivoted.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/key-countries-pivoted.csv
curl -o reference.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/reference.csv
curl -o time-series-19-covid-combined.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv
curl -o us_confirmed.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/us_confirmed.csv
curl -o us_deaths.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/us_deaths.csv
curl -o worldwide-aggregated.csv https://raw.githubusercontent.com/datasets/covid-19/master/data/worldwide-aggregated.csv

cd ..
"C:\Program Files\R\R-3.6.3\bin\Rscript.exe"  UpdateData.R
"C:\Program Files\R\R-3.6.3\bin\Rscript.exe"  RunShinyServer.R
pause