# Charter School Project

For the NYU A3SR consulting class with Professor Ying Lu.

Collaborative Project by Chansoo Song, Erik Wang, Frankie Wunschel

## Code

(1) "compile_schools_data.R" 
- imports data from NYC DOE (https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview), separate files for each AY
- imports data from QGIS (QGIS is used to merge CSV files exported from 'export.R' with school zones). separate files for each AY
- import location files. separate files for each AY
- import demographics files. separate files for each AY
- compute diversity index using Shannon Entropy (with demographics data)
- merge all files together
- exports a master dataset called "all_schools_master.csv", which contains DBN, scores by grade, and indicators for math/ela and charter

(2) "data_inventory.R":
- Define "region" as NTA, Community District, Census Tract, Elementary School Zone ID
- groups data by region. separate columns for each year indicate the # of charters/public schools contained in the region for that year

(3) "export.R": 
- creates longitude and latitude columns
- exports files that can be imported into QGIS (e.g. separate CSV files containing elementary schools for each year)

## Data

#### Test Scores:
- Test scores for year ending 2014 to 2018 from NYC: https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/information-and-data-overview
- Test scores for 2006-2012: NYC Open Data
- Grade 3-8 state assessments in English Language Arts (ELA) and Math
- ELA and Math tests changed in 2013, when New York State moved to the Common Core Learning Standards. 
- Each Excel file contains results for all students tested, as well as results by student characteristics including disability status, English Language Learner (ELL) status, race/ethnicity, and gender.

#### Demographics:
- Received from Ying (NYC Open Data)
- Contains: free lunch percent, total enrollment, # students per grade, # ell, # special ed, # students by race, # students by sex

#### Locations:
- Source: NYC Open Data 
- e.g. link for AY 2017-2018: https://data.cityofnewyork.us/Education/2017-2018-School-Locations/p6h4-mpyy
- Available for AY 2012-13 to 2017-18
- Contains: Community District, Council District, Census Tract, NTA (Neighborhood Tabulation Area), Geographical District Code, Admin_District_Location_Code, Address (with Longitude/Lattitude)

#### Shapefiles:
- Source: NYC Open Data (shapefiles from AY 2012-2013 to 2017-2018)
- e.g. link for AY 2017-2018: https://data.cityofnewyork.us/Education/2017-2018-School-Zones/ghq4-ydq4
- Each year contains separate shapefiles for Elementary School Zones, Middle School Zones, High School Zones.
- esid, msid, hsid each indicate id of the zones
- NTA Shapefile: https://data.cityofnewyork.us/City-Government/Neighborhood-Tabulation-Areas/cpf4-rkhq
- Community Districts Shapefile: https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4


