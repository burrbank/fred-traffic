#simple data colection script

#get data from fredericton open data portal
data_source = 'https://opendata.arcgis.com/datasets/b4547c95f12e47b3b2942c64329088f5_0.csv'
local_dest = 'data/traffic_accidents.csv'
download.file(data_source, local_dest)

#add current date-time to the end of collection log
write(date(), 'data/collection_log.txt')