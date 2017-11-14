# get intersections from collision data

import csv

outfile = open('data/intersection.txt', 'w')

def get_comb(street, at_near, near):
    t = [street, at_near, near]
    t.sort()
    return ''.join(t)

def tidy(street:str) -> str:
    street = street.replace('Street', '')
    street = street.replace('Road', '')
    street = street.strip()
    return street

with open('data/traffic_accidents.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        temp = [tidy(row['Street']), tidy(row['Near'])]
        temp.sort()
        out = '-'.join(temp)
        print(out, file=outfile)
