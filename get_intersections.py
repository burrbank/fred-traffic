# get intersections from collision data

import csv

outfile = open('data/intersection', 'w')

def get_comb(street, at_near, near):
    t = [street, at_near, near]
    t.sort()
    return ''.join(t)

with open('data/traffic_accidents.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        temp = [row['Street'], row['At_Near'], row['Near']]
        temp.sort()
        out = ''.join(temp)
        print(out, file=outfile)
