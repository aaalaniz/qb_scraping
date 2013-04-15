import csv

with open('output.csv', 'rb') as qb_file:
    qb_reader = csv.reader(qb_file, delimiter=',', quotechar='|')
    for row in qb_reader:
        print "\"http://www.pro-football-reference.com" + row[1] + "\","
