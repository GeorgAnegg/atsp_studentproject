#!/usr/bin/env python
import sys
import csv
def writeCSV(filename):
    dim = int(''.join([char for char in filename if char.isnumeric()]))
    file = open(filename, "r")
    strings = file.read().splitlines()[7:-1]
    dists = [dist for sublist in [string.split() for string in strings] for dist in sublist ]
    newfile=filename[:-5]+".csv"
    with open(newfile, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=';')
        for d in range(dim):
            writer.writerow(dists[dim*d:dim*(d+1)])
            


if __name__ == '__main__':
    f=sys.argv[1]
    writeCSV(f)

