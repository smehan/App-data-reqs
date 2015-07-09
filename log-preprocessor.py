__author__ = 'shawnmehan'

import csv, re

# first lets open the file, which is tab delimited because of , in description field and others
with open('./data/AppDataRequest2010-2015.tsv', 'rb') as csvfile:
    outfile = open('./data/AppDataRequest2010-2015-clean.tsv', 'wb')  # TODO get proper line endings, not ^M
    records = csv.reader(csvfile, delimiter='\t')
    outwriter = csv.writer(outfile, delimiter='\t')
    count = 0
    for row in records:  # iterate through each row to clear out errant newlines
        for i, s in enumerate(row):  # i is list index, s is string
            row[i] = re.sub(r'\n+', '', s)
        outwriter.writerow(row)  # write the clean row
        count += 1
    print u"There were {0:d} lines processed in this run.".format(count)
    outfile.close()
