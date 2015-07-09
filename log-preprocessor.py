__author__ = 'shawnmehan'

import csv, os, re


# # first lets open the file, which is tab delimited because of , in description field and others
with open('./data/AppDataRequest2010-2015.tsv', 'rb') as csvfile:
    testfile = open("./data/test.tsv", "wb") #TODO get proper line endings, not ^M
    records = csv.reader(csvfile, delimiter='\t')
    testwriter = csv.writer(testfile, delimiter='\t')
    count = 0
    for row in records:
        for i, s in enumerate(row):
            row[i] = re.sub(r'\n+', '', s)
        testwriter.writerow(row)
        count += 1
    # for row in records:
    #     target = ','.join(row)
    #     print(target)
    #     if re.match(".*(?<!EOR)$", target):
    #         testwriter.writerow(row)
    #     elif re.match(".*EOR$", target):
    #         # print(row)
    #         # count += 1
    #         testwriter.writerow(row)
    #     else:
    #         testwriter.writerow(target)
    print count
    testfile.close()