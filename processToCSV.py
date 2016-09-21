import csv
import json
import time
import os
from os import path
fc = csv.writer(open("test2.csv", "w", newline="\n", encoding="utf-8"))
fc.writerow(['created_at', 'text','user', 'location', 'URL'])
for filename in os.listdir(os.getcwd()):
   if filename.endswith('.data'):
      print(filename)
      with open(filename) as f:
         lines = (line.rstrip() for line in f) # All lines including the blank ones
         lines = (line for line in lines if line) # Non-blank lines
         try:
            for line in lines:
               if line:
                  print(type(line))
                  x = json.loads(line)
                  if x['text'][:2] != 'RT':
                     fc.writerow([x['created_at'], x['text'],x['user']['name'], x['user']['location'], 'https://twitter.com/statuses/' + x['id_str']])
         except Exception as e:
            print("failed")
            #time.sleep(5)
            pass

reader=csv.reader(open('test2.csv', 'r', newline='\n', encoding='utf-8'), delimiter=',')
writer=csv.writer(open(path.relpath('outputs/AITweets/test2_clean.csv'), 'w', newline='\n', encoding='utf-8'), delimiter=',')

entries = set()
for row in reader:
   key = (row[1]) # instead of just the last name
   try:
      if key not in entries:
         writer.writerow(row)
         entries.add(key)
   except Exception as e:
      print("Failed")
      pass	
	  
