import fileinput
import re
import os
import sys

# python3 preprocessing_review.py yelp_academic_dataset_review.json
new_review_file = open("yelp_review_clean.json", "w")
counter = 0
for line in fileinput.input():
	counter += 1
	line = re.sub('("text":.*",\s)', "", line.rstrip())
	line = re.sub('("votes":.*},\s)', "", line.rstrip())
	line = re.sub('("type":.*",\s)', "", line.rstrip())
	line = re.sub('("review_id":.*",\s)',"",line.rstrip())
	new_review_file.write(line + "\n")
	if counter%50000 == 0:
		print(str(counter) + " records processed")
		sys.stdout.flush()
new_review_file.close()
