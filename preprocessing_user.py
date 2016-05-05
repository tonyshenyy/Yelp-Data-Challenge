import fileinput
import re
import os
import sys

# python3 preprocessing_user.py yelp_academic_dataset_user.json
new_review_file = open("yelp_user_clean.json", "w")
counter = 0
for line in fileinput.input():
	counter += 1
	line = re.sub('("friends".*\],\s)',"", line.rstrip())
	votes_matcher = re.compile('"votes":\s\\{([^}]*)},')
	line = votes_matcher.sub('\\1,', line)
	compliment_matcher = re.compile('"compliments":\s(\\{[^}]*})')
	m = compliment_matcher.search(line)
	x = eval(m.group(1))
	#print(x)
	total = sum([int(x[key]) for key in x])
	line = re.sub('"compliments":\s(\{[^}]*})', '"compliments": '+str(total), line.rstrip())
	line = re.sub('("type":.*",\s)', "", line.rstrip())
	elite_matcher = re.compile('"elite":\s([^\]]*\])')
	m = elite_matcher.search(line)
	x = m.group(1)
	if x != "[]":
		line = re.sub('"elite":\s([^\]]*\])', '"elite": 1', line.rstrip())
	else:
		line = re.sub('"elite":\s([^\]]*\])', '"elite": 0', line.rstrip())
	new_review_file.write(line + "\n")
	if counter%50000 == 0:
		print(str(counter) + " records processed")
		sys.stdout.flush()
print("Processing complete, "+str(counter) +" records processed!")
sys.stdout.flush()
new_review_file.close()