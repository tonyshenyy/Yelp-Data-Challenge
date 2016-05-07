import fileinput
import re
import os
import sys

# python3 preprocessing_review.py yelp_academic_dataset_review.json

# Preprocessing to get business data into a set and make a new file of businesses that are not restaurants
new_business_file = open("yelp_business_restaurants.json", "w")
counter = 0
restaurants = set()
# This will match businesses that are restaurants
restaurant_matcher = re.compile('"categories":.*[Rr]estaurants')
for line in fileinput.input("yelp_academic_dataset_business.json"):
	counter += 1
	# If this is a restaurant
	if restaurant_matcher.search(line) is not None:
		bus_id = re.search('"business_id":\s"([^"]*)"', line).group(1)
		restaurants.add(bus_id)
		line = re.sub('("type":.*",\s)', "", line.rstrip())
		new_business_file.write(line + "\n")
	if counter%10000 == 0:
		print(str(counter) + " records processed")
		sys.stdout.flush()

#print("Finished business processing, moving on to reviews")
sys.stdout.flush()
new_review_file = open("yelp_review_restaurants.json", "w")
counter = 0
problem_matcher = re.compile('"text":.*\\{(.*)}.*"type"')
for line in fileinput.input("yelp_academic_dataset_review.json"):
	counter += 1
	bus_id = re.search('"business_id":\s"([^"]*)"', line).group(1)
	if bus_id in restaurants:
		line = re.sub('("votes":[^}]*},\s)', "", line.rstrip())
		line = re.sub('("type":.*",\s)', "", line.rstrip())
		#if problem_matcher
		new_review_file.write(line + "\n")
	if counter%50000 == 0:
		print(str(counter) + " records processed")
		sys.stdout.flush()
new_review_file.close()
print("Finished review processing")
sys.stdout.flush()