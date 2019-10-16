# ex39 Dictionaries, Oh Lovely Dictionaries

#A dict lets you use anything, not just numbers.
states = {
    'Oregon': 'OR',
    'Florida': 'FL',
    'California': 'CA',
    'New York': 'NY',
    'Michigan': 'MI'
}
cities = {
    'CA': 'San Fransico',
    'MI': 'Detroit',
    'FL': 'Jacksonville'
}

cities['NY'] = 'New York'
cities['OR'] = 'Portland'

print('-' * 10)
print("NY State has: ",cities['NY'])
print("Michigan's abbreviation is: ",states['Michigan'])
print("Florida has: ",cities[states['Florida']])

print('-' * 10)
for state,abbrev in list(states.items()):
    print(f"{state} is abbreviated {abbrev}")

print('-' * 10)
for abbrev,city in list(cities.items()):
    print(f"{abbrev} has the city {city}")

print('-' * 10)
for state,abbrev in list(states.items()):
    print(f"{state} state is abbreviated {abbrev}")
    print(f"and has city {cities[abbrev]}")

print('-' * 10)
# safely get a abbreviation by state that might not be there
state = states.get('Texas')
if not state:
    print("sorry, no Texas")

city = cities.get('TX', 'Does NOT Exist')
print(f"the city for the state 'TX' is : {city}")


