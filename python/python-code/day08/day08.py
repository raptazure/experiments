# A dictionary is a collection of unordered, modifiable(mutable) key value paired data type.

''' Creating a dictionary '''
empty_dict = {}
dct = {'key1':'item1', 'key2':'item2', 'key3':'item3', 'key4':'item4'}
person = {
    'first_name': 'miao',
    'last_name': 'miao',
    'age': 250,
    'country': 'China',
    'is_married': False,
    'skills': ['JavaScript', 'Python'],
    'address': {
        'street': 'Space street',
        'zipcode': '222'
    }
}
print(len(person))    # 7

''' Accessing a dictionary items '''
print(person['first_name'])
print(person['country'])
print(person['skills'])
# Accessing an item by key name raises an error if the key does not exist.
# To avoid this error first we have to check if a key exist or we can use the get method.
print(person.get('city'))   # None

''' Adding item to a dictionary '''
dct['key5'] = 'item5'
print(dct)

''' Checking a key in a dictionary '''
print('key2' in dct)   # True

''' Removing key items from dictionary '''
dct.pop('key4')
print(dct)
dct.popitem()   # remove the last item
print(dct)
del dct['key2']
print(dct)

''' Changing dictionary to list items '''
print(dct.items())   #  dictionary -> list of tuples.

''' Clearing dictionary list item '''
print(dct.clear())

''' Deleting dictionary '''
del dct

''' Copy a dictionary '''
dct = {'key1':'item1', 'key2':'item2', 'key3':'item3', 'key4':'item4'}
dct_copy = dct.copy()

''' Getting dictionary keys as list '''
keys = dct.keys()
print(keys)

''' Getting dictionary values as list '''
values = dct.values()
print(values)