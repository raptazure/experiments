# Using list builtin function
lst = list()
print(len(lst))

# Using square brackets
empty_list = []
_list = ['Asabeneh', 250, True, {'country':'Finland', 'city':'Helsinki'}] # list containing different data types
fruits = ['banana', 'orange', 'mango', 'lemon']
print(_list)
print('Fruits:', fruits)

''' Accessing list items '''
last_fruit = fruits[len(fruits) - 1]
last_fruit = fruits[-1]
print(last_fruit)

''' Unpacking list items '''
packed_list = ['item','item2','item3', 'item4', 'item5']
first_item, second_item, third_item, *rest = packed_list
print(first_item)     # item1
print(second_item)    # item1
print(third_item)     # item2
print(rest)           # ['item4', 'item5']
first, second, third,*rest, tenth = [1,2,3,4,5,6,7,8,9,10]
print(first)
print(second)
print(third)
print(rest)
print(tenth)

''' Slicing items from list '''
fruits = ['banana', 'orange', 'mango', 'lemon'] 
all_fruits = fruits[0:4]         # it returns all the fruits
all_fruits = fruits[0:]          # if we don't set where to stop it takes all the rest
orange_and_mango = fruits[1:3]   # it does not include the end index
orange_mango_lemon = fruits[1:]
all_fruits = fruits[-4:]         # it returns all the fruits
orange_and_mango = fruits[-3:-1]
orange_mango_lemon = fruits[-3:]

''' Modifying list '''
fruits[0] = 'Avocado' 
print(fruits)

''' Checking items in a list '''
does_exist = 'banana' in fruits
print(does_exist)  # False

''' Adding items in a list '''
lst.append('item')
lst.append(3)
print(lst)

''' Inserting item into a list '''
# Insert apple between orange and mango
fruits.insert(2, 'apple')
print(fruits)
# 'lime' becomes the 3rd   
fruits.insert(3, 'lime')
print(fruits)

''' Removing item from list '''
fruits.remove('lime')
print(fruits)

# Using del
del fruits[len(fruits) - 1]
print(fruits)
del lst
try:
    print(lst)
except BaseException as e:
    print(e, '-> Continue')

# Using pop
fruits.pop()
print(fruits)

# Clearing list items
fruits.clear()
print(fruits)

''' Copying a list '''
fruits = ['banana', 'orange', 'mango', 'lemon']
fruits_copy = fruits.copy()     
print(fruits_copy)

''' Joining lists '''
# Plus operator
positive_numbers = [1, 2, 3, 4, 5]
zero = [0]
negative_numbers = [-5, -4, -3, -2, -1]
integers = negative_numbers + zero + positive_numbers
print(integers)

# Using `extend()` method
negative_numbers = [-5, -4, -3, -2, -1]
positive_numbers = [1, 2, 3, 4, 5]
zero = [0]
negative_numbers.extend(zero)
negative_numbers.extend(positive_numbers)
print('Integers:', negative_numbers)

''' Counting items in a list '''
ages = [22, 19, 24, 25, 26, 24, 25, 24]
print(ages.count(24))   # 3

''' Finding index of an item '''
print(fruits.index('orange'))  # the 1st occurence

''' Reversing a list '''
ages = [26, 27, 25, 24]
ages.reverse()
print(ages)     # [24, 25, 27, 26]

''' Sorting list items '''
# sort()
fruits.sort()                 # ascending
print(fruits) 
fruits.sort(reverse=True)     # descending
print(fruits)

# sorted()
fruits = ['banana', 'orange', 'mango', 'lemon']
fruits_ascending = sorted(fruits)
fruits_descending = sorted(fruits, reverse = True)
print(fruits_ascending)
print(fruits_descending)
print(fruits)