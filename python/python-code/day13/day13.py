''' List Comprehension '''
# One way
language = 'Python'
lst = list(language) # changing the string to list
print(type(lst))     # list
print(lst)           # ['P', 'y', 't', 'h', 'o', 'n']

# Second way: list comprehension
lst = [i for i in language]
print(type(lst))     # list
print(lst)           # ['P', 'y', 't', 'h', 'o', 'n']

numbers = [(i, i * i) for i in range(11)]
print(numbers)

# Combined with if
numbers = [-8, -7, -3, -1, 0, 1, 3, 4, 5, 7, 6, 8, 10]
positive_even_numbers = [i for i in range(21) if i % 2 == 0 and i > 0]
print(positive_even_numbers)

# Flattening two dimensional array
two_dimen_list = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened_list = [number for row in two_dimen_list for number in row]
print(flattened_list)    # [1, 2, 3, 4, 5, 6, 7, 8, 9]

''' Lambda Function '''
# Similar to anonymous function in JavaScript
add_two_nums = lambda a, b : a + b
print(add_two_nums(2,3))    # 5
cube = lambda x : x ** 3
print(cube(3))              # 27

# Lambda function inside another function
def power(x):
    return lambda n : x ** n
print(power(2)(3))          # 8

