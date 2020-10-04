''' Higher Order Functions '''
# Funciton as a parameter
def sum_numbers(nums):      # normal function
    return sum(nums)

def higher_order_function0(f, *args):
    summation = f(*args)
    return summation
result = higher_order_function0(sum_numbers, [1, 2, 3, 4, 5])
print(result)

# Function as a return value
def square(x):              # a square function
    return x ** 2

def cube(x):                # a cube function
    return x ** 3

def absolute(x):            # an absolute value function
    if x == 0:
        return x
    elif x < 1:
        return -(x)
    else:
        return x

def higher_order_function(type):  # a higher order function returning function
    if type == 'square':
        return square
    elif type == 'cube':
        return cube
    elif type == 'absolute':
        return absolute

# Union['square', 'cube', 'absolute']
result = higher_order_function('square')
print(result(3))       # 9
result = higher_order_function('cube')
print(result(3))       # 27
result = higher_order_function('absolute')
print(result(-3))      # 3

''' Python Closures '''
def add_ten():
    ten = 10

    def add(num):
        return num + ten
    return add

closure_result = add_ten()
print(add_ten()(10))       # 20
print(closure_result(5))   # 15

''' Python Decorators '''
 # Creating decorators
 # Normal function
def greeting():
    return 'Welcome to Python'
def uppercase_decorator(function):
    def wrapper():
        func_ret = function()
        make_uppercase = func_ret.upper()
        return make_uppercase
    return wrapper
g = uppercase_decorator(greeting)
print(g())           # WELCOME TO PYTHON

# This decorator function is a higher order which takes function as a parameter
def uppercase_decorator(function):
    def wrapper():
        func_ret = function()
        make_uppercase = func_ret.upper()
        return make_uppercase
    return wrapper
@uppercase_decorator
def greeting():
    return 'Welcome to Python'
print(greeting())    # WELCOME TO PYTHON

# Applying multiple decorators to a single function
def uppercase_decorator(function):
    def wrapper():
        func_res = function()
        make_uppercase = func_res.upper()
        return make_uppercase
    return wrapper

def split_string_decorator(function):
    def wrapper():
        func_res = function()
        splitted_string = func_res.split()
        return splitted_string
    return wrapper

def greeting0():
    return 'welcome to python'

# Pay attention to the order
@split_string_decorator
@uppercase_decorator
def greeting():
    return 'welcome to python'
print(greeting())   # ['WELCOME', 'TO', 'PYTHON']
print(split_string_decorator(uppercase_decorator(greeting0))())

# Accepting parameters in decorator functions
def decorator_with_parameters(function):
    def wrapper_accepting_parameters(para1, para2, para3):
        function(para1, para2, para3)
        print("I live in {}. ".format(para3))
    return wrapper_accepting_parameters

@decorator_with_parameters
def print_full_name(first_name, last_name, country):
    print("I am {} {}. ".format(first_name, last_name, country))

print_full_name('meow', 'cat', 'utopia')

''' Built-in Higher Order Functions '''
# Map
numbers = [1, 2, 3, 4, 5]   # iterable
numbers_squared = map(lambda x : x ** 2, numbers)
print(list(numbers_squared))

numbers_str = ['1', '2', '3', '4', '5']
numbers_int = map(int, numbers_str)
print(list(numbers_int))

names = ['meow', 'alex', 'kate', 'cristina']
print(list(map(lambda name : name.upper(), names)))

# Filter
def is_even(num):
    if num % 2 == 0:
        return True
    return False
print(list(filter(is_even, numbers)))

long_names = filter(lambda name : True if (len(name) > 5) else False, names)
print(list(long_names)) 

# Reduce
numbers_str = ['1', '2', '3', '4', '5']
def add(x, y):
    return int(x) + int(y)

import functools
total = functools.reduce(add, numbers_str)
print(total)    # 15