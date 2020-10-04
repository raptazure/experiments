''' Functions '''
# Function with parameters
def print_fullname(firstname, lastname):
    space = ' '
    full_name = firstname  + space + lastname
    print(full_name)
print_fullname('meow', 'cat')

# Passing arguments with key and value
def add_two_numbers (num1, num2):
    total = num1 + num2
    print(total)
add_two_numbers(num2 = 3, num1 = 2)

# Returning a value from a function
def calculate_age (current_year, birth_year):
    age = current_year - birth_year
    return age
print('Age: ', calculate_age(2019, 1819))

def find_even_numbers(n):
    evens = []
    for i in range(n+1):
        if i % 2 == 0:
            evens.append(i)
    return evens
print(find_even_numbers(10))

# Function with default parameters
def weight_of_object (mass, gravity = 9.81):
    weight = str(mass * gravity)+ ' N'
    return weight
print('Weight of an object in Newton: ', weight_of_object(100))

# Arbitrary number of arguments
def sum_all_nums(*nums):
    total = 0
    for num in nums:
        total += num
    return total
print(sum_all_nums(2, 3, 5))

# Default and arbitrary number of parameters in function
def generate_groups (team, *args):
    print(team)
    for i in args:
        print(i, end = " ")
    print('')
generate_groups('Team-1', 'Asabeneh', 'Brook', 'David', 'Eyob')

# Function as parameter of other function
def square_number (n):
    return n * n
def do_something(f, x):
    return f(x)
print(do_something(square_number, 3))