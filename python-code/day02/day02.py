print(help('keywords'))
print(dir(str))

# It takes multiple numbers or a list as arguments and return min
min(20, 30, 40, 50)
min([20, 30, 40, 500])
# Difference: `sum` only takes list as an argument
sum([20, 30, 40, 50])

# Variables
skills = ['HTML', 'CSS', 'JS', 'Python']
age0 = 250
is_married = True
person_info = {
    'firstname': 'Black',
    'lastname': 'Azure',
}
print('Age: ', age0)
print('Married: ', is_married)
print("hello", ",", "world", len('hello , world'))
# Input
age = int(input("How old are you? "))
print(">>> age =",repr(age)) 
print(f"so you're {age}")