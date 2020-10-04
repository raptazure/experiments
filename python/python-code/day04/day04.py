''' String formatting '''
age = 12
first_name = 'rapt'
last_name = 'azure'
formatted_string = 'I am %s %s. I am %d. ' %(first_name, last_name, age)
print(formatted_string)
a, b = 3, 4
print('{} + {} = {}'.format(a, b, a + b))
print('{} / {} = {:.2f}'.format(a, b, a / b))
radius = 10
pi = 3.14
area = pi * radius ** 2
formatted_string0 = 'The area of radius {} is {:.2f}.'.format(radius, area) # 2 digits after decimal
print(formatted_string0)
print(f"{a} / {b} = {a / b :.2f}")
c, d, e, f, g, h = 'Python'
print(g)
language = 'Python'
last_letter = language[-1]
print(last_letter) # n

''' Slicing Python Strings '''
first_three = language[0:3]  # starts at zero index and up to 3 but not include 3
last_three = language[3:6]
print(first_three)
print(last_three)   # hon
# Another way
last_three = language[-3:]
print(last_three)   # hon
last_three = language[3:]
print(last_three)

# Reversing a string
print(language[::-1])

# Skipping characters while slicing
pto = language[0:6:2]
print(pto)

''' String Methods '''
challenge = 'thirty days of python'
# capitalize(): Converts the first character the string to Capital Letter
print(challenge.capitalize())

# count(): returns occurrences of substring in string, count(substring, start=.., end=..)
print(challenge.count('th'))        # 2
print(challenge.count('y', 7, 14))  # 1

# endswith(): Checks if a string ends with a specified ending
# startswith(): Checks if String Starts with the Specified String
print(challenge.endswith('on'))
print(challenge.startswith('thi'))

# expandtabs(): Replaces tab character with spaces, default tab size is 8. It takes tab size argument
challenge = 'thirty\tdays\tof\tpython'
print(challenge.expandtabs())   # 'thirty  days    of      python'
print(challenge.expandtabs(10)) # 'thirty    days      of        python'
  
# find(): Returns the index of first occurrence of substring
challenge = 'thirty days of python'
print(challenge.find('y'))  # 5
print(challenge.find('th')) # 0

# index(): Returns the index of substring
print(challenge.index('th'))

# isalnum(): Checks alphanumeric character
# isalpha(): Checks if all characters are alphabets
# isidentifier():Checks for valid identifier means it check if a string is a valid variable name
# islower():Checks if all alphabets in a string are lowercase
# isnumeric():Checks numeric characters
print(challenge.isalnum()) # False
challenge = '30DaysPython'
print(challenge.isalnum()) # True
print(challenge.isalpha())
print(18.5.is_integer())
print('ten'.isnumeric())

# join(): Returns a concatenated string
web_tech = ['HTML', 'CSS', 'JavaScript', 'React']
result = '#, '.join(web_tech)
print(result)

# strip(): Removes both leading and trailing characters
challenge = ' thirty days of python '
print(challenge.strip(' '))

# replace(): Replaces substring inside
challenge = 'thirty days of python'
print(challenge.replace('python', 'coding'))

# split():Splits String from Left
print(challenge.split())

# title(): Returns a Title Cased String
print(challenge.title())

# swapcase(): Checks if String Starts with the Specified String The string swapcase() method converts all uppercase characters to lowercase and all lowercase characters to uppercase characters of the given string, and returns it.
challenge = 'thirty days of python'
print(challenge.swapcase())   # THIRTY DAYS OF PYTHON
challenge = 'Thirty Days Of Python'
print(challenge.swapcase())  # tHIRTY dAYS oF pYTHON




