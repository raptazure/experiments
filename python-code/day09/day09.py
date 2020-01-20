''' Conditionals '''
a = 0
if a > 0:
    print('A is a positive number')
elif a < 0:
    print('A is a negative number')
else:
    print('A is zero')

# Short Hand
a = 3
print("A is positive") if a > 0 else print('A is negative')

# Nested Conditionals
a = 0
if a > 0:
    if a % 2 == 0:
        print('A is positive even integer')
    else:
        print('A positive number')
elif a == 0:
    print('Zero')
else:
    print('A negative number')

# Logical Operator
a = 0
if a > 0 and a % 2 == 0:
        print('A is even positive integer')
elif a > 0 and a % 2 != 0:
     print('A is positive integer') 
elif a == 0:
    print('Zero')
else:
    print('A negative number')

if a > 0 or a % 2 == 0:
        print('A is positive integer')
elif a == 0:
    print('Zero')
else:
    print('A negative number')