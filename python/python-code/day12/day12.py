''' Importing a module '''
import mymodule
print(mymodule.generate_full_name('rapt', 'azure'))

''' Import functions from a module '''
from mymodule import generate_full_name, sum_two_nums
print(sum_two_nums(1,9))

''' Import functions from a module and renaming '''
from mymodule import generate_full_name as fullname
print(fullname('meow', 'cat'))

''' Import Builtin Modules '''
# os module
import os
# os.mkdir('test')
# os.chdir('test')
# retval = os.getcwd()
# print("Directory changed successfully %s" % retval)

# sys module
import sys
print(sys.argv[0], sys.argv[1], sys.argv[2])
print('Welcome {}. Enjoy {} challenge!'.format(sys.argv[1], sys.argv[2]))

# statistics module
from statistics import *
ages = [12, 13, 56, 78, 98, 45, 34, 20, 20]
print(mean(ages))
print(median(ages))
print(mode(ages))
print(stdev(ages))

# math module
import math
print(math.pi)            # 3.141592653589793, pi constant
print(math.sqrt(2))       # 1.4142135623730951, square root
print(math.pow(2, 3))     # 8.0, exponential
print(math.floor(9.81))   # 9, rounding to the lowest
print(math.ceil(9.81))    # 10, rounding to the highest
print(math.log10(100))    # 2 

from math import *
print(pi)                 # 3.141592653589793, pi constant
print(sqrt(2))            # 1.4142135623730951, square root
print(pow(2, 3))          # 8.0, exponential
print(floor(9.81))        # 9, rounding to the lowest
print(ceil(9.81))         # 10, rounding to the highest
print(math.log10(100))    # 2 

# random module
from random import random, randint
print(random())           # it doesn't take argument and return 0 to 0.9999
print(randint(5, 20))     # it returns a random number between 5 and 20

