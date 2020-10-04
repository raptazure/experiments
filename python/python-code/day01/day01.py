# Check Data types
print(type([1, 2, 3]))          # list
print(type({'name': 'pacman'})) # dict
print(type((2, 4, 5)))          # tuple
print(type({4.5, 9}))           # set
print(type(1 + 1j))             # complex
print(type(True))               # bool
print(type(zip([1,2],[3,4])))   # set
print(3 ** 2)
print(3 // 2)

# Casting
num_int = 10
print('num_int',num_int)         # 10
num_float = float(num_int)
print('num_float:', num_float)   # 10.0

# float to int
gravity = 9.81
print(int(gravity))             # 9

# int to str
num_int = 10
print(num_int)                  # 10
num_str = str(num_int)
print(num_str)                  # '10'

# str to int
num_str = '10.6'
print('num_int', int(num_str))      # 10
print('num_float', float(num_str))  # 10.6

# str to list
first = 'Asabeneh'
print(first_name)
print(first_name)                    # 'Asabeneh'
first_name_to_list = list(first_name)
print(first_name_to_list)            # ['A', 's', 'a', 'b', 'e', 'n', 'e', 'h']