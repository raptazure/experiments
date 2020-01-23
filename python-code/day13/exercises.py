numbers = [-4, -3, -2, -1, 0, 2, 4, 6]
nums = [num for num in numbers if num <= 0]
print(nums)

list_of_lists =[[[1, 2, 3]], [[4, 5, 6]], [[7, 8, 9]]]
flatten = [num for _3d in list_of_lists for _2d in _3d for num in _2d]
print(flatten)

res = []
for i in range(11):
    ele = []
    ele.append(i)
    ele.extend([i ** j for j in range(6)])
    res.append(tuple(ele))
print(res)

countries = [[('Finland', 'Helsinki')], [('Sweden', 'Stockholm')], [('Norway', 'Oslo')]]
flat_countries = [country.upper() for _3d in countries for _2d in _3d for country in _2d]
print(flat_countries)

country_res = []
keys = ['country', 'city']
for i in range(0, 5, 2):
    country_res.append(dict(zip(keys, flat_countries[i:i+2])))
print(country_res)

names = [[('Asabeneh', 'Yetaeyeh')], [('David', 'Smith')], [('Bill', 'Gates')]]
flat_names = [name for _3d in names for _2d in _3d for name in _2d]
list_name = [flat_names[i] + ' ' + flat_names[i + 1] for i in range(0, len(flat_names), 2)]
print(list_name)

slope = lambda a, b, c : -1 * b / a
print(slope(4, -3, 5))