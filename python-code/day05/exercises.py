it_companies = ['Facebook', 'Google', 'Microsoft', 'Apple', 'IBM', 'Oracle', 'Amazon']
print("The number of companies is:", len(it_companies) + 1)
print(it_companies[0], it_companies[len(it_companies) >> 1])
it_companies.append('RedHat')
it_companies.insert(len(it_companies) >> 1, 'GitHub')
it_companies[3] = it_companies[3].upper()
print(it_companies)
it_companies.extend('#; ')
print(it_companies)
it_companies = it_companies[0:-3]
print('IBM' in it_companies)
it_companies.sort()
print(it_companies)
it_companies.reverse()
print(it_companies)
del it_companies[len(it_companies) >> 1]
print(it_companies)
it_companies.remove('APPLE')
print(it_companies)
del it_companies
try:
    print(it_companies)
except BaseException as e:
    print(e, "=> Continue!")

front_end = ['HTML', 'CSS', 'JS', 'React', 'Redux']
back_end = ['Node','Express', 'MongoDB']
joined_list = front_end + back_end
full_stack = joined_list.copy()
full_stack.insert(full_stack.index('Redux') + 1, 'Python')
full_stack.insert(full_stack.index('Python') + 1, 'SQL')
print(full_stack)

ages = [19, 22, 19, 24, 20, 25, 26, 24, 25, 24]
ages.sort()
print(ages)
min_age, max_age = ages[0], ages[len(ages) - 1]
print(f"The sum is {min_age + max_age}")
print(f"The median age is { (ages[len(ages) >> 1]+ ages[len(ages) // 2 + 1]) >> 1 }")
age_sum = sum(ages, 0)
age_aver = age_sum / len(ages)
print("The average age is:", age_aver)
print("The value of (min - average) is bigger than (max - average):", abs(min_age - age_aver) > abs(max_age - age_aver))

part1 = full_stack[0 : len(full_stack) >> 1]
part2 = full_stack[len(full_stack) >> 1 :]
print(part1, '\n', part2)
countries = ['China', 'Russia', 'USA', 'Finland', 'Sweden', 'Norway', 'Denmark']
_1st, _2nd, _3rd, *scandic = countries
print(scandic)