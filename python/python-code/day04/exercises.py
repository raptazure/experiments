s = 'Coding ' + 'For ' + 'All'
company = s
print(company)
print(company.__len__())
print(len(company))
print(company.upper())
print(company.capitalize())
print(company.swapcase())
print(company[0:6])
if company.find('Coding') != -1: print("yes")
print(company.replace('Coding', 'Python'))
print(company.split(' '))
print(company.rfind('l'))
sen = "You cannot end a sentence with because because because is a conjunction"
print(sen.rindex('because'))
print(sen[31:54].split(' '))
python_libraries = ['Django', 'Flask', 'Numpy', 'Pandas']
formatted_string = ' '.join('The following are python libraries: %s' % python_libraries)
print(formatted_string)
print(' # '.join(python_libraries))
