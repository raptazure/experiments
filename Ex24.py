# ex24 More Practice
print("Let's practice everything")
print('You\'d need to know \'bout escapes with \\ that do:')
print('\n newlines and \t tabs')
def secret_formula(started):
    jelly_beans = started * 500
    jars = jelly_beans / 100
    crates = jars / 100
    return jelly_beans, jars, crates

start_point = 1000
beans, jars, crates = secret_formula(start_point)

print("With a starting point of: {}".format(start_point))

print(f"We'd have {beans} beans, {jars} jars and {crates} crates.")

start_point /= 10
print("We can also do that this way.")
formular = secret_formula(start_point)
print("We'd have {} beans, {} jars, and {} crates.".format(*formular))


