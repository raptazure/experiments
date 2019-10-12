# ex18 Names, Variables, Code, Functions

#this one is like your scripts with argv
def print_two(*args):
    # * tells python to take all the arguments to the function and then 
    # put them in args as a list(like a argv bur for functions)
    arg1,arg2 = args
    print(f"arg1:{arg1},arg2:{arg2}")

#ok, that *args is actually pointers, we can just do this
def print_two_again(arg1,arg2):
    print(f"arg1:{arg1},arg2:{arg2}")

#this just takes one argument
def print_one(arg1):
    print(f"arg1:{arg1}")

#this one takes no arguments
def print_none():
    print("I got nothin'. ")

print_two("Raptazure","HITwh")
print_two_again("Raptazure","HITwh")
print_one("First!")
print_none()