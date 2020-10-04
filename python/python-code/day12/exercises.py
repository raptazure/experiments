from random import randint as rd
choices = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 
'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 
'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
def random_user_id():
    max_index = len(choices) - 1
    ans = ''
    for i in range(0, 7):
        ans += choices[rd(0, max_index)]
    return ans
print(random_user_id())

chars = int(input("Enter the number of characters "))
ids = int(input("Enter the number of ids "))
def id_gen_by_user(chars, ids):
    for id in range(1, ids + 1):
        max_index = len(choices) - 1
        ans = ''
        for i in range(0, chars):
            ans += choices[rd(0, max_index)]
        print(id, ":", ans)
id_gen_by_user(chars, ids)

hexa = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'a', 'b', 'c', 'd', 'e', 'f']
def list_of_hexa_colors(nums):
    ans = []
    max_index = len(hexa) - 1
    for i in range(0, nums):
        tmp = '#'
        for j in range(0, 7):
            tmp += hexa[rd(0, max_index)]
        ans.append(tmp)
    return ans

def list_of_rgb_colors(nums):
    ans = []
    for i in range(0, nums):
        v1 = rd(0, 255)
        v2 = rd(0, 255)
        v3 = rd(0, 255)
        tmp = 'rgb(' + str(v1) + ', ' + str(v2) + ', ' + str(v3) + ')'
        ans.append(tmp)
    return ans

def generate_colors(mode: str, nums: int):
    if mode == 'hexa':
        print(list_of_hexa_colors(nums))
    if mode == 'rgb':
        print(list_of_rgb_colors(nums))

generate_colors('hexa', 3)
generate_colors('rgb', 2)

from random import shuffle
def shuffle_list(lst: list):
    shuffle(lst)
    return lst
print(shuffle_list([1, 2, 3, 4, 5]))

def list_unique():
    ans = set()
    while len(ans) < 7:
        ans.add(rd(0, 9))
    return list(ans)
print(list_unique())