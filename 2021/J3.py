#!/usr/bin/env python3.8

with open ("data/J3.txt", "r") as my_file:
    l = my_file.read().split('\n')
    my_file.close()
del l[len(l)-1]

def find(l, rating):
    index = 0
    while len(l) > 1: # on veut qu'il ne reste qu'un nombre
        somme = 0
        for el in l:
            # print(str(el)[index], end=' ')
            somme += int(str(el)[index])

        if rating == 'oxygen':
            # on détermine la plus grande valeur
            if somme >= len(l)/2: # il y a plus de la moitié qui sont des "1"
                gde = 1
            else:
                gde = 0
        if rating == 'co2':
            # on cherche plus petite valeur
            if somme < len(l)/2: # il y a plus de la moitié qui sont des "0"
                gde = 1
            else:
                gde = 0
        criteria = gde
        #print(criteria)
        # if somme >= len(l)/2: # majorité de "1" ?
        #     if rating == 'oxygen':
        #         criteria = 1
        #     else:
        #         criteria = 0
        # else:
        #     if rating == 'oxygen':
        #         criteria = 0
        #     else:
        #         criteria = 1

        #on enlève ceux qui se suivent pas le critère
        for i in range(somme):
            modif = False
            for j in range(len(l)):
                if not modif:
                    if str(l[j])[index] == str(criteria) and len(l) > 1:
                        del l[j]
                        modif = True
        index += 1
    return l

l2 = list()
for el in l:
    l2.append(el)

#print(find(l, 'oxygen')[0])
#print(find(l, 'co2')[0])
oxygen = int (str(find(l, 'oxygen')[0]), 2)
co2 = int (str(find(l2, 'co2')[0]), 2)
print(oxygen*co2)
