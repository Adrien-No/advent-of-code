#!/usr/bin/env python3.8

values = [ l.strip() for l in open('data/J3.txt') ]
n = len(values)

b = { True : '1', False : '0'}
mosts = ''.join( b[[ v[i] for v in values  ].count('1') > n//2] for i in range(12))
most = eval('0b'+mosts)
least = 2**12 - most - 1
print('Part 1', most * least)

def search(val, i, most=True):
    if len(val) == 1:
        return eval('0b'+val[0])

    val1 = [ v for v in val if v[i] == '1' ]
    val0 = [ v for v in val if v[i] == '0' ]
    n1, n0 = len(val1), len(val0)
    if not most:
        val1, val0 = val0, val1

    if n1 >= n0:
        return search(val1, i+1, most=most)
    else:
        return search(val0, i+1, most = most)

oxygen = search(values, 0, True)
co2 = search(values, 0, False)
print('Part 2', oxygen * co2)
