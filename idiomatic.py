#!/usr/bin/env python3

def f(xs):
    """Print a running sum total, and return the product."""
    sum = 0
    product = 1

    for x in xs:
        sum += x
        product *= x
        print(sum, end=' ')

    return product

x = f([1,2,3,4])
print('| {}'.format(x))
