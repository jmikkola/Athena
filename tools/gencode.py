#!/usr/bin/env python3

import random

template = '''
fn main() {{
    print(String({expr}))
}}
'''

def gen_expr(target, prev_operator=None, depth=1):
    if depth > 5:
        return literal_expr(target, depth)
    options = [
        literal_expr,
        plus_expr,
        minus_expr,
        paren_expr,
    ]
    option = random.choice(options)
    return option(target, depth+1)

def literal_expr(target, depth):
    return str(target)

def plus_expr(target, depth):
    delta = random.randint(-1000, 1000)
    left = gen_expr(target-delta, '+', depth)
    right = gen_expr(delta, '+', depth)
    return left + ' + ' + right

def minus_expr(target, depth):
    rhs = random.randint(-1000, 1000)
    left = gen_expr(target+rhs, '-', depth)
    right = gen_expr(rhs, '-', depth)
    return left + ' - ' + right

def paren_expr(target, depth):
    inner = gen_expr(target, None, depth)
    return '(' + inner + ')'

def main():
    expr = gen_expr(target=42)
    result = template.format(expr=expr).strip()
    print(result)


if __name__ == '__main__':
    main()
