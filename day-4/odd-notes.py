# Building up pairs w/ out assignment (except for functions)
def cons(a):
     return lambda b: (lambda z: z(a)(b))
def car(p):
     return p(lambda x: lambda y: x)
def cdr(p):
     return p(lambda x: lambda y: y)
p = cons(2)(3)

# Prepping for the lambda calculus
one = lambda f: lambda x: f(x)
two = lambda f: lambda x: f(f(x))
three = lambda f: lambda x: f(f(f(x)))
four = lambda f: lambda x: f(f(f(f(x))))
def inc(x):
     return x + 1
#inc(0)
#four(inc)(0)
