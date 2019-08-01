# factorial function
#fact = lambda n: 1 if n == 0 else n*fact(n-1) #illegal

#print(fact(5))


# in lambda calculus:
#print((lambda n: 1 if n == 0 else n*fact(n-1))(5))

print((lambda r: lambda n: 1 if n == 0 else n*r(r)(n-1))
    (lambda r: lambda n: 1 if n == 0 else n*r(r)(n-1))(5))


print(f'''Factorial(5) = {(lambda r: lambda n: 1 if n == 0 else n*r(r)(n-1))
    (lambda r: lambda n: 1 if n == 0 else n*r(r)(n-1))(5)}''')


fact = lambda n: 1 if n == 0 else n*fact(n-1)
# fact = (lambda r: lambda n: 1 if n == 0 else n*r*fact(n-1)(fact)
        # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        # "fact" is a fixed point of this

R = (lambda r: lambda n: 1 if n == 0 else n*r(n-1))
# "fact" is a fixed point of R
# f(x) = x

# In the spirit of SICP, suppose there was a function Y that could
# figgure out the fixed point of R.

# Y(R) -> Fixed point of R.
# fact = Y(R) # Would have a working factorial function.

# Y - "Y combinator"

# R(Y(R)) = Y(R)
# OR Y(R) = R(Y(R))
# So Y(R) = (lambda f: R(f))(Y(R))
# OR Y(R) = (lambda f: R(f))(lambda f: R(f)) #The repeat yourself trick
# Y(R) = (lambda f: R(f(f)))(lambda f: R(f(f)))
# Y = (lambda r: (lambda f: r(f(f)))(lambda f: r(f(f)))) # nope

Y = (lambda r: (lambda f: r(lambda z: f(f)(z)))(lambda f: r(lambda z: f(f)(z))))
fact = Y(R)

F = lambda r: lambda n: 1 if n <= 2 else r(n-1) + r(n-2)
fib = Y(F)
