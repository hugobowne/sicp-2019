# scheme.py
#
# Challenge:  Can you implement a mini-scheme interpreter (program that's running another program) capable of
# executing the following code:

def substitute(expr, name, value):
    if expr == name:
        return value
    elif isinstance(expr, tuple):
        return tuple(substitute(e, name, value)
                     for e in expr)
    else:
        return expr

def seval(sexp, env):
    # Define a procedure
    if isinstance(sexp, tuple):
        if sexp[0] == 'lambda':
            # (lambda (parameters) body)
            parameters = sexp[1] # names
            body = sexp[2] # expression
            def proc(*args):
                # Bind names (this is wrong wrt SICP & principle of substitution)
                pbody = body
                for name, arg in zip(parameters, args):
                    #localenv[name] = arg
                pbody = substitute(pbody, name, arg)
                return seval(pbody)
            return proc

        # Call a procedure
        # (x, y, z)
        proc = seval(sexp[0], env)
        args =  [ seval(a)
                    for a in sexp[1:]]
        return proc(*args)


# ints
# env
# lists
# procedures
# built-ins
# special forms

# A function definition expressed as a S-expression (in tuples)
fact = ('define', 'fact',
        ('lambda', ('n',), ('if', ('=', 'n', 1), 1, ('*', 'n', ('fact', ('-', 'n', 1))))))

# Some test code
seval(fact, env)
seval(('define', 'n', 5), env)
result = seval(('fact', 'n'), env)
assert result == 120
