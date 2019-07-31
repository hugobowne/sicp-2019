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
    if isinstance(sexp, (int, float)):
        return sexp
    elif isinstance(sexp, str):  #Symbols
        return env[sexp]  #Evaluatte symobl names in the 'env'
    elif isinstance(sexp, tuple):
        # Special forms
        if sexp[0] == 'define':
            name = sexp[1]
            value = seval(sexp[2], env)
            env[name] = value
            return
        if sexp[0] == 'if':
            # (if test consequence alternative)
            if seval(sexp[1], env):
                return seval(sexp[2], env)
            else:
                return seval(sexp[3], env)
        if sexp[0] == 'lambda':
            # (lambda (parameters) body)
            parameters = sexp[1] # names
            body = sexp[2] # expression
            def proc(*args):
                localenv = dict(env)
                # Bind names (this is wrong wrt SICP & principle of substitution)
                pbody = body
                for name, arg in zip(parameters, args):
                    localenv[name] = arg
                pbody = substitute(pbody, name, arg)
                return seval(pbody, localenv)
            return proc

        # (x, y, z)
        proc = seval(sexp[0], env)
        args =  [ seval(a,  env)
                    for a in sexp[1:]]
        return proc(*args)

env = {
    # Make some builtin  functions

    '+' : lambda x,y: x + y,
    '*' : lambda x,y: x * y,
    '-' : lambda x,y: x - y,
    '<' : lambda x,y: x < y,
    '=' : lambda x,y: x == y,
    '>' : lambda x,y: x > y
}


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
