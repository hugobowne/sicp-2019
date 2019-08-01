# scheme.py
#
# Challenge:  Can you implement a mini-scheme interpreter (program that's running another program) capable of
# executing the following code (now at bottom of file):

def seval(sexp, env):
    if isinstance(sexp, (int, float)):
        return sexp
    elif isinstance(sexp, str):  #Symbols
        return env[sexp]  #Evaluate symbol names in the 'env'
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
        if sexp[0] == 'set!':
            # (set! name value)
            name = sexp[1]
            value = seval(sexp[2], env)
            # Must change the variable in  the original environment where
            # it was defined
            if name in env:
                for m in env.maps: #Works from most local --> global
                    if name in m:
                        m[name] = value
                    break
                return
            else:
                raise RuntimeError("Undefined Variable")
        if sexp[0] == 'begin':
            # (begin e1 e2 ... en)
            result = None
            for e in sexp[1:]:
                result = seval(e, env)
            return result
        if sexp[0] == 'lambda':
            # (lambda (parameters) body)
            parameters = sexp[1] # names
            body = sexp[2] # expression
            def proc(*args):
                localenv = env.new_child() #make a new "local" scope for proc
                # Bind names (this is wrong wrt SICP & principle of substitution)
                for name, arg in zip(parameters, args):
                    localenv[name] = arg #Storing names in env
                return seval(body, localenv)
            return proc

        # (x, y, z)
        proc = seval(sexp[0], env)
        args =  [ seval(a,  env)
                    for a in sexp[1:]]
        return proc(*args)

from collections import ChainMap
env = ChainMap({
    # Make some builtin  functions

    '+' : lambda x,y: x + y,
    '*' : lambda x,y: x * y,
    '-' : lambda x,y: x - y,
    '<' : lambda x,y: x < y,
    '=' : lambda x,y: x == y,
    '>' : lambda x,y: x > y
})

# test with this code
seval(('define', 'x', 42), env)
seval('x', env)
seval(('set!', 'x', 69), env)
seval('x', env)

# more tesiting of mutation
make_accum = ('define', 'make-accumulator',
                ('lambda', ('value',),
                    ('lambda', ('n',), ('begin',
                                        ('set!', 'value', ('+', 'value', 'n')),
                                        'value'))))
seval(make_accum, env)
# # A function definition expressed as a S-expression (in tuples)
# fact = ('define', 'fact',
#         ('lambda', ('n',), ('if', ('=', 'n', 1), 1, ('*', 'n', ('fact', ('-', 'n', 1))))))
#
# # Some test code
# seval(fact, env)
# seval(('define', 'n', 5), env)
# result = seval(('fact', 'n'), env)
# assert result == 120
