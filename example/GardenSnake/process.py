import pprint
from compiler import ast
names ={}

values = {
    'False': False,
    'None': None,
    'True': True,
}

import node_types

import generated_node_types

seen = {}
from path import Path
cwd = Path.getcwd() + "/output"

def foo(self):
    
    s= self.d['extent.start'].split(':')[1]
    k= self.d['kind'].__name__
    #if '/usr/lib/gcc/x86_64-linux-gnu/5/plugin' in s:
    if '/' in s:
        sp= Path(s).parent
        n = cwd  + sp + "/" +k
        fn = n + "/data.py"
        mode = 'w'
        if n not in seen:
            n.makedirs_p()
            seen[n]=1
            #print s
            print fn
        else:
            mode = 'a' # append

        with open (fn,mode) as f:
            f.write(pprint.pformat(self.d))

    #if 'tree' in self.d['extent.start']:
    #    pprint.pprint(self.d)

    #pass



def install():
    for x in generated_node_types.__dict__:
        if (
            ('Decl' in x) and
            ('Expr' not in x) and
            ('Param' not in x)
        ):
            v = generated_node_types.__dict__[x]
            print "Install", x,v
            v.foo=foo
install()

def skip(self):
    pass
generated_node_types.Decl.foo=skip
generated_node_types.ParmDecl.foo = skip
generated_node_types.DeclRefExpr=skip
    
def process_dict(x):

    if isinstance(x, tuple):
        for y in x:
            process_dict(y)
    elif isinstance(x, ast.Name):
        v = None
        
        if x.name in node_types.node_types:
            return node_types.node_type(x.name)
        elif x.name in values:
            return values[x.name]
        else:
            #pprint.pprint({'name':x.__dict__})
            if x.name in names:
                names[x.name]= names[x.name]+1
            else:
                names[x.name]=0
            
    elif isinstance(x, ast.CallFunc):
        # this is always a call to SourceLocation
        if isinstance(x.args[0], ast.Name):
            return 'file:'+str(x.args[0].name) + ":" + str(x.args[1].value) + ":" + str(x.args[2].value)
        else:
            return 'file:'+x.args[0].value + ":" + str(x.args[1].value) + ":" + str(x.args[2].value)        
    elif isinstance(x, ast.Const):
        #pprint.pprint({'const':x.__dict__})
        return x.value
    elif isinstance(x, ast.Tuple):
        l = []
        for y in x.nodes:
            v = process_dict(y)
            l.append(v)
        #pprint.pprint(l)
        return l
    elif isinstance(x, list):
        l = []
        for y in x:
            v = process_dict(y)
            l.append(v)
        return l
    elif isinstance(x, dict):
        l = {}
        for f in x:
            v = x[f]
            if f in ('doc','filename','lineno'):
                pass
            elif f in ('flags', 'name'):                
                l[f]=v
            else:
                l[f]=process_dict(v)

        if 'kind' in l :
            #pprint.pprint(l['kind'])
            o = l['kind'](l)
            if o:
                o.foo()
            
        return l
    else:
        print type(x)
        print x.__type__
