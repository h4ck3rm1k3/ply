import pprint
from compiler import ast
names ={}

values = {
    'False': False,
    'None': None,
    'True': True,
}

import node_types

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
            pprint.pprint(l['kind'])
            l['kind'](l)
        return l
    else:
        print type(x)
        print x.__type__
