node_types = (
    "UNEXPOSED_DECL",
    "UNEXPOSED_EXPR",
    "TYPE_REF",
    "TYPEDEF_DECL",
    "INTEGER_LITERAL",
    "FIELD_DECL",
    "STRUCT_DECL",
    "UNION_DECL",
    "ENUM_CONSTANT_DECL",
    "ENUM_DECL",
    "BINARY_OPERATOR",
    "VAR_DECL",
    "ARRAY_SUBSCRIPT_EXPR",
    "ASM_LABEL_ATTR",
    "BINARY_OPERATOR",
    "BREAK_STMT",
    "CALL_EXPR",
    "CASE_STMT",
    "CHARACTER_LITERAL",
    "CLASS_DECL",
    "CLASS_TEMPLATE",
    "CLASS_TEMPLATE_PARTIAL_SPECIALIZATION",
    "COMPOUND_ASSIGNMENT_OPERATOR",
    "COMPOUND_STMT",
    "CONDITIONAL_OPERATOR",
    "CONST_ATTR",
    "CONSTRUCTOR",
    "CONTINUE_STMT",
    "CONVERSION_FUNCTION",
    "CSTYLE_CAST_EXPR",
    "CXX_ACCESS_SPEC_DECL",
    "CXX_BASE_SPECIFIER",
    "CXX_BOOL_LITERAL_EXPR",
    "CXX_CATCH_STMT",
    "CXX_CONST_CAST_EXPR",
    "CXX_DELETE_EXPR",
    "CXX_FUNCTIONAL_CAST_EXPR",
    "CXX_METHOD",
    "CXX_NEW_EXPR",
    "CXX_REINTERPRET_CAST_EXPR",
    "CXX_STATIC_CAST_EXPR",
    "CXX_THIS_EXPR",
    "CXX_THROW_EXPR",
    "CXX_TRY_STMT",
    "DECL_REF_EXPR",
    "DECL_STMT",
    "DEFAULT_STMT",
    "DESTRUCTOR",
    "DO_STMT",
    "ENUM_CONSTANT_DECL",
    "ENUM_DECL",
    "FIELD_DECL",
    "FOR_STMT",
    "FUNCTION_DECL",
    "FUNCTION_TEMPLATE",
    "GNU_NULL_EXPR",
    "GOTO_STMT",
    "IF_STMT",
    "INTEGER_LITERAL",
    "LABEL_REF",
    "LABEL_STMT",
    "MEMBER_REF",
    "MEMBER_REF_EXPR",
    "NAMESPACE",
    "NAMESPACE_REF",
    "NULL_STMT",
    "OVERLOADED_DECL_REF",
    "PAREN_EXPR",
    "PARM_DECL",
    "PURE_ATTR",
    "RETURN_STMT",
    "STRING_LITERAL",
    "STRUCT_DECL",
    "SWITCH_STMT",
    "TEMPLATE_NON_TYPE_PARAMETER",
    "TEMPLATE_REF",
    "TEMPLATE_TEMPLATE_PARAMETER",
    "TEMPLATE_TYPE_PARAMETER",
    "TRANSLATION_UNIT",
    "TYPEDEF_DECL",
    "TYPE_REF",
    "UNARY_OPERATOR",
    "UNEXPOSED_ATTR",
    "UNEXPOSED_DECL",
    "UNEXPOSED_EXPR",
    "UNION_DECL",
    "USING_DECLARATION",
    "USING_DIRECTIVE",
    "VAR_DECL",
    "VISIBILITY_ATTR",
    "WHILE_STMT" )

#print __name__

seen = {}

def eclass(f, n, base=None):

    if base:
        base = "(%s)" % base
    else:
        base = "(nodebase.NodeBase)"
        
    f.write("""
class {classname}{base}:
  def __init__(self,args):
    nodebase.NodeBase.__init__(self,args)
""".format(classname=n, base=base))

def process_name(x):
    p2 = []
    parts = x.split('_')
    for p in parts:
        y = p.capitalize()
        p2.append(y)
    return p2

def generate_bases(f):
    f.write("# Base classes\n")
    for x in node_types:
        p2 = process_name(x)
        for x in p2:
            if x not in seen :
                eclass(f,x)
                seen[x]=1

def class_name(p):
    p2 = process_name(p)
    return "".join(p2)
    
def generate(f):
    generate_bases(f)
    f.write("# Derived classes\n")
    for x in node_types:       
        n = class_name(x)
        b = ",".join(process_name(x))
        eclass(f, n,b)    

if __name__ == '__main__':
    f = open("generated_node_types.py","w")
    f.write("import nodebase\n")
    generate(f)
    f.close()

import generated_node_types

#import pprint
#pprint.pprint(generate_node_types)
#pprint.pprint(generate_node_types.__dict__)

def get_class(c):
    n = class_name(c)
    return generated_node_types.__dict__[n]

def instance(c, data):
    return get_class(c)(data)

def node_type(t):
    return get_class(t)    

