import string

from strings_with_arrows import *

##############################################
## CONSTANTS
##############################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = DIGITS + LETTERS

##############################################
## POSITION
##############################################

class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

##############################################
## TOKENS
##############################################

TT_INT = 'INT'
TT_REAL = 'REAL'
TT_STRING = 'STRING'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_ASTERISK = 'ASTERISK'
TT_SLASH = 'SLASH'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF = 'EOF'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_COLON = 'COLON'
TT_ASSIGN = 'ASSIGN' ##refers to '<-'
TT_EQUAL = 'EQUAL'
TT_EE = 'EE'
TT_GREATERTHAN = 'GREATERTHAN'
TT_LESSTHAN = 'LESSTHAN'
TT_GREATEREQ = 'GREATEREQ'
TT_LESSEQ = 'LESSEQ'
TT_NOTEQ = 'NOTEQ'
TT_DATATYPE = 'DATATYPE'

KEYWORDS = [
    'DECLARE',
    'AND',
    'NOT',
    'OR',
    'IF',
    'THEN',
    'ELSE',
    'ENDIF',
    'FOR',
    'TO',
    'NEXT',
    'WHILE',
    'DO',
    'ENDWHILE'
]

DATA_TYPES = [
    'INT',
    'INTEGER',
    'REAL'
]


class Token:
    def __init__(self,type_,value=None, start = None, end=None):
        self.type = type_
        self.value = value

        if start:
            self.start = start.copy()
            self.end = start.copy()
            self.end.advance()

        if end:
            self.end=end

    def matches(self,type_,value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

##############################################
## ERRORS
##############################################


class Error:
    def __init__(self, start, end, error_name, details):
        self.start = start
        self.end = end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}:{self.details}'
        result += f'File{self.start.fn},line{self.start.ln+1}'
        result += '\n\n' + string_with_arrows(self.start.ftxt, self.start, self.end)
        return result

class IllegalCharError(Error):
    def __init__(self, start, end, details):
        super().__init__(start, end, 'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, start, end, details=''):
        super().__init__(start, end, 'Invalid Syntax', details)

class RunTimeError(Error):
    def __init__(self, start, end, details,context):
        super().__init__(start, end, 'Runtime Error', details)
        self.context=context
    def as_string(self):
        result=self.generate_traceback()
        result = f'{self.error_name}:{self.details}'
        result += '\n\n' + string_with_arrows(self.start.ftxt, self.start, self.end)
        return result
    def generate_traceback(self):
        result =''
        pos = self.start
        ctx = self.context
        while ctx:
            result=f'File{pos.fn},line{str(pos.ln+1)},in{ctx.display_name}\n' +result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent_entry_pos

        return 'Traceback (most recent call last:\n'+result


##############################################
## SCANNER
##############################################

class Scanner:
    def __init__(self,fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1,0,-1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx<len(self.text) else None



    def make_tokens(self):
        tokens = []

        while self.current_char != None:
            if self.current_char in ' /t':
                self.advance()

            elif self.current_char in DIGITS:
                tokens.append(self.make_number())

            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())

            elif self.current_char == '"':
                tokens.append(self.make_string())
            
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS,start=self.pos))
                self.advance()

            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS,start=self.pos))
                self.advance()

            elif self.current_char == '*':
                tokens.append(Token(TT_ASTERISK,start=self.pos))
                self.advance()

            elif self.current_char == '/':
                tokens.append(Token(TT_SLASH,start=self.pos))
                self.advance()

            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN,start=self.pos))
                self.advance()

            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN,start=self.pos))
                self.advance()

            elif self.current_char == '!':
                tok, error = self.make_not_equals()
                if error: return [], error
                tokens.append(tok)

            elif self.current_char == '=':
                tokens.append(self.make_equals())

            elif self.current_char == '<':
                tokens.append(self.make_less_than())

            elif self.current_char == '>':
                tokens.append(self.make_greater_than())

            elif self.current_char == ':':
                tokens.append(Token(TT_COLON,start=self.pos))
                self.advance()

            else:
                start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(start, self.pos, "'"+char+"'")
            

        tokens.append(Token(TT_EOF,start=self.pos))
        return tokens, None

    def make_number(self):
        num_str = ''
        dot_count = 0
        start = self.pos.copy()


        while self.current_char != None and self.current_char in (DIGITS + '.'):

            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1

            num_str += self.current_char
            self.advance()


        if dot_count == 0:
            return Token(TT_INT, int(num_str), start,self.pos)
        else:
            return Token(TT_REAL, float(num_str), start,self.pos)

    def make_string(self):
        string = ''
        start = self.pos.copy()
        esc_character = False

        self.adv

    def make_identifier(self):
        identifier_str = ''
        start = self.pos.copy()

        while self.current_char!=None and self.current_char in LETTERS_DIGITS + "_":
            identifier_str+=self.current_char
            self.advance()

        if identifier_str in KEYWORDS:
            tok = TT_KEYWORD
        elif identifier_str in DATA_TYPES:
            tok = TT_DATATYPE
        else:
            tok = TT_IDENTIFIER
        return Token(tok,identifier_str,start,self.pos)

    def make_not_equals(self):
        start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            return Token(TT_NOTEQ,start=start,end=self.pos), None
        self.advance()
        return None, InvalidSyntaxError(start,self.pos,"Expected '=' after '!")

    def make_equals(self):
        tok = TT_EQUAL
        start=self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok = TT_EE

        return Token(tok,start=start, end=self.pos)

    def make_less_than(self):
        tok = TT_LESSTHAN
        start=self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok = TT_LESSEQ
        elif self.current_char == '-':
            self.advance()
            tok = TT_ASSIGN
        elif self.current_char == '>':
            self.advance()
            tok = TT_NOTEQ

        return Token(tok,start=start, end=self.pos)

    def make_greater_than(self):
        tok = TT_GREATERTHAN
        start=self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok = TT_GREATEREQ

        return Token(tok,start=start, end=self.pos)


##############################################
## NODES
##############################################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.start=self.tok.start
        self.end=self.tok.end

    def __repr__(self):
        return f'{self.tok}'



class BinOpNode:
    def __init__(self, lnode, op_tok, rnode):
        self.lnode = lnode
        self.op_tok = op_tok
        self.rnode = rnode

        self.start=self.lnode.start
        self.end = self.rnode.end

    def __repr__(self):
        return f'({self.lnode},{self.op_tok},{self.rnode})'

class UnaryOpNode:
    def __init__(self,op_tok,node):
        self.op_tok=op_tok
        self.node=node

        self.start = self.op_tok.start
        self.end = self.node.end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

class IfNode:
    def __init__(self,cases,else_case):
        self.cases=cases
        self.else_case = else_case
        self.start = self.cases[0][0].start
        self.end = (self.else_case or self.cases[len(self.cases)-1][0]).end

class ForNode:
    def __init__(self, var_name_tok, start_value_node, end_value_node, body_node):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.body_node = body_node

        self.start = self.var_name_tok.start
        self.end = self.body_node.end

class WhileNode:
    def __init__(self,condition_node,body_node):
        self.condition_node = condition_node
        self.body_node = body_node

        self.start = self.condition_node.start
        self.end = body_node.end


class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.start = self.var_name_tok.start
        self.end = self.var_name_tok.end

class VarAssignNode:
    def __init__(self,var_name_tok,value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.start = self.var_name_tok.start
        self.end = self.value_node.end

##############################################
## PARSE RESULT
##############################################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register_advance(self):
        pass

    def register(self,res):
        if res.error:self.error=res.error
        return res.node

        return res
    def success(self,node):
        self.node = node
        return self

    def failure(self, error):
        self.error=error
        return self


##############################################
## PARSER
##############################################
    
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()

    def advance(self, ):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_tok = self.tokens[self.token_index]
        return self.current_tok

    def peek(self):
        if (self.token_index+1)<len(self.tokens):
            return self.tokens[self.token_index+1]

    def parse(self):
        result = self.expr()
        print(not result.error)
        print(self.current_tok.type)
        if not result.error and self.current_tok.type!=TT_EOF:
            return result.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,"Expected '+', '-', '*', or '/'"
            ))
        return result

    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_tok.matches(TT_KEYWORD,'IF'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end, f"Expected 'IF'"
            ))

        res.register_advance()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res
        if not self.current_tok.matches(TT_KEYWORD,'THEN'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,
                f"Expected 'THEN'"
            ))

        res.register_advance()
        self.advance()

        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition,expr))

        if not self.current_tok.matches(TT_KEYWORD,'ENDIF'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,
                f"Expected 'ENDIF'"
            ))

        res.register_advance()
        self.advance()

        if self.current_tok.matches(TT_KEYWORD,'ELSE'):
            res.register_advance()
            self.advance()

            else_case = res.register(self.expr())
            if res.error: return res

            res.register_advance()
            self.advance()

            if not self.current_tok.matches(TT_KEYWORD, 'ENDIF'):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected 'ENDIF'"
                ))
        return res.success(IfNode(cases,else_case))

    def for_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD,"FOR"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,
                f"Expected 'FOR"
            ))

        res.register_advance()
        self.advance()

        if self.current_tok.type !=TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected identifier"
            ))
        var_name = self.current_tok
        res.register_advance()
        self.advance()

        if self.current_tok.type != TT_ASSIGN:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected '<-'"
            ))

        res.register_advance()
        self.advance()

        start_value = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD, 'TO'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'TO'"
            ))

        res.register_advance()
        self.advance()

        end_value = res.register(self.expr())
        if res.error: return res

        res.register_advance()
        self.advance()

        body = res.register(self.expr())
        if res.error: return res

        res.register_advance()
        self.advance()

        if not self.current_tok.matches(TT_KEYWORD,"NEXT"):
            res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'NEXT"
            ))

        res.register_advance()
        self.advance()

        if  self.current_tok.type != TT_IDENTIFIER:
            res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected identifier after 'NEXT'"
            ))

        return res.success(ForNode(var_name,start_value,end_value,body))

    def while_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD,"WHILE"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,
                f"Expected 'WHILE"
            ))

        res.register_advance()
        self.advance()

        condition = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD,"DO"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'DO'"
            ))

        res.register_advance()
        self.advance()

        body = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD,"ENDWHILE"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'ENDWHILE'"
            ))

        return res.success(WhileNode(condition,body))

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_INT, TT_REAL):
            res.register_advance()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TT_IDENTIFIER:
            res.register_advance()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TT_LPAREN:
            res.register_advance()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advance()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int, float, identifier, '+', '-' or '('"
        ))

    def factor(self, ):
        res = ParseResult()
        tok = self.current_tok
        print("FACTOR "+str(tok.type))

        if tok.type in (TT_PLUS,TT_MINUS):
            res.register_advance()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in (TT_INT, TT_REAL):
            res.register_advance()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TT_IDENTIFIER:
            res.register_advance()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TT_LPAREN:
            res.register_advance()
            self.advance()
            expr=res.register(self.expr())
            if res.error:return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advance()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end, "Expected ')'"
                ))

        elif tok.matches(TT_KEYWORD,'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        elif tok.matches(TT_KEYWORD, "FOR"):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)

        elif tok.matches(TT_KEYWORD, "WHILE"):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)

    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.current_tok.type in ops or (self.current_tok.type,self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advance()
            self.advance()
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)
        return res.success(left)

    def term(self):
        return self.bin_op(self.factor, (TT_ASTERISK, TT_SLASH))

    def comp_expr(self):
        res = ParseResult()
        if self.current_tok.matches(TT_KEYWORD,'NOT'):
            op_tok = self.current_tok
            res.register_advance()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok,node))

        node = res.register(self.bin_op(self.arith_expr,(TT_EQUAL,TT_EE,TT_NOTEQ,TT_LESSEQ,TT_GREATERTHAN,TT_LESSTHAN,TT_GREATEREQ)))
        if res.error:return res.failure(InvalidSyntaxError(
            self.current_tok.start,self.current_tok.end,
            "Expected int, float, identifier, '+', '-', or '(', 'NOT'"
        ))

        return res.success(node)

    def arith_expr(self):
        return self.bin_op(self.term,(TT_PLUS,TT_MINUS))

    def expr(self):
        res=ParseResult()
        if self.current_tok.matches(TT_KEYWORD, 'DECLARE'):
            res.register_advance()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end, "Expected identifier"
                ))

            var_name = self.current_tok
            res.register_advance()
            self.advance()


            if self.current_tok.type != TT_COLON:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end,
                    "Expected ':' after identifier"
                ))

            res.register_advance()
            self.advance()

            if self.current_tok.value not in DATA_TYPES:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end,
                    "Expected data type after ':'"
                ))
            print(self.current_tok.type, self.current_tok.value)
            if self.current_tok.matches(TT_DATATYPE, 'INT') or self.current_tok.matches(TT_DATATYPE,'INTEGER'):
                return_name = var_name
                return_tok = self.current_tok

                res.register_advance()
                self.advance()
                return res.success(VarAssignNode(
                    return_name,

                    NumberNode(Token(
                        TT_INT, 0, return_tok.start, return_tok.end
                    ))))

            elif self.current_tok.matches(TT_DATATYPE,'REAL'):
                return_name = var_name
                return_tok = self.current_tok

                res.register_advance()
                self.advance()
                return res.success(VarAssignNode(
                    return_name,

                    NumberNode(Token(
                        TT_REAL,0.0,return_tok.start, return_tok.end
                    ))))

        elif self.current_tok.type == TT_IDENTIFIER:
            var_name = self.current_tok
            if self.peek().type == TT_ASSIGN:
                res.register_advance()
                self.advance()
                res.register_advance()
                self.advance()
                expr = res.register(self.expr())
                if res.error: return res
                if not global_symbol_table.get(var_name.value):
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.start,self.current_tok.end,
                        f"Variable '{var_name}' was not declared yet (TIP: Use DECLARE <varname> : <DATATYPE> to declare a variable) "
                    ))
                return res.success(VarAssignNode(var_name, expr))
            return self.bin_op(self.comp_expr,((TT_KEYWORD,"AND"),(TT_KEYWORD,"OR")))

        return self.bin_op(self.comp_expr,((TT_KEYWORD,"AND"),(TT_KEYWORD,"OR")))

##############################################
## RUNTIME RESULT
##############################################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self,res):
        if res.error: self.error = res.error
        return res.value
    def success(self,value):
        self.value=value
        return self

    def failure(self,error):
        self.error = error
        return self

##############################################
## VALUES
##############################################

class Number:
    def __init__(self, value):
        self.value=value
        self.set_pos()
        self.set_context()

    def set_pos(self,start=None, end=None):
        self.start=start
        self.end = end
        return self

    def set_context(self,context=None):
        self.context = context
        return self

    def add(self, other):
        if isinstance(other,Number):
            return Number(self.value+other.value).set_context(self.context), None

    def minus(self,other):
        if isinstance(other, Number):
            return Number(self.value-other.value).set_context(self.context),None

    def mul(self,other):
        if isinstance(other, Number):
            return Number(self.value*other.value).set_context(self.context),None

    def div(self,other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RunTimeError(
                    other.start, other.end, "Division by zero not allowed", self.context
                )
            return Number(self.value / other.value).set_context(self.context), None

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            if self.value==other.value:
                return Boolean(True).set_context(self.context),None
            else:
                return Boolean(False).set_context(self.context),None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            if self.value!=other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None


    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            if self.value<other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            if self.value > other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None


    def anded(self, other):
        if isinstance(other, Number):
            if self.value and other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def ored(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None


    def notted(self):
        if not self.value:
            return Boolean(True).set_context(self.context),None
        else:
            return Boolean(False).set_context(self.context),None


    def __repr__(self):
        return str(self.value)

class Boolean:
    def __init__(self, value):
        self.value=value
        self.set_pos()
        self.set_context()

    def set_pos(self,start=None, end=None):
        self.start=start
        self.end = end
        return self

    def set_context(self,context=None):
        self.context = context
        return self

    def __repr__(self):
        return str(self.value).upper()

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            if self.value == other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            if self.value != other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            if self.value < other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            if self.value > other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

    def anded(self, other):
        if isinstance(other, Number):
            if self.value and other.value:
                return Boolean(True).set_context(self.context), None
            else:
                return Boolean(False).set_context(self.context), None

    def ored(self, other):
        if isinstance(other, Number):
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

    def notted(self):
        if not self.value:
            return Boolean(True).set_context(self.context), None
        else:
            return Boolean(False).set_context(self.context), None

##############################################
## CONTEXT
##############################################

class Context:
    def __init__(self,display_name,parent=None,parent_entry_pos=None):
        self.display_name=display_name
        self.parent=parent
        self.parent_entry_pos=parent_entry_pos
        self.symbol_table = None

##############################################
## SYMBOL TABLE
##############################################

class SymbolTable:
    def __init__(self):
        self.symbols = {}
        self.parent = None

    def get(self, name):
        value = self.symbols.get(name,None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value

    def set(self, name, value):
        self.symbols[name] = value

    def remove(self,name):
        del self.symbols[name]

##############################################
## INTERPRETER
##############################################

class Interpreter:
    def visit(self,node, context):
        method_name=f'visit_{type(node).__name__}'
        method = getattr(self,method_name,self.no_visit_method)
        return method(node, context)

    def no_visit_method(self,node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_NumberNode(self,node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.start,node.end)
        )

    def visit_IfNode(self,node,context):
        res = RTResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition,context))
            if res.error: return res
            if condition_value:
                expr_value = res.register(self.visit(expr,context))
                if res.error: return res
                return res.success(expr_value)

        if node.else_case:
            else_value = res.register(self.visit(node.else_case,context))
            if res.error: return res
            return res.success(else_value)

        return res.success(None)

    def visit_ForNode(self, node, context):
        res = RTResult()

        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error: return res

        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error: return res

        step_value = Number(1)

        i = start_value.value

        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value

        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value

            res.register(self.visit(node.body_node, context))
            if res.error: return res

        return res.success(None)

    def visit_WhileNode(self, node, context):
        res = RTResult()

        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res

            if not condition.is_true(): break

            res.register(self.visit(node.body_node, context))
            if res.error: return res

        return res.success(None)

    def visit_VarAccessNode(self,node,context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RunTimeError(
                node.start, node.end, f"Variable '{var_name}' is not defined (TIP: Use DECLARE <varname> : <DATATYPE> to declare a variable)",
                context
            ))

        return res.success(value)


    def visit_VarAssignNode(self,node,context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res
        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_BinOpNode(self,node, context):
        res=RTResult()
        left = res.register(self.visit(node.lnode, context))
        if res.error: return res
        right = res.register(self.visit(node.rnode, context))
        if res.error: return res

        if node.op_tok.type == TT_PLUS:
            result, error = left.add(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.minus(right)
        elif node.op_tok.type == TT_ASTERISK:
            result, error = left.mul(right)
        elif node.op_tok.type == TT_SLASH:
            result, error = left.div(right)
        elif node.op_tok.type == TT_EQUAL or node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NOTEQ:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LESSTHAN:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GREATERTHAN:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LESSEQ:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GREATEREQ:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD,"AND"):
            result,error = left.anded(right)
        elif node.op_tok.matches(TT_KEYWORD,"OR"):
            result, error = left.ored(right)

        if error: return res.failure(error)
        else:
            return res.success(result.set_pos(node.start,node.end))

    def visit_UnaryOpNode(self,node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error: return res
        error = None
        if node.op_tok.type==TT_MINUS:
            number,error=number.mul(Number(-1))
        elif node.op_tok.matches(TT_KEYWORD,"NOT"):
            number, error = number.notted()
        if error: return res.failure(error)
        else:
            return res.success(number.set_pos(node.start,node.end))
##############################################
## RUN
##############################################

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL",Number(0))
global_symbol_table.set("TRUE", Number(1))
global_symbol_table.set("FALSE", Number(0))

def run(fn, text):
    scanner = Scanner(fn, text)
    tokens, error = scanner.make_tokens()
    if error: return None,error

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node,context)
    return result.value, result.error

