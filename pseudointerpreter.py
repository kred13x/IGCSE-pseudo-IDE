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
TT_LSQUARE = 'LSQUARE'
TT_RSQUARE = 'RSQUARE'
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
TT_COMMA = 'COMMA'
TT_NEWLINE = 'NEWLINE'

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
    'ENDWHILE',
    'PROCEDURE',
    'ENDPROCEDURE'
    'CALL',
    'FUNCTION',
    'RETURN',
    'OUTPUT'
]

DATA_TYPES = [
    'INT',
    'INTEGER',
    'REAL',
    'STRING'
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
            if self.current_char in ' \t':
                self.advance()

            elif self.current_char in ';\n':
                tokens.append(Token(TT_NEWLINE, start=self.pos))
                self.advance()

            elif self.current_char in DIGITS:
                tokens.append(self.make_number())

            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())

            elif self.current_char == '"':
                tokens.append(self.make_string())

            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA,start=self.pos))
                self.advance()
            
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

            elif self.current_char == '[':
                tokens.append(Token(TT_LSQUARE,start=self.pos))
                self.advance()

            elif self.current_char == ']':
                tokens.append(Token(TT_RSQUARE,start=self.pos))
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

        self.advance()

        while self.current_char != None and self.current_char != '"':
            string += self.current_char
            self.advance()

        self.advance()
        return Token(TT_STRING, string, start, self.pos)

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

    def __str__(self):
        return f'{self.tok.value}'

class StringNode:
    def __init__(self, tok):
        self.tok = tok
        self.start=self.tok.start
        self.end=self.tok.end

    def __repr__(self):
        return f'{self.tok.value}'

    def __str__(self):
        return f'{self.tok.value}'


class ListNode:
    def __init__(self,element_nodes, start, end):
        self.element_nodes = element_nodes
        self.start = start
        self.end = end



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

class OutputNode:
    def __init__(self, output_value):
        self.output_value = output_value
        self.start = self.output_value.start
        self.end = self.output_value.end


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

class ProcFuncDefNode:
    def __init__(self, var_name_tok, arg_name_toks, body_node):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_node

        if self.var_name_tok:
            self.start = self.var_name_tok.pos_start

        self.end = self.body_node.end

class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.start = self.node_to_call.start

        if len(self.arg_nodes)>0:
            self.end = self.arg_nodes[len(self.arg_nodes)-1].end
        else:
            self.end = self.node_to_call.end

##############################################
## PARSE RESULT
##############################################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.last_registered_advance_count = 0
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advance(self):
        pass

    def register(self,res):
        if res.error:self.error=res.error
        return res.node

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return self.register(res)

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
        print("CURRENT TOKEN:" + self.current_tok.type, self.current_tok.value)
        return self.current_tok

    def peek(self):
        if (self.token_index+1)<len(self.tokens):
            return self.tokens[self.token_index+1]

    def reverse(self, amount=1):
        self.token_index -= amount
        self.update_current_tok()
        return self.current_tok

    def update_current_tok(self):
        if self.token_index >= 0 and self.token_index < len(self.tokens):
            self.current_tok = self.tokens[self.token_index]

    def statements(self):
        res = ParseResult()
        statements = []
        start = self.current_tok.start.copy()

        while self.current_tok.type == TT_NEWLINE:
            res.register_advance()
            self.advance()

        statement = res.register(self.expr())
        if res.error: return res
        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_tok.type == TT_NEWLINE:
                res.register_advance()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements: break
            statement = res.try_register(self.expr())
            if not statement:
                self.reverse(res.to_reverse_count)
                more_statements = False
                continue
            statements.append(statement)

        return res.success(ListNode(
            statements,
            start,
            self.current_tok.end.copy()
        ))

    def parse(self):
        result = self.statements()
        print(not result.error)
        print(self.current_tok.type)
        if not result.error and self.current_tok.type!=TT_EOF:
            return result.failure(InvalidSyntaxError(
                self.current_tok.start,self.current_tok.end,"Expected '+', '-', '*', or '/'"
            ))
        print("result: "+str(result))
        return result

    def list_expr(self):
        res = ParseResult()
        element_nodes = []
        start = self.current_tok.start.copy()
        if self.current_tok.type != TT_LSQUARE:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected '['"
            ))

        res.register_advance()
        self.advance()

        if self.current_tok.type == TT_RSQUARE:
            res.register_advance()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
                ))

            while self.current_tok.type == TT_COMMA:
                res.register_advance()
                self.advance()

                element_nodes.append(res.register(self.expr()))
                if res.error: return res

            if self.current_tok.type != TT_RSQUARE:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected ',' or ']'"
                ))

            res.register_advance()
            self.advance()

        return res.success(ListNode(
            element_nodes,
            start,
            self.current_tok.end.copy()
        ))


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
        print("calling for expression")
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

        print("expressing the start value")
        start_value = res.register(self.expr())
        if res.error: return res

        if not self.current_tok.matches(TT_KEYWORD, 'TO'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'TO'"
            ))

        res.register_advance()
        self.advance()

        print("expressing the end value")

        end_value = res.register(self.expr())
        if res.error: return res

        res.register_advance()
        self.advance()

        print("expressing the body value")

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

        res.register_advance()
        self.advance()

        return res.success(WhileNode(condition,body))

    def output_expr(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD,"OUTPUT"):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'OUTPUT"
            ))

        res.register_advance()
        self.advance()

        output_value = res.register(self.expr())
        if res.error: return res

        return res.success(OutputNode(output_value))


    def proc_def(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD,'PROCEDURE'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'PROCEDURE'"
            ))

        res.register_advance()
        self.advance()

        if self.current_tok != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected identifier"
            ))

        var_name_tok = self.current_tok
        res.register_advance()
        self.advance()
        arg_name_toks = []

        if self.current_tok == TT_LPAREN:
            res.register_advance()
            self.advance()
            if self.current_tok.type != TT_IDENTIFIER:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end,
                    f"Expected identifier"
                ))

            arg_name_tok = self.current_tok
            res.register_advance()
            self.advance()

            if self.current_tok.type != TT_COLON:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected colon"
                ))

            res.register_advance()
            self.advance()

            if self.current_tok.type != TT_DATATYPE:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected data type"
                ))

            arg_name_toks.append(arg_name_tok)
            res.register_advance()
            self.advance()

            while self.current_tok.type == TT_COMMA:
                res.register_advance()
                self.advance()
                if self.current_tok.type != TT_IDENTIFIER:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected identifier"
                    ))

                arg_name_tok = self.current_tok
                res.register_advance()
                self.advance()

                if self.current_tok.type != TT_COLON:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected colon"
                    ))

                res.register_advance()
                self.advance()

                if self.current_tok.type != TT_DATATYPE:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected data type"
                    ))

                arg_name_toks.append(arg_name_tok)
                res.register_advance()
                self.advance()

            if self.current_tok.type != TT_RPAREN:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected ',' or ')''"
                ))

            res.register_advance()
            self.advance()

        node_to_return = res.register(self.expr())
        if res.error:return res
        res.register_advance()
        self.advance()


        if not self.current_tok.matches(TT_KEYWORD,'ENDPROCEDURE'):
            res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'ENDPROCEDURE'"
            ))

        res.register_advance()
        self.advance()

        return res.success(ProcFuncDefNode(
            var_name_tok, arg_name_toks, node_to_return
        ))

    def func_def(self):
        res = ParseResult()

        if not self.current_tok.matches(TT_KEYWORD,'FUNCTION'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'FUNCTION'"
            ))

        res.register_advance()
        self.advance()

        if self.current_tok != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected identifier"
            ))

        var_name_tok = self.current_tok
        res.register_advance()
        self.advance()
        arg_name_toks = []

        if self.current_tok == TT_LPAREN:
            res.register_advance()
            self.advance()
            if self.current_tok.type != TT_IDENTIFIER:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start,self.current_tok.end,
                    f"Expected identifier"
                ))

            arg_name_tok = self.current_tok
            res.register_advance()
            self.advance()

            if self.current_tok.type != TT_COLON:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected colon"
                ))

            res.register_advance()
            self.advance()

            if self.current_tok.type != TT_DATATYPE:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected data type"
                ))

            arg_name_toks.append(arg_name_tok)
            res.register_advance()
            self.advance()

            while self.current_tok.type == TT_COMMA:
                res.register_advance()
                self.advance()
                if self.current_tok.type != TT_IDENTIFIER:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected identifier"
                    ))

                arg_name_tok = self.current_tok
                res.register_advance()
                self.advance()

                if self.current_tok.type != TT_COLON:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected colon"
                    ))

                res.register_advance()
                self.advance()

                if self.current_tok.type != TT_DATATYPE:
                    res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected data type"
                    ))

                arg_name_toks.append(arg_name_tok)
                res.register_advance()
                self.advance()

            if self.current_tok.type != TT_RPAREN:
                res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    f"Expected ',' or ')''"
                ))

            res.register_advance()
            self.advance()

        node_to_return = res.register(self.expr())
        if res.error:return res
        res.register_advance()
        self.advance()

        if not self.current_tok.matches(TT_KEYWORD,'ENDFUNCTION'):
            res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                f"Expected 'ENDFUNCTION'"
            ))

        res.register_advance()
        self.advance()

        return res.success(ProcFuncDefNode(
            var_name_tok, arg_name_toks, node_to_return
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

        elif tok.type == TT_STRING:
            res.register_advance()
            self.advance()
            return res.success(StringNode(tok))

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

        elif tok.type == TT_LSQUARE:
            list_expr = res.register(self.list_expr())
            if res.error: return res
            return res.success(list_expr)

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

        elif tok.matches(TT_KEYWORD, "PROCEDURE"):
            proc_def = res.register(self.proc_def())
            if res.error: return res
            return res.success(proc_def)

        elif tok.matches(TT_KEYWORD, "FUNCTION"):
            func_def = res.register(self.func_def())
            if res.error: return res
            return res.success(func_def)

        elif tok.matches(TT_KEYWORD, "OUTPUT"):
            output_expr = res.register(self.output_expr())
            if res.error: return res
            return res.success(output_expr)

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

            elif self.current_tok.matches(TT_DATATYPE,'STRING'):
                return_name = var_name
                return_tok = self.current_tok

                res.register_advance()
                self.advance()
                return res.success(VarAssignNode(
                    return_name,

                    StringNode(Token(
                        TT_STRING,"",return_tok.start, return_tok.end
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
            node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(' or 'NOT'"
                ))

            return res.success(node)
        node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

        if res.error:
            return res.failure(InvalidSyntaxError(
                self.current_tok.start, self.current_tok.end,
                "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(' or 'NOT'"
            ))

        return res.success(node)

    def call(self):
        res = ParseResult()
        arg_nodes = []
        factor = res.register(self.factor())
        if res.error: return res

        if self.current_tok.type == TT_LPAREN:
            res.register_advance()
            self.advance()

            if self.current_tok.type == TT_RPAREN:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.start, self.current_tok.end,
                    "Expected identifier"
                ))
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.start,self.current_tok.end,
                        "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUNCTION', int, real, identifier, string"
                    ))

                while self.current_tok.type == TT_COMMA:
                    res.register_advance()
                    self.advance()

                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res

                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
                        self.current_tok.start, self.current_tok.end,
                        f"Expected ',' or ')'"
                    ))

                res.register_advance()
                self.advance()

        return res.success(CallNode(factor, arg_nodes))

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

class Value:
    def __init__(self):
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
        return None, self.illegal_operation(other)

    def minus(self, other):
        return None, self.illegal_operation(other)

    def mul(self, other):
        return None, self.illegal_operation(other)

    def div(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_eq(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_ne(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None, self.illegal_operation(other)

    def anded(self, other):
        return None, self.illegal_operation(other)

    def ored(self, other):
        return None, self.illegal_operation(other)

    def notted(self):
        return None, self.illegal_operation()

    def execute(self, args):
        return RTResult().failure(self.illegal_operation())

    def copy(self):
        raise Exception('No copy method defined')

    def illegal_operation(self, other=None):
        if not other: other = self
        return RunTimeError(
            self.start, other.end,
            'Illegal operation',
            self.context
        )

class Number(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

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
        else:
            return None, Value.illegal_operation(self, other)

    def minus(self,other):
        if isinstance(other, Number):
            return Number(self.value-other.value).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def mul(self,other):
        if isinstance(other, Number):
            return Number(self.value*other.value).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self, other)

    def div(self,other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RunTimeError(
                    other.start, other.end, "Division by zero not allowed", self.context
                )
            return Number(self.value / other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Boolean(self.value == other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Boolean(self.value != other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Boolean(self.value < other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Boolean(self.value > other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Boolean(self.value <= other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Boolean(self.value >= other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.start, self.end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

    def __str__(self):
        return str(self.value)

Number.null = Number(0)

class String(Value):
    def __init__(self,value):
        super().__init__()
        self.value = value

    def add(self,other):
        if isinstance(other,String):
            return String(self.value+other.value).set_context(self.context),None
        else:
            return None, Value.illegal_operation(self,other)

    def mul(self,other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self,other)

    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.start, self.end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return f'{self.value}'

    def __str__(self):
        return f'{self.value}'

class Boolean(Value):
    def __init__(self,value):
        super().__init__()
        self.value = value

    def anded(self, other):
        if isinstance(other, Boolean):
            return Boolean(str(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored(self, other):
        if isinstance(other, Boolean):
            return Boolean(str(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def notted(self):
        return Boolean(True if self.value == False else False).set_context(self.context), None

    def copy(self):
        copy = Boolean(self.value)
        copy.set_pos(self.start, self.end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value).upper()

class List(Value):
    def __init__ (self,elements):
        super().__init__()
        self.elements = elements

    def dived_by(self, other):
        if isinstance(other, Number):
            try:
                return self.elements[other.value], None
            except:
                return None, RunTimeError(
                    other.start, other.end,
                    'Element at this index could not be retrieved from list because index is out of bounds',
                    self.context
                )
        else:
            return None, Value.illegal_operation(self, other)

    def copy(self):
        copy = List(self.elements)
        copy.set_pos(self.start, self.end)
        copy.set_context(self.context)
        return copy

    def __str__(self):
        return ", ".join([str(x) for x in self.elements])

    def __repr__(self):
        return f'[{", ".join([repr(x) for x in self.elements])}]'

class BaseFunction (Value):
    def __init__(self,name):
        super().__init__()
        self.name = name

    def generate_new_context(self):
            new_context = Context(self.name, self.context, self.start)
            new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
            return new_context

    def check_args(self, arg_names, args):
            res = RTResult()
            if len(args) > len(arg_names):
                return res.failure(RuntimeError(
                    self.start, self.end,
                    f"{len(args) - len(arg_names)} too many args passed into '{self.name}'",
                    self.context
                ))

            if len(args) < len(self.arg_names):
                return res.failure(RuntimeError(
                    self.start, self.end,
                    f"{len(arg_names) - len(args)} too few args passed into '{self.name}'",
                    self.context
                ))

            return res.success(None)

    def populate_args(self, arg_names, args, exec_ctx):
            for i in range(len(args)):
                arg_name = arg_names[i]
                arg_value = args[i]
                arg_value.set_context(exec_ctx)
                exec_ctx.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, exec_ctx):
            res = RTResult()
            res.register(self.check_args(arg_names, args))
            if res.error: return res
            self.populate_args(arg_names, args, exec_ctx)
            return res.success(None)



class Function(BaseFunction):
    def __init__(self, name, body_node, arg_names):
        super().__init__()
        self.name = name
        self.body_node = body_node
        self.arg_names = arg_names

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        exec_ctx = self.generate_new_context()

        res.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
        if res.error: return res

        value = res.register(interpreter.visit(self.body_node, exec_ctx))
        if res.error: return res
        return res.success(value)

    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.start, self.end)
        return copy


class BuiltInFunction(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args):
        res = RTResult()
        exec_ctx = self.generate_new_context()

        method_name = f'execute_{self.name}'
        method = getattr(self, method_name, self.no_visit_method)

        res.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
        if res.should_return(): return res

        return_value = res.register(method(exec_ctx))
        if res.should_return(): return res
        return res.success(return_value)

    def no_visit_method(self, node, context):
        raise Exception(f'No execute_{self.name} method defined')

    def copy(self):
        copy = BuiltInFunction(self.name)
        copy.set_context(self.context)
        copy.set_pos(self.start, self.end)
        return copy

    def __repr__(self):
        return f"<built-in function {self.name}>"


    def execute_run(self, exec_ctx):
        fn = exec_ctx.symbol_table.get("fn")

        if not isinstance(fn, String):
            return RTResult().failure(RunTimeError(
                self.start, self.end,
            "Second argument must be string",
                exec_ctx
            ))

        fn = fn.value

        try:
            with open(fn, "r") as f:
                script = f.read()
        except Exception as e:
            return RTResult().failure(RunTimeError(
                self.start, self.end,
                f"Failed to load script \"{fn}\"\n" + str(e),
                exec_ctx
            ))

        _, error = run(fn, script)

        if error:
            return RTResult().failure(RunTimeError(
                self.start, self.end,
                f"Failed to finish executing script \"{fn}\"\n" +
                error.as_string(),
                exec_ctx
            ))

        return RTResult().success(Number.null)


    execute_run.arg_names = ["fn"]

BuiltInFunction.run					= BuiltInFunction("run")


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

    def visit_StringNode(self, node, context):
        return RTResult().success(
            String(node.tok.value).set_context(context).set_pos(node.start, node.end)
        )

    def visit_IfNode(self,node,context):
        res = RTResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition,context))
            if res.error: return res
            if condition_value.value:
                expr_value = res.register(self.visit(expr,context))
                if res.error: return res
                return res.success(expr_value)

        if node.else_case:
            else_value = res.register(self.visit(node.else_case,context))
            if res.error: return res
            return res.success(else_value)

        return res.success(None)

    def visit_ForNode(self, node, context):
        print("visiting fornode")
        res = RTResult()
        elements = []

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

            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res

        return res.success(
            List(elements).set_context(context).set_pos(node.start, node.end)
        )

    def visit_WhileNode(self, node, context):
        res = RTResult()
        elements = []

        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res

            if not condition.value ==1 : break

            elements.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res

        return res.success(
            List(elements).set_context(context).set_pos(node.start, node.end)
        )

    def visit_ListNode(self, node, context):
        res = RTResult()
        elements = []

        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context)))
            if res.error: return res

        return res.success(
            List(elements).set_context(context).set_pos(node.start, node.end)
        )

    def visit_OutputNode(self, node, context):
        res = RTResult()
        to_output = res.register(self.visit(node.output_value, context))
        print(to_output.value)
        return res.success(
            String(to_output.value).set_context(context).set_pos(node.start, node.end)
        )

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
        print("binopnode found")
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

    def visit_ProcFuncDefNode(self, node, context):
        res = RTResult()

        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.start,
                                                                                            node.end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)

        return res.success(func_value)

    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []

        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error: return res
        value_to_call = value_to_call.copy().set_pos(node.start, node.end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res

        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return res.success(return_value)
##############################################
## RUN
##############################################

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number.null)
global_symbol_table.set("RUN", BuiltInFunction.run)

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

