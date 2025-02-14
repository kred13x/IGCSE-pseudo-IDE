#imports the pseudocode interpreter code to use its tokenisation process
import pseudointerpreter as p

tok_list = []

#imports the scan function from the main interpreter program and turns the code into tokens
def scan(text):
    scanner = p.Scanner('<stdin>', text)
    tok_list, error = scanner.make_tokens()
    print(tok_list)
    return tok_list

#interprets the given tokens
#done in a function so the user interface file can easily call it
def convert(tokens, file):
    pos=0
    #loops until the end of all the given tokens
    while pos < len(tokens):
        #writes to the file depending on the token for tokens that are the same between pseudo and python
        if tokens[pos].type == p.TT_EOF:
            pos+=1
            pass
        elif tokens[pos].type == p.TT_NEWLINE:
            pos+= 1
            file.write('\n')
        elif tokens[pos].type == p.TT_INT or tokens[pos].type == p.TT_REAL:
            file.write(str(tokens[pos].value))
            pos+=1
        elif tokens[pos].type == p.TT_STRING:
            file.write("\""+tokens[pos].value+"\"")
            pos+=1
        elif tokens[pos].type == p.TT_PLUS:
            file.write(' + ')
            pos+=1
        elif tokens[pos].type == p.TT_MINUS:
            file.write(' - ')
            pos+=1
        elif tokens[pos].type == p.TT_ASTERISK:
            file.write(' * ')
            pos+=1
        elif tokens[pos].type == p.TT_SLASH:
            file.write(' / ')
            pos+=1
        elif tokens[pos].type == p.TT_LPAREN:
            file.write(' ( ')
            pos+=1
        elif tokens[pos].type == p.TT_RPAREN:
            file.write(' ) ')
            pos+=1
        elif tokens[pos].type == p.TT_EQUAL:
            file.write(' == ')
            pos+=1
        elif tokens[pos].type == p.TT_GREATERTHAN:
            file.write(' > ')
            pos+=1
        elif tokens[pos].type == p.TT_LESSTHAN:
            file.write(' < ')
            pos+=1
        elif tokens[pos].type == p.TT_GREATEREQ:
            file.write(' >= ')
            pos+=1
        elif tokens[pos].type == p.TT_LESSEQ:
            file.write(' <= ')
            pos+=1
        elif tokens[pos].type == p.TT_NOTEQ:
            file.write(' != ')
            pos+=1
        elif tokens[pos].type == p.TT_IDENTIFIER:
            if (pos+1) < len(tokens):
                #differentiating from whether the identifier is used in an assign statement
                #or if it's used for other purposes (eg OUTPUT statement)
                if tokens[pos+1].type == p.TT_ASSIGN:
                    var_name = tokens[pos].value
                    pos+=1
                    pos+=1
                    expr = []
                    #fills the array with all following tokens until the end of the line
                    while tokens[pos].type != p.TT_NEWLINE and tokens[pos].type != p.TT_EOF:
                        expr.append(tokens[pos])
                        pos+=1
                    file.write(var_name + ' = ')
                    #using recursion to convert the tokens in the expr array
                    convert(expr, file)
                elif tokens[pos].value == 'ENDIF':
                    pos+=1
                else:
                    file.write(tokens[pos].value)
                    pos+=1
            elif tokens[pos].value == 'ENDIF':
                pos += 1
            else:
                file.write(tokens[pos].value)
                pos+=1

        elif tokens[pos].type == p.TT_KEYWORD:
            if tokens[pos].value == 'DECLARE':
                pos += 1
                identifier_name = tokens[pos].value
                pos += 1
                pos += 1
                if tokens[pos].value == 'INT' or tokens[pos].value == 'INTEGER':
                    var_value = '0'
                elif tokens[pos].value == 'REAL':
                    var_value = '0.0'
                elif tokens[pos].value == 'STRING':
                    var_value = "\"\""
                file.write(identifier_name + ' = ' + var_value)
                pos+=1

            elif tokens[pos].value == 'OUTPUT':
                pos+= 1
                expr = []
                #fills the expr array with all following tokens until the end of the line
                while tokens[pos].type != p.TT_NEWLINE and tokens[pos].type != p.TT_EOF:
                    expr.append(tokens[pos])
                    pos+=1
                file.write("print(")
                #using recursion to convert tokens in the expr array
                convert(expr,file)
                #adds the closing parentheses at the end of the Python print statement
                file.write(')')

            elif tokens[pos].value == 'THEN':
                file.write(": ")
                pos+= 1

            elif tokens[pos].value == "ENDIF":
                pos+=1

            else:
                file.write(str(tokens[pos].value.lower())+" ")
                pos+=1


def output():
    #reads from the file and returns its contents
    #streamlit has a specific formatting for displaying text on the website
    #so printing immediately here will not be shown on the webiste
    with open("python.txt",'r') as file:
        return file.read()

#the file handling and writing is done in a singular function, making it easier for the user interface file to call
def file_and_convert(text):
    #if python.txt doesn't exist yet, creates a new file
    #all python code is written to this file
    file = open("python.txt", "w")
    tok_list = scan(text)
    convert(tok_list,file)
    file.close()


