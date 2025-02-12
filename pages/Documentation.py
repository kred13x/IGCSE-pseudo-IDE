import streamlit as st

st.subheader("Documentation")

with st.expander("1. Declaring and Assigning Variables"):
    st.markdown('''
**DECLARING AND ASSIGNING VARIABLES**

In IGCSE pseudocode, all variables must be declared before being used. Declaration statements are usually all done at the very beginning of the program, and essentially create empty variables. They are done in this format:

\tDECLARE <variable name> : <DATA TYPE>

This pseudocode interpreter currently supports the data types INT (INTEGER is also accepted), REAL, STRING and BOOLEAN.

This means you can create declaration statements like:

\tDECLARE num1 : REAL
\tDECLARE name : STRING

Once you have a variable declared, you can assign a value to it. This is done in this format:

\t<variable name> ← <value> (In this pseudocode interpreter, the ‘←’ symbol should be typed as ‘<-’)

Make sure that the given value aligns with the declared data type. With declaration and assignment statements, you can create a simple program such as this:

\tDECLARE height : REAL
\tDECLARE age : INTEGER
\tDECLARE name : STRING

\theight ← 165.2
\tage ← 19
\tname ← “Amy”
    ''')

with st.expander("2. If statements"):
    st.markdown('''
**IF STATEMENTS**

IF statements in IGCSE pseudocode executes certain line(s) of code if a condition is met. They are formatted as so:

\tIF <condition> THEN <statement> ENDIF

Or, if you want to execute multiple statements:

\tIF <condition> THEN
\t\t<statement 1>
\t\t<statement 2>
\t\t<statement 3>
\tENDIF

IF statements may also include an ELSE case. The ELSE case will be executed if the condition is not met. With the ELSE statement, you can nest another IF statement inside to check multiple conditions. With these methods, you can create a program like this:

\tIF grade >= 80 THEN
\t\tOUTPUT “You got an A!”
\tENDIF
\tELSE IF grade >= 70 THEN
\t\tOUTPUT “You got a B!”
\tENDIF
\tELSE IF grade >= 60 THEN
\t\tOUTPUT “You got a C!”
\tENDIF
\tELSE
\t\tOUTPUT “You failed :(“
\tENDIF
    ''')
