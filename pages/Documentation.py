import streamlit as st


st.subheader("Documentation")
st.markdown('''You can access tutorials on how to use this website and on writing IGCSE pseudocode here!''')
st.markdown("")

def readfile(fn):
    full_fn = 'documentation/'+fn+'.txt'
    with open(full_fn,'r') as file:
        return file.read()

st.markdown("**Using this IDE**")

#using an expander to make the documentation drop-down
with st.expander("Using the command line"):
    st.markdown(readfile('CommandLine'))

with st.expander("Using the code editor"):
    st.markdown(readfile('CodeEditor'))

with st.expander("Using the Python converter"):
    st.markdown(readfile('PythonConverter'))

st.markdown("")
st.markdown('''**Writing IGCSE Pseudocode**''')

with st.expander("Declaring and assigning variables"):
    st.markdown(readfile('DeclareAssignVar'))

with st.expander("Output statements"):
    st.markdown(readfile('OutputStatements'))

with st.expander("Arithmetic expressions"):
    st.markdown(readfile('ArithExpr'))

with st.expander("Comparison expressions"):
    st.markdown(readfile('CompExpr'))

with st.expander("Logic expressions"):
    st.markdown(readfile('LogicExpr'))

with st.expander("If statements"):
    st.markdown(readfile('IfStatements'))
