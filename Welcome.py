import streamlit as st

st.header("**Welcome to the IGCSE Pseudocode Interpreter!**")
st.markdown("To start, click on any of these buttons:")
st.page_link("pages/Command Line.py", icon = "💻")
st.markdown("The **command line** allows you to enter **singular** lines of IGCSE pseudocode and get the result of each line. Perfect for shorter programs or just for testing small features!")
st.markdown("")
st.page_link("pages/Code Editor.py", icon = "📝")
st.markdown("The **code editor** allows you enter **multiple** lines of IGCSE pseudocde and run all the lines at once. Suited for longer programs and for practising formal IGCSE questions!")
st.markdown("")
st.page_link("pages/Documentation.py", icon = "📑")
st.markdown("The **documentation** provides the IGCSE pseudocode syntax, as well as helpful tutorials!")
st.markdown("")
st.markdown("⬅️ You can also access all these pages via the sidebar!")