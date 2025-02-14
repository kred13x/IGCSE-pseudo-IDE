import pythonconverter as pc
import streamlit as st


col1, col2, col3 = st.columns([0.5,0.2,0.3], gap="small")

with col1:
    code = st.text_area("Enter your pseudocode here:",height = 680)

def convertcode():
    pc.file_and_convert(code)
    with col3:
        formatted = pc.output().replace("\n", "<br>")
        st.markdown(formatted, unsafe_allow_html=True)

with col2:
    st.button("Convert to Python!", type="primary", on_click = convertcode)
    name = st.text_input("Enter a file name here before downloading:","pythoncode")
    file = open("python.txt",'r')
    st.download_button("Download your code as Python!", file, file_name=name+".py")
    file.close()

with col3:
    st.markdown("**Result goes here:**")