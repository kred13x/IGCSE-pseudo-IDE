import pseudointerpreter
import streamlit as st
run = True

class Main():
    def __init__(self):
        self.running = False
        self.current_text = None

main = Main()

def click_button():
    main.running = True
    main.current_text = text
    st.session_state.button = False

text = st.text_input("Enter your code here")
st.button('Run', on_click=click_button)

while text != None:
    current_text = text
    if main.current_text.strip() == "": continue
    result, error = pseudointerpreter.run('<stdin>', main.current_text)

    if error:
        st.write(error.as_string())
    elif result:
        if len(result.elements) == 1:
            st.write(repr(result.elements[0]))
        else:
            st.write(repr(result))