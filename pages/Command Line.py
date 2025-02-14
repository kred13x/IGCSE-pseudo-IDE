import pseudointerpreter
import streamlit as st

def click_button():
    st.session_state.messages = []
    st.session_state.button = False

# Button to reset program history
st.button('Reset program history', on_click=click_button, type="primary")

# Initialize session state for messages
if "messages" not in st.session_state:
    st.session_state.messages = []

# Display chat messages from history on app rerun
for message in st.session_state.messages:
    with st.chat_message(message["role"]):
        st.markdown(message["content"])

# Handle user input
text = st.chat_input("Enter code here", key="runtext")

if text:
    # Display user message in chat message container
    st.chat_message("user").markdown(text)
    # Add user message to chat history
    st.session_state.messages.append({"role": "user", "content": text})

    if text.strip() == "":
        # Skip empty input
        st.warning("Please enter valid code.")
    else:
        result, error = pseudointerpreter.run('<stdin>', text)

        # Display the results or error messages
        if error:
            with st.chat_message("assistant"):
                st.error(error.as_string())
            st.session_state.messages.append({"role": "assistant", "content": error.as_string()})
        elif result:
            if len(result.elements) == 1:
                with st.chat_message("assistant"):
                    st.markdown(repr(result.elements[0]))
                st.session_state.messages.append({"role": "assistant", "content": repr(result.elements[0])})
            elif len(result.elements) > 0:
                for element in result.elements:
                    with st.chat_message("assistant"):
                        st.markdown(repr(element))
                    st.session_state.messages.append({"role": "assistant", "content": repr(element)})
