import pseudointerpreter
import streamlit as st

if "messages" not in st.session_state:
    st.session_state.messages = []

# Display chat messages from history on app rerun
for message in st.session_state.messages:
    with st.chat_message(message["role"]):
        st.markdown(message["content"])

while True:
    text = st.chat_input("Enter code here", key="runtext")
    if text:
        # Display user message in chat message container
        st.chat_message("user").markdown(text)
        # Add user message to chat history
        st.session_state.messages.append({"role": "user", "content": text})


        if text.strip() == "": continue
        result, error = pseudointerpreter.run('<stdin>', text)

        if error:
            with st.chat_message("assistant"):
                st.markdown(error.as_string())
            st.session_state.messages.append({"role":"assistant","content": error.as_string()})
        elif result:
            if len(result.elements) == 1:
                with st.chat_message("assistant"):
                    st.markdown(repr(result.elements[0]))
                st.session_state.messages.append({"role":"assistant","content": result.elements[0]})
            elif len(result.elements)>0:
                for count in range(0,len(result.elements)):
                    with st.chat_message("assistant"):
                        st.markdown(repr(result.elements[count]))
                    st.session_state.messages.append({"role": "assistant", "content": result.elements[count]})
