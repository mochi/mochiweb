There are two ways to use websockets, active and passive.

In passive mode, you can call the blocking get_data() to read a frame 
from the websocket. Passive mode is best suited to a transmit-only
server, or where you read from the client then send, then read in a loop.

Active mode allows for fully asynchronous sending and receiving. 
Your process gets sent {frame, Msg} when messages arrive, and you can
call send at any time.
