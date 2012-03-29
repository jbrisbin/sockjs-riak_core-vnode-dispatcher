# SockJS riak_core vnode Dispatcher

Similar to the misultin-based vnode dispatcher [1], this dispatcher demonstrates how to use 
riak_core to serve a SockJS-based application.

It's not a re-usable application so much as it is a reference application you can cut-and-paste 
from to add SockJS capabilities to your riak_core application to give you a super-high-performance, 
highly-scalable, highly-available real-time web application.

### Use

git clone this project, then do a `make`. When that's built, start the server with the special 
bash script `./node1.sh`. Then point a SockJS capable browser to `http://localhost:3000/static/ws.html`. 

You should see "Hello World!" if it works correctly. Check your browser's console output for more information.

[1] - https://github.com/jbrisbin/misultin-riak-core-vnode-dispatcher