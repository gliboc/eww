Question 1

- [x] a) Agent
- [x] b) Topology
- [x] c) Join
- [x] d) Kill
- [ ] e) / Use 'Naming processes' section of LYEFGG



Question 2

- [x] a) `com.erl`
- [ ] b)
- [ ] c)



Question 3

- [x] a) `handle_data`
- [x] b) done using `retrieve_data` which is called when a node receive a request and hash the key in its keys. If it hasn't, it sends it to its next peer. If the key is never found, `fail_msg` warns the client. Todo : recycle `fail_msg`
- [x] c) The terminate function transfers all of a node's data and keys to its peer. It sends pre-signed data to its peer, so the peer doesn't have to generate an UUID; it just writes the data and stores the key as its own.
- [x] d) A message, {del, Key}, can be passed around to delete the associated File from all nodes of the network.
- [ ] e)
- [ ] f)
- [ ] g)



Question 4 CLI `client.erl`

- [x] a) Function `client:start`. Very basic atm
- [x] b) Function `client:retire`. Sketchy at best.
- [x] c) `client:push` function. Very bad atm : just sends data to the first node it knows about.
- [x] d) This is the `client:pull` function.
- [x] e) Function `client:release`, using a msg structure caught by `handlers:handle_msg` , treated by `handlers:process_msg`, executed by `transfer:delete_data`
- [x] f) Using `client:ping` : the ping goes through all the network, adding up the number of nodes, the number of hashes kept (ie the number of messages that passed through the network), and the number of keys (ie the number of files that are being kept). 
  - [ ] Todo: find a way to give out the total size of the files, to give node-specific information (by naming them ?), and to find the size of the biggest/lowest file that is being stored. Other cool data that would require nodes to maintain an history : size of data that was exchanged since the network started, number of files since the network started, number of push/pull operations, up-time of each node, ping time of each node in ms, ping time of the global network.
- [x] g) `client:deploy` deploy a ring-shaped topology of nodes using the `Nodes` list of adresses.



Others:

- [ ] Implement authentification using indigo-dc/oidcc erlang implementation of the OpenId Connect protocol