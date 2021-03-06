# eww

This project is being written by Guillaume Duboc.

Its aim is to build a distributed hash table using a ring-shaped topology
of erlang nodes.

## Usage

### Dependencies

This code uses https://github.com/avtobiff/erlang-uuid, which is imported and compiled 
using `rebar3`. `rebar3` is downloaded automatically from https://s3.amazonaws.com/rebar3/rebar3 when you launch `make` for the first time. This link is given at https://github.com/erlang/rebar3 and contains the latest compiled version for `rebar3`. 

### Client

Compile the modules using `make`. This will output the compiled code to the different `ebin/`
directories inside `_build/`. 

Then, type `make run` to start an erlang shell which imports this code, and use the defined functions
to spawn and manage a topology of nodes.

### Documentation

You can generate and visualize the documentation with `make doc`. The overview should open in 
a browser window. If you don't have a browser, the docs are generated in the `doc` folder.

### Constellation of devices

In order to showcase the use of this program, I would like to create a list
of wired devices that would run the agent and constitute a shared data system 
between me and some friends that could host similar devices.

A list of such devices can be found in `devices.txt`. At some points, the 
client will be able to read into this file and spawn agents on the diffent
nodes of the topology.

The current list contains only one device :
- Raspberry Zero W

## Architecture

### Agent

The `agent.erl` module contains all the functions for spawning a topology with
listening agents. An agent can be `terminated` to shut down and transfer its (key, data)
pairs to its next peer. It can be `destroyed`; in that case, the whole network self-destroys.
It can `join` a topology, and can be contacted in various ways as it handles commands, messages
and data transfers.

### Client

The client allows to communicate with a platform using any node's Pid. It can `push`
or `pull` files from it, ask for some files to be deleted with `release`, and `stop` nodes or `start` them. 

The `client.erl` module also offers the possibility to pass pass a `ping` through the network. The ping stores a lot of information that it gathers along its path, such as the number of nodes, the list of keys kept in the network, or the number of messages exchanged. If a ping ends correctly, it means that the whole system is sound.


### Topology

This application uses a ring-shaped topology that is mostly uni-directionnal. Each node is constantly linked with one peer, but can establish direct connections with the client when transferring data.

#### Fault tolerance

/!\ Disclaimer /!\ This part is not fully implemented yet: the `fault.erl` module has yet to be tested.

This topology is very useful for simply checking the integrity of the network: you can send a message throughout the whole system and see if it makes it through the ring. When it doesn't make it, one node at the cut extremity is asked to repair the network. It can be done by trying to respawn an agent on the unresponsive node. If it can't be done, there are two possibilities:

 - This node has a valid sequence of successor nodes, and it connects to its next node
 - If this doesn't work, the node that checked the sanity of the network can recover the address of the node on the other extremity, and give it to this node.

This last approach is problematic because it might create a forest of distinct rings, so I have disabled it until I can find a way to tackle this issue.

Therefore, the nodes are forced to regularly check if they have a list of correct successor nodes. This is done by the function `fault:check_successors` which is activated when the token `sanity_check` passes through the system. This token is regularly sent by a monitor node, which is spawned by the current leader of the network.

#### Communication system

The nodes communicate with four different type of message :
- commands `{cmd, Cmd, Ref}`
- packets `{pack, Msg, Ref}`
- data streams `{data, Msg, Ref}`
- acknowledgements `{ack, Ref}` and `{check, Ref}`

The main difference between `commands` and `packets` are that commands can be 
adressed to a single node of the network whereas packets should run through
the entire network.

The `Id` is a hash of the node's Pid and processor time obtained with `now()`.
It is used to check if a message was received before.

`acknowledgements` and `checks`, though not implemented yet, should be used in order to confirm
no messages were lost, and eventually to detect that a node is sleeping/dead.

### Detecting failure

In order to detect failing nodes, I had two main ideas :

- Implement an acknowledgement system where each communication would receive a confirmation. If a node doesn't acknowledge, 
  after some time, it is considered dead/sleeping
- Implement periodic checks :
    - local checks, where a node checks on its neighboor
    - global checks, where an elected node passes a token through the ring and launches a restructuration if the token doesn't go through

Before implementing, I decided that I prefered the latter option. In the first option, the number of acknowledgements would scale linearly with the number of API calls, and I don't think like that idea.


## Analysis and testing

### Type checking

This code is type-checked using `dialyzer`, which you can do by calling `make dialyzer`, provided `dialyzer` was initialized on your system.

### Tests

There are modules with the names `?MODULE_tests.erl` in order to test the most important functions in the app. For example, `transfer_tests.erl` tests the data transfer and other features.


## Going further

### Fault tolerance

My first approach was to try to come up with an asynchronous solution to faulty nodes. I would have 
liked to tie up a self-stabilization algorithm with a "safety-checking" node that would dynamically
check the sanity of the network and try to repair it.

But the paper [@fischer1985impossibility] discouraged me from doing so.

So I decided to resort to Erlang's supervisor model, that allow to report problems that
occurred in synchronous communications, and react accordingly. 

I wanted to try an asynchronous solution because I dislike having to duplicate communications to send
acknowledgements and I thought it would lead to faster communications in the network. 



