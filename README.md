# eww

This project is being written by Guillaume Duboc.

Its aim is to build a distributed hash table using a ring-shaped topology
of erlang nodes.

## Usage

Compile the modules using `make`. Then, you can use the `agent` module to spawn a topology
and manage the nodes of the network.

## Architecture

### Agent

The `agent.erl` module contains all the functions for spawning a topology with
listening agents. The supported operations on the agents are :
	
- KILL : kill a specific node and reshape the ring
- DESTROY : destruct all the nodes in the topology
- JOIN / NEW_NODE : spawn a node and make it join the topology
- PING : pass a ping through the network

## Communication system

### Shape of the messages

The nodes communicate with three different type of message :
	
- COMMANDS `{cmd, Cmd, Id}`
- PACKETS `{pack, Msg, Id}`
- ACKNOWLEDGEMENTS `{ack, Id}`
- PID `{pid, Pid}`

The main difference between COMMANDS and PACKETS are that commands can be 
adressed to a single node of the network whereas packets should run through
the entire network.

The `Id` is a hash of the node's Pid and processor time obtained with `now()`.
It is used to check if a message was received before.

ACKNOWLEDGEMENTS, though not implemented yet should be used in order to confirm
no messages were lost, and eventually to detect that a node is sleeping/dead.

### Detecting failure

In order to detect failing nodes, I had two main ideas :

- implement an acknowledgement system where each communication would receive a confirmation. If a node doesn't acknowledge, 
- after some time, it is considered dead/sleeping
- implement periodic checks :
    - local checks, where a node checks on its neighboor
    - global checks, where an elected node passes a token through the ring and launches a restructuration if the token doesn't go through

Before implementing, I decided that I prefered the latter option. In the first option, the number of acknowledgements would scale linearly with the number of API calls, and I don't think that's a good thing.


