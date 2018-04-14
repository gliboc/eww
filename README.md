# eww

This project is being written by Guillaume Duboc.

Its aim is to build a distributed hash table using a ring-shaped topology
of erlang nodes.

## Usage

### Client

Compile the modules using `erl -make`. This will compile the code and output
its results inside the `ebin/` directory. 

Then, type `erl -pa ebin` to start a shell, and use the defined functions
to spawn and manage a topology of nodes.

You could also type `make:all([load]).` from an erlang shell.

### Constellation of devices

In order to showcase the use of this program, I would like to create a list
of wired devices that would run the agent and constitute a shared data system 
between me and some friends that could host similar devices.

A list of such devices can be found in `devices.txt`. At some points, the 
client will be able to read into this file and spawn agents on the diffent
nodes of the topology.

The current list contains only one device :
- Raspberry Zero W, accessible at 90.66.179.32

## Architecture

### Agent

The `agent.erl` module contains all the functions for spawning a topology with
listening agents. The supported operations on the agents are :
	
- `kill` : kill a specific node and reshape the ring
- `destroy` : destruct all the nodes in the topology
- `join` / `new_node` : spawn a node and make it join the topology
- `ping` : pass a ping through the network

## Communication system

### Shape of the messages

The nodes communicate with three different type of message :
	
- commands `{cmd, cmd, id}`
- packets `{pack, msg, id}`
- acknowledgements `{ack, id}`
- pid `{pid, pid}`

The main difference between `commands` and `packets` are that commands can be 
adressed to a single node of the network whereas packets should run through
the entire network.

The `Id` is a hash of the node's Pid and processor time obtained with `now()`.
It is used to check if a message was received before.

`acknowledgements`, though not implemented yet should be used in order to confirm
no messages were lost, and eventually to detect that a node is sleeping/dead.

### Detecting failure

In order to detect failing nodes, I had two main ideas :

- implement an acknowledgement system where each communication would receive a confirmation. If a node doesn't acknowledge, 
- after some time, it is considered dead/sleeping
- implement periodic checks :
    - local checks, where a node checks on its neighboor
    - global checks, where an elected node passes a token through the ring and launches a restructuration if the token doesn't go through

Before implementing, I decided that I prefered the latter option. In the first option, the number of acknowledgements would scale linearly with the number of API calls, and I don't think that's a good thing.


