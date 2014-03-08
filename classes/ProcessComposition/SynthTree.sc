/*
Access and modify the structure of a tree of synths, with their busses and groups.  
Each node of the tree holds a synth, and if present, the group, and input-output
control names and busses.

Each server has one root SynthTree, whose root branch contains the RootNode.
There is one global root tree, that holds the root trees of all servers. 

For more, see separate doc.

IZ Thu, Mar  6 2014, 21:51 EET
*/

SynthTree : IdentityTree {
	
	classvar servers;

	var <synth;  // the synth of this node
	var <inputs; // dictionary of input names and bus specs or busses
	var <outputName; // name of output
	var <outputBus;
	var <outputSynth;
	var <group;  // optional group enclosing synth as well as synths of input subtree [!??!?]
	var <template; // optional template for re-creating synth

	*add { | name, template, target, replaceMethod = \fadeOut |
		
\\		if (synth.notNil) { synth.perform(replaceMethod); } 
		var synthNode;
		synthNode = servers.at(target.asTarget.server, name);
	}

}