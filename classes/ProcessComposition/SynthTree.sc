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
	
	classvar <default;
	classvar nameSpaces; // dictionaries holding the SynthTree instances by server
    
	var <synth;  // the synth of this node
	var <inputs; // dictionary of input names and bus specs or busses
	var <outputName; // name of input where this synth sends it output
	var <output; // SynthTree instance where this synth sends its output
	var >group;  // optional group enclosing synth as well as synths of input subtree [!??!?]
	var <template; // optional template for re-creating synth
    var <notStopped = true; // if false, do not restart on initTree or on chuck/replace
    var <fadeTime = 0.1;

	*initClass {
		StartUp add: {
			default = this.new;
			ServerBootCheck add: {
				default.group = RootNode();
				default.initTree;
            };
		}
	}

	*at { | symbol, createIfMissing = true |
		var synthTree;
		synthTree = this.nameSpaces[this.server, symbol];
		if (synthTree.isNil and: createIfMissing) {
			postf("Making new Synthtree for server %, name %\n", this.server, symbol);
			synthTree = this.new;
			this.nameSpaces[this.server, symbol] = synthTree;
		};
		^synthTree;
	}

	*nameSpaces {
		^nameSpaces ?? { nameSpaces = MultiLevelIdentityDictionary() };
	}

	*server { ^ this.root.group.server }
	*root { ^ ~root ? default }

    initTree {
        if (notStopped) {
            this.makeSynth;
            inputs do: _.initTree;
        }
    }

	asSourceTree { /* ^this */ }

	chuck { | synthOrTemplate, replaceAction = \fadeOut, fadeOut |
		if (synth.isPlaying) { synth.perform(replaceAction, fadeOut ? fadeTime) };
		if (synth.isKindOf(Node)) {
			synth.set(\out, this.getOutputBusIndex).moveToHead(this.group);
		}{
			template = synthOrTemplate;
			if (notStopped) { this.makeSynth };
		};
	}

    makeSynth {
		synth = template.asSynth(this);
		synth.onEnd(\this, {}); // just keep track of isPlaying state
		notStopped = true;
	}

	getOutputBusIndex {
		^output.inputBusIndex(outputName);
	}

	inputBusIndex { | inputName = \in |
		if (inputs.isNil) { ^0 };
		if (inputs.size < 2) {
			^inputs.asArray.first.index;
		}{
			^inputs[inputName].index;
		}
	}

    start { if (synth.isPlaying) { } { this.makeSynth }; }

	release { | argFadeTime | 
		synth release: argFadeTime ? fadeTime;
		synth.isPlaying = false;
		notStopped = false;
	}

	fadeOut { | argFadeTime |
		synth.set(\timeScale, argFadeTime ? fadeTime, \gate, 0);
		synth.isPlaying = false;
		notStopped = false;
	}

	stop { this.free }

    free {
		synth.free;
		notStopped = false;
	}

	mute {
		
	}

    group {
        ^group ?? {
            if (output.isNil) { default.group } { output.group }
        }
    }
}
