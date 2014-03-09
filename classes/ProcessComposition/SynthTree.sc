/*
Access and modify the structure of a tree of synths, with their busses and groups.  
Each node of the tree holds a synth, and if present, the group, and input-output
control names and busses.

Each server has one root SynthTree, whose root branch contains the RootNode.
There is one global root tree, that holds the root trees of all servers. 

For more, see separate doc.

IZ Thu, Mar  6 2014, 21:51 EET

SynthDef("test", { WhiteNoise.ar().adsrOut }).add;
"test" => \test;
\test.fadeOut(5);

{ WhiteNoise.ar() } => \noise;
{ SinOsc.ar(440) } =>.5 \noise;
{ PinkNoise.ar } =>.3 \noise;
{ GrayNoise.ar } => \noise;
\noise.fadeOut;
\noise.start;
{ SinOsc.ar(440) } =>.free \noise;

*/

SynthTree : IdentityTree {
	
	classvar <default;
	classvar nameSpaces; // dictionaries holding the SynthTree instances by server
    
	var <synth;  // the synth of this node
	var <inputs; // dictionary of input names and bus specs or busses
	var <outputName; // name of input where this synth sends it output
	var <output; // SynthTree instance where this synth sends its output
	var >group;  // optional group enclosing synth as well as 
	// synths of input subtree [!??!?]
	var <template; // optional template for re-creating synth
    var <notStopped = true; // if false, do not restart on initTree or 
	// on chuck/replace
    var <fadeTime = 0.1;
	var <>name;

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
			postf("Making new Synthtree for server %, name %\n", 
				this.server, symbol);
			synthTree = this.new.name_(symbol);
			this.nameSpaces[this.server, symbol] = synthTree;
		};
		^synthTree;
	}

	*nameSpaces {
		^nameSpaces ?? { nameSpaces = MultiLevelIdentityDictionary() };
	}

	server { ^ this.group.server }

	*server { ^ this.root.group.server }
	*root { ^ ~root ? default }

    initTree {
		this.remakeInputs;
        if (notStopped) {
            this.makeSynth;
            this do: _.initTree;
        }
    }

	remakeInputs {
		/* Remake input busses when a server reboots. Called by initTree */
		var server, bus;
		if (inputs.isNil) { ^this };
		server = this.server;
		// keysValuesDo could produce errors because of operating 
		// on array being modified
		inputs.keys do: { | key |
			bus = inputs[key];
			inputs[key] = Bus.audio(server, bus.numChannels);
		};
	}

	asSourceTree { /* ^this */ }

	chuckMakingInput { | synthOrTemplate, replaceAction = \fadeOut, 
		argFadeTime, alwaysStart = true |
		this.makeInputs();
		this.chuck(synthOrTemplate, replaceAction, argFadeTime, alwaysStart)
	}

	makeInputs { | specs |
		var server, bus;
		server = this.server;
		specs = specs ?? { [in: 1] };
		switch (specs.class,
			Integer, { specs = [in: specs] },
			Symbol, { specs = [specs, 1] }
		);
		inputs = IdentityDictionary();
		specs keysValuesDo: { | key, numChans |
			bus = inputs[key];
			if (bus.isNil) {
				inputs[key] = Bus.audio(server, numChans);
			}{
				if (bus.numChannels != numChans) {
					bus.free;
					inputs[key] = Bus.audio(server, numChans);
				}
			}
		};
	}

	chuck { | synthOrTemplate, replaceAction = \fadeOut, 
		argFadeTime, startWhen = \now |
		if (synth.isPlaying) { 
			this.endSynth(replaceAction, argFadeTime ? fadeTime)
		};
		if (synthOrTemplate.isKindOf(Node)) {
			synth = synthOrTemplate;
			synth.set(\out, this.getOutputBusIndex).moveToHead(this.group);
		}{
			template = synthOrTemplate;
			switch (startWhen,
				\now, { this.makeSynth },
				\ifNotStopped, { if (notStopped) { this.makeSynth } },
				\later, {} // any other symbol will also do
			)
		};
	}

	addInputSynth{ | synthTree, inputName = \in, startWhen = \now |
		// TODO! TEST THIS
		/*  Add synthTree to your dictionary under its name, 
			THEN create the synth, using your group as target,
			addToHead as add method, and setting the output \out
			to one of your inputs, through args at synth creation time. 
		*/
		this[synthTree.name] = synthTree;
		if (startWhen === \now) { synthTree.start }
	}

	endSynth { | replaceAction = \fadeOut, argFadeTime |
		if (replaceAction isKindOf: SimpleNumber) {
			synth.fadeOut(replaceAction);
		}{
			switch (replaceAction,
				\fadeOut, { synth.fadeOut(argFadeTime ? fadeTime) },
				\free, { synth.free }
			)
		}
	}

    makeSynth {
		synth = template.asSynth(this);
		synth.onEnd(\this, {}); // just keep track of isPlaying state
		notStopped = true;
	}

	synthArgs {
		/* return argument array for Synth.new / Function.play, containing
			the setters for the output and the input busses */
		var out;
		out = [out: this.getOutputBusIndex];
		inputs !? { 
			inputs keysValuesDo: { | key, bus |
				out = out add: key;
				out = out add: bus.index;
			};
		}
		^out;
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
		[this, thisMethod.name, "not implemented"].postln;
	}

    group {
        ^group ?? {
            if (output.isNil) { default.group } { output.group }
        }
    }
}
