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
{ GrayNoise.ar } => \gnoise;
\noise.fadeOut;
\noise.start;
{ SinOsc.ar(440) } =>.free \noise;

SynthTree.initTree;
*/

SynthTree : IdentityTree {
	
	classvar <default;
	classvar nameSpaces; // dictionaries holding the SynthTree instances by server
    
	var <synth;  // the synth of this node
	var <>inputs; // dictionary of input names and bus specs or busses
	var <outputName; // name of input where this synth sends it output
	var <output; // SynthTree instance where this synth sends its output
	var >group;  // optional group enclosing synth as well as 
	// synths of input subtree [!??!?]
	var <>template; // optional template for re-creating synth
    var <notStopped = true; // if false, do not restart on initTree or 
	// on chuck/replace
    var <>fadeTime = 0.1;
	var <>name;
	var <>args; // TODO: args sent to synth at creation time
	var <>replaceAction = \fadeOut; // only used by addInputSynth

	*initClass {
		StartUp add: {
			var server;
			server = Server.default;
			default = this.new(\root);
			nameSpaces = MultiLevelIdentityDictionary();
			nameSpaces[server, \root] = default;
			default.inputs = IdentityDictionary();
			default.inputs[\in] = Bus('audio', 0, 
				// server.options.numOutputBusChannels, 
				0, // trick the allocator: reserve 0 channels
				server);
			ServerBootCheck add: {
				default.group = server.asTarget;
				default.initTree(true);
            };
		}
	}

	*at { | symbol, createIfMissing = true |
		var synthTree;
		synthTree = this.nameSpaces[this.server, symbol];
		if (synthTree.isNil and: createIfMissing) {
			postf("Making new SynthTree for server %, name %\n", 
				this.server, symbol);
			synthTree = this.new(symbol);
			this.nameSpaces[this.server, symbol] = synthTree;
			this.root[synthTree.name] = synthTree;
		};
		^synthTree;
	}

	*new { | name | ^super.new.init(name); }

	init { | argName |
		name = argName;
		args = SynthTreeArgs();
	}

	*nameSpaces {
		^nameSpaces ?? { nameSpaces = MultiLevelIdentityDictionary() };
	}

	server { ^ this.group.server }

	*server { ^ this.root.group.server }
	*root { ^ ~root ? default }

	*initTree { | remakeInputs = false | default.initTree(remakeInputs); }

    initTree { | remakeInputs = false |
		/*  Restart all synths.
			If the server just booted, then also allocate busses.
			This is run by default whenever the server boots.
			Otherwise it can be used to restart the entire tree's synths.
			Only nodes that have "notStopped" set to true will be restarted.
			Safety: If a synth is already running, do not restart.
		*/
		if (remakeInputs) { this.remakeInputs; };
		if (synth.isPlaying) { synth.free };
        if (notStopped) {
            this.makeSynth;
            this do: _.initTree(remakeInputs);
        }
    }

	isPlaying { ^synth.isPlaying }

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

	asSynthTree { /* ^this */ }

	chuckMakingInput { | synthOrTemplate, replaceAction = \fadeOut, 
		argFadeTime, startWhen = \now |
		this.makeInputs();
		this.chuck(synthOrTemplate, replaceAction, argFadeTime, startWhen);
	}

	makeInputs { | specs |
		var server, bus;
		server = this.server;
		specs = specs ?? { [in: 1] };
		switch (specs.class,
			Integer, { specs = [in: specs] },
			Symbol, { specs = [specs, 1] }
		);
		inputs = inputs ?? { IdentityDictionary(); };
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

	chuck { | synthOrTemplate, argReplaceAction = \fadeOut, 
		argFadeTime, startWhen = \now |
		/*  Set my synth.  Start depending on startWhen. */
		{
		if (synth.isPlaying) { 
			this.endSynth(argReplaceAction, argFadeTime ? fadeTime);
		};
		if (synthOrTemplate.isKindOf(Node)) {
			synth = synthOrTemplate;
			synth.set(\out, this.getOutputBusIndex).moveToHead(this.group);
			inputs do: _.moveBefore(synth);
		}{
			template = synthOrTemplate;
			switch (startWhen,
				\now, { notStopped = true; 
					this.makeSynth;
				},
				\ifNotStopped, { if (notStopped) { this.makeSynth } },
				\later, {} // any other symbol will also do
			)
		};
		}.fork;
	}

	moveBefore { | argSynth |
		// TODO: move my synth before the output synth
		// and then call moveBeforeOutput on all my inputs sunthTrees
		if (synth.isPlaying) {
			synth.moveBefore(argSynth); 
			this do: _.moveBefore(synth);
		};
	}

	setTemplate { | argTemplate, argReplaceAction = \fadeOut |
		/* set template without starting. Called by ==> operator
			Starting is deferred to after connecting 
			self as input to another SynthTree (=<). 
			Manner of replacing previous synth is stored in replaceAction */
		replaceAction = argReplaceAction;
		template = argTemplate;
	}

	addInputSynth{ | synthTree, inputName = \in, startWhen = \now |
		// TODO! TEST THIS
		/*  Add synthTree to my inputs and make it output its signal to my input.
			Add synthTree to your dictionary under its name, 
			THEN create the synth, using your group as target,
			addToHead as add method, and setting the output \out
			to one of your inputs, through args at synth creation time. 
		*/
		if (inputs.isNil) {
			postf("% has no inputs. Cannot add input.\n", name);
			^this;
		};
		if (this outputsTo: synthTree) {
			postf("% outputs to % and therefore cannot add it as input. Cycle!\n", 
			name, synthTree.name);
			^this
		};
		this[synthTree.name] = synthTree;
		synthTree.setOutput(this, inputName);
		if (startWhen === \now) { synthTree.start };
	}

	outputsTo { | synthTree |
		if (output.isNil) { ^false };
		if (output === synthTree) { ^true } { ^output outputsTo: synthTree };
	}

	setOutput { | synthTree, inputName = \in |
		var outputBus;
		outputBus = synthTree.getInputBus(inputName);
		if (outputBus.isNil) {
			postf("% has no input named %. Cannot output to it\n",
				synthTree.name, inputName);
			^this;
		};
		output = synthTree;
		outputName = inputName;
		if (synth.isPlaying) { synth.set(\out, outputBus.index) }
	}

	getInputBus { | inputName = \in |
		^inputs !? { inputs[inputName] };
	}

	endSynth { | argReplaceAction = \fadeOut, argFadeTime |
		if (argReplaceAction isKindOf: SimpleNumber) {
			synth.fadeOut(argReplaceAction);
		}{
			switch (argReplaceAction,
				\fadeOut, { synth.fadeOut(argFadeTime ? fadeTime) },
				\free, { synth.free }
			)
		}
	}

    makeSynth {
		synth = template.asSynth(this);
		// guarantee that moveBefore happens AFTER the synth has really started!
		synth !? {
			synth.onEnd(\this, {}); // This also registers on NodeWatcher
			this.addNotifierOneShot(synth, 'n_go', {
				this do: _.moveBefore(synth);
			});
		};
		notStopped = true;
	}

	synthArgs {
		/* return argument array for Synth.new / Function.play, containing
			the setters for the output and the input busses */
		var argsArray;
		argsArray = [out: this.getOutputBusIndex];
		inputs !? { 
			inputs keysValuesDo: { | key, bus |
				argsArray = argsArray add: key;
				argsArray = argsArray add: bus.index;
			};
		}
		^(args.asArray collect: _.synthArgs ++ argsArray).flat;
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
		if (synth.isPlaying) {
			synth.set(\timeScale, argFadeTime ? fadeTime, \gate, 0);
			synth.isPlaying = false;
		};
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

	mapSet { | parameter, value |
		// args connect themselves directly to controllers
		// this saves having to access the parameter each time. 
		// therefore this method may not be used.
		/* var param;
		param = args[parameter];
		param !? param.mapSet(value);
		*/
		
		[this, thisMethod.name, parameter, value].postln;
	}

	set { | ... argArgs |
		// Also store parameters in args
		argArgs keysValuesDo: { | key, value |
			args.storeArgValue(key, value);
		};
		if (synth.isPlaying) { synth.set(*argArgs) };
	}

	setSynthParameter { | parameter, value |
		// used by StreamPattern in args
		if (synth.isPlaying) { synth.set(parameter, value); };
	}

	// Controls
	/*
   .out(param = \out, chans = 1) // creates bus ref
   .in(param = \in, chans = 1) // creates bus ref
		.view(param, nameOrView = param) // , storeName = \view
   .osc(param, specs = param, storeName = \osc)
   .buf(name, param, chans) // creates buf ref
   .midi(param, specs, storeName = \midi)
   .map(name, param, chans) // creates bus ref
   // following compose patterns / streams. for later? ... ?
   .add(param, element, storeName, path);
   .sub(param, element, storeName, path);
   .mul(param, element, storeName, path);
   .div(param, element, storeName, path);
   .mod(param, element, storeName, path);
   .pow(param, element, storeName, path);
   .sel(param, element, storeName, path);
   .rej(param, element, storeName, path);
   .fun(param, element, storeName, path);
   .choose(param, element, path);
   .wchoose(param, element, path);
	*/
	view { | param, nameOrView |
		
	}
}
