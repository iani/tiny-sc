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
	classvar <selected; /* Current synthtree to act on
		Usually selected on Faders pane.  Used to chuck new 
		templates into, either as template or as input. 
	*/
	var <synth;  // the synth of this node
	var <>inputs; // dictionary of input names and bus specs or busses
	var <outputName; // name of input where this synth sends it output
	var <output; // SynthTree instance where this synth sends its output
	var >group;  // optional group enclosing synth as well as
	// synths of input subtree [!??!?]
	var <>template; // optional template for re-creating synth
    var <>notStopped = true; // if false, do not restart on initTree or
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
			ServerBootCheck add: { // most reliable way to check server boot
				default.group = server.asTarget;
				BufferFunc.initBuffers(server);
				{ BufferFunc.postBufNames }.defer(3);
				default.initTree(true);
				this.changed(\serverBooted);
            };
			Spec.specs.at(\amp).default = 0.1;
			Spec.specs.[\trigRate] = [0.1, 50, 'exp', 0, 1, ""].asSpec;
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
			this.changed(\newSynthTree, synthTree);
		};
		^synthTree;
	}

	*onServer { | argServer |
		// Return synthtrees created on a given server
		argServer = argServer ?? { this.server };
		^this.nameSpaces.at(argServer);
	}

	*new { | name |
		^super.new.init(name);
	}

	init { | argName |
		name = argName;
		args = SynthTreeArgs(this);
	}

	*nameSpaces {
		^nameSpaces ?? { nameSpaces = MultiLevelIdentityDictionary() };
	}

	server { ^ this.group.server }

	*server { ^ this.root.group.server }
	*root { ^ ~root ? default }

	*stopAll { | startNode, argFadeTime = 0.1 |
		(startNode ? default).stopAll;
	}

	stopAll { | argFadeTime = 0.1 |
		this.fadeOut(argFadeTime);
		this do: _.stopAll(argFadeTime);
	}

	*initTree { | remakeInputs = false |
		default.notStopped = true;
		default.initTree(remakeInputs);
	}

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

	trig { | ... someArgs |
		// restart, ending previous synth if running
		someArgs.keysValuesDo({ | key, value |
			args.storeArgValue(key, value);
		});
		this.chuck;
	}
	chuck { | synthOrTemplate, argReplaceAction = \fadeOut, argFadeTime |
		/*  Set my template.  Start synth. Replace previous one. */
		notStopped = true;
		if (synthOrTemplate.isKindOf(Node)) {
			synth = synthOrTemplate;
			synth.set(\out, this.getOutputBusIndex).moveToHead(this.group);
			inputs do: _.moveBefore(synth);
		}{
			template = (synthOrTemplate ? template).asSynthTemplate(this);
			this.makeInputs(template.inputSpecs);
			if (synth.isPlaying) {
				this.endSynth(argReplaceAction, argFadeTime ? fadeTime);
			};
			this.makeSynth;
		};
	}

	moveBefore { | argSynth |
		// TODO: move my synth before the output synth
		// and then call moveBeforeOutput on all my inputs sunthTrees
		if (synth.isPlaying) {
			synth.moveBefore(argSynth);
			this do: _.moveBefore(synth);
		};
	}

    makeSynth { | attackTime |
		synth = template.asSynth(this, attackTime);
		// guarantee that moveBefore happens AFTER the synth has really started!
		synth !? {
			synth.onEnd(\this, { this.changed(\stopped) }); // This also registers on NodeWatcher
			this.addNotifierOneShot(synth, 'n_go', {
				this do: _.moveBefore(synth);
				this.changed(\started);
			});
		};
		notStopped = true;
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
		^output.inputBusIndex(outputName) ? 0;
	}

	inputBusIndex { | inputName = \in |
		if (inputs.isNil) { ^0 };
		if (inputs.size < 2) {
			^inputs.asArray.first.index;
		}{
			^inputs[inputName].index;
		}
	}

	toggle { | fadeTime | 
		if (this.isPlaying) { this.fadeOut(fadeTime) } { this.start(fadeTime) } }

    start { | attackTime |
		// start, but only if synth is not playing
		if (synth.isPlaying) { } { this.makeSynth(attackTime) };
	}

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
		if (synth.isPlaying) { synth.free; };
		notStopped = false;
	}

	mute {
		// possibly divert output to a "sink" bus
		[this, thisMethod.name, "not implemented"].postln;
	}

    group {
        ^group ?? {
            if (output.isNil) { Server.default.defaultGroup } { output.group }
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
		// used by MultiControl etc.
		if (synth.isPlaying) { synth.set(parameter, value); };
	}

	specs { | eventOrArrayOfSpecs |
		// Create argument parameters with their specs
		eventOrArrayOfSpecs keysValuesDo: { | param, spec |
			args.makeParam(param, spec);
		}
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
		.view(param, name, view ...) // name etc. optional. creates knob per default
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
	
	knobs {
		Knobs.getPanel(name).front;
		this.getArgsFromTemplate do: _.addView;
	}

	getArgsFromTemplate {
		^template.templateArgs.reject({ | cName |
			cName.rate === \scalar or: { [\gate, \out] includes: cName.name }
		}) collect: { | cName |
			args.getParam(cName.name, nil, cName.defaultValue);
		};
	}

	view { | param, viewName, view, func, onClose, enabled = true |
		// only param is obligatory. All others are provided by MultiControl
		args.getParam(param)
		.addView(viewName, view, func, onClose, enabled);
	}
	buf { | bufName, param = \buf |
		args.getParam(param).setBuffer(bufName ? param)
	}

	*faders { | argServer |
		var all, panel;
		argServer ?? { argServer = SynthTree.server };
		all = SynthTree.onServer(argServer);
		panel = Sliders.getPanel(argServer.asSymbol);
		panel.sliders do: { | s |
			s.label.canReceiveDragHandler = { View.currentDrag isKindOf: Template };
			s.label.receiveDragHandler = {
				var name;
				if (s.label.object isKindOf: String) {
					name = View.currentDrag.makeSynthTreeName;
					View.currentDrag.template => name;
					s.label.string = name;
				};
			};
			s.label.focusGainedAction = { | me |
				selected = SynthTree.at(me.string.asSymbol, false);
			};
			s.slider.keyDownAction = { | view, char |
				switch (char,
					$b, { BufferList.showList('Create Buffer Player', argServer); },
					$,, { thisProcess.stop },
					$., { SynthTree.stopAll },
					$i, { SynthTree.initTree },
					$/, { SynthTree.initTree }
				)
			};
		};
		all.keys.asArray.select({ | name | name != \root }).sort
		do: { | name | all[name].prFader(panel) };
		panel.addNotifier(this, \newSynthTree, { | synthTree | this.faders });
		^panel;
	}

	prFader { | panel |
		/* Make a fader for amp on a Sliders panel. 
			Note: This is a private class.  It is called via notification
			whenever a new SynthTree is created and an amp faders panel is open.
		*/
		var param;
		panel = panel ?? { Sliders.getPanel(this.server.asSymbol) };
		param = args.getParam(\amp);
		param.addView(\fader, 
			param.connectParamView(panel.sliderLabeled(name))
		);
	}

	// under development
	fade { | duration = 1, target = 0 |
		// fade amplitude with a line ugen from current value to 
		// target value in duration seconds
		this.map(\amp, target@duration);
	}

	map { | param, curve | 
		/*  Fade any parameter to any value(s) using a line or envelope ugen
           on a control bus, mapped to the parameter. See MultiControl:map for details.
		*/
		this.getParam(param).map(curve);
	}

	bufferList {
		// From BufferList. Select buffer from list and play it
		var buffers, keys;
		buffers = Library.at(this.server);
		keys = buffers.keys.asArray.select({ | b | buffers[b].path.notNil }).sort;
		Windows.for(this, \bufferList, { | window |
			var list;
			window.view.layout = VLayout(
				list = ListView().items_(keys);
			);
			list.action = { | me | window.changed(\buffer, me.item); };
			list.keyDownAction = { | view, char, modifiers, unicode, keycode, key |
				switch (char,
					13.asAscii, {
						if (this.isPlaying) {
							this.stop;
						}{
							{ \buf.playBuf } => this.buf(view.item)
							.set(\amp, 1)
							.set(\loop, if (modifiers == 0) { 0 } { 1 });
						}
					},
					Char.space, { Library.at(this.server, view.item).play; },
					$l, { SynthTree.faders; },
					{ view.defaultKeyDownAction(
						char, modifiers, unicode, keycode, key) 
					}
				)
			};
		});
	}

	// Emacs interaction

	*chuckSelectingSynthTree { | argServer |
		/* Eval current snippet or org section as function and chuck into 
		synthtree selected or input interactively in Emacs. */
		Emacs.selectEvalSnippet(
			this.synthTreeNames(argServer),
			"{ %s } => '%s'",
			"Chuck snippet into SynthTree (default: %s): "
		)
	}

	*knobsSelectingSynthTree { | argServer |
		/* Select a synthtree in Emacs and show its knobs window. */
		Emacs.selectEval(
			this.synthTreeNames(argServer),
			"'%s'.knobs",
			"Chuck snippet into SynthTree (default: %s): "
		)
	}

	*toggleSelectingSynthTree { | argServer |
		/* Toggle run status (start/stop) of a synthtree interactively
			selected in Emacs.  Selection defaults to last selected synthtree . */
		Emacs.selectEval(
			this.synthTreeNames(argServer),
			"'%s'.toggle",
			"Toggle SynthTree (default: %s): ",
			true
		)
	}

	*startSelectingSynthTree { | argServer |
		/* Start a synthtree interactively
			selected in Emacs.  Selection defaults to last selected synthtree . */
		Emacs.selectEval(
			this.synthTreeNames(argServer),
			"'%s'.start",
			"Start SynthTree (default: %s): ",
			true
		)
	}

	*fadeOutSelectingSynthTree { | argServer, fadeTime |
		/* Fadeout a synthtree interactively
			selected in Emacs.  Selection defaults to last selected synthtree . 
		Universal (C-U) argument value specifies fadeout time in seconds. */
		Emacs.selectEval(
			this.synthTreeNames(argServer),
			"'%s'" ++ format(".fadeOut(%)", if (fadeTime.isNil) { "" } { fadeTime }),
			"Fadeout SynthTree (default: %s): ",
			true
		)
	}

	*synthTreeNames { | argServer, removeRoot = true |
		var names;
		names = SynthTree.onServer(argServer).asArray collect: _.name;
		if (removeRoot) { names remove: \root };
		^names;
	}
	
}