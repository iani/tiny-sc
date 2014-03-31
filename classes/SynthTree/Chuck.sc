/* 
Chuck operator and variants.

IZ Sat, Mar  8 2014, 23:40 EET
*/

+ Object {
	=!> { | key |
		currentEnvironment.parent[key] = this;
	}

	=> { | chuckee, replaceAction = \fadeOut |
		// chuck a source to a synthTree and play
		//		^synthTree.asSynthTree.chuck(this, replaceAction);
		// New implementation: Sat, Mar 29 2014, 03:14 EET :
		^chuckee.receiveChuck(this, replaceAction);
	}
	/*  // now done automatically, see chuck. Sat, Mar 29 2014, 03:00 EET
	=<> { | synthTree, replaceAction = \fadeOut |
		// chuck, and create default input bus
		^synthTree.asSynthTree.chuckMakingInput(this, replaceAction);
	}
	*/
	// TODO:
	=< { | synthTree, inputName = \in |
		// add synthTree to the input synths of the receiver
		this.asSynthTree.addInputSynth(synthTree.asSynthTree, inputName)
	}

	==> { | synthTree, replaceAction = \fadeOut |
		// as => but do not start the synth now: 
		// synth gets started when the synthTree is added as input with =<
		synthTree = synthTree.asSynthTree;
		if (synthTree.isPlaying) {
			^synthTree.chuck(this, replaceAction);
		}{
			^synthTree.setTemplate(this);
		};
	}

	=|> { | synthTree, replaceAction = \fadeOut |
		// Just set the synthTree's template.
		synthTree = synthTree.asSynthTree;
		^synthTree.setTemplate(this);
	}

	=@> { | synthTree, param = \amp |
		// map a source to a parameter of the synthtree
		^this doIfSynthTree: { | st | st.map(param, this) }
	}
	
	doIfSynthTree { | action |
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? {
			action.(synthTree, this);
		};
	}

	hasInputs { ^false }

	push {}
}

+ Nil {
	receiveChuck { | chucker, fadeAction |
		("Ooops!  Someone tried to chuck to nil.  Probable causes:").postln;
		("1. No SynthTree has yet been created by chucking.").postln;
		("2. Trying to chuck into a SynthTree parameter that does not exist.").postln;
		"Current parameters are:".scatList(currentEnvironment.keys.asArray.sort).postln;
		^chucker;
	}
    asSynth { /* ^nil */ }
	inputBusIndex { /* ^nil */ }
	onEnd { /* ^nil */ }
}

+ Symbol {
	receiveChuck { | chucker, replaceAction |
		//		^synthTree.asSynthTree.chuck(this, replaceAction);
		^this.asSynthTree.chuck(chucker, replaceAction)
	}
    asSynth { | synthTree |
		^[this].asSynth(synthTree);
    }
	fadeOut { | fadeTime |
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.fadeOut(fadeTime) };
	}

	fadeTime_ { | fadeTime = 0.2 |
		^this.asSynthTree.fadeTime = fadeTime;
	}
	set { | ... args |
		/* Note: TODO: SynthTree stores args and use them when later 
			starting the synth, in which case it makes
			sense to send set to newly created SynthTree instances
		*/
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.set(*args) }
	}

	asSynthTree { | createIfMissing = true |
		^SynthTree.at(this, createIfMissing);
	}

	toggle { | fadeTime |
		^this doIfSynthTree: { | st | st.toggle(fadeTime) };
	}

	start {
		/*
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.start };		
		*/
		^this doIfSynthTree: { | st | st.start };
	}
	stop {
		/*
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.stop };		
		*/
		^this doIfSynthTree: { | st | st.stop };
	}
	free {
		^this doIfSynthTree: { | st | st.free };
	}

	freeBuffer {
		this.free;
		BufferList.free(this);
	}

	synth {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.synth };		
	}
	isPlaying {
		var synthTree;
		synthTree = this.asSynthTree(false);
		^if (synthTree.isNil) { false } { synthTree.synth.isPlaying };
	}
	inputs {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.inputs };
	}
	output {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.output };
	}
	args {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.args };
	}

	template {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.template };		
	}

	template_ { | template |
		var synthTree;
		synthTree = this.asSynthTree(false);
		if (synthTree.notNil) { synthTree.template = template };
		^synthTree;
	}
	
	view { | param, viewName, view, func, onClose, enabled = true |
		^this.asSynthTree.view(param, viewName, view, func, onClose, enabled);
		/*		var synthTree;
		synthTree = this.asSynthTree(false);
		if (synthTree.notNil) {
			synthTree.view(param, viewName, view, func, onClose, enabled) };
		^synthTree;
		*/
	}

	knobs {
		^this doIfSynthTree: { | st | st.knobs };		
	}

	buf { | bufName, paramName = \buf |
		bufName = bufName ? this;
		^this.asSynthTree.buf(bufName, paramName);
	}

	fader {
		var synthTree;
		synthTree = this.asSynthTree;
		SynthTree.faders(synthTree.server);
		^synthTree;
	}
}

+ Function {
	
	asSynthTemplate { | name | 
		^FunctionSynthTemplate(this, name);
	}

    asSynth { | synthTree, fadeTime |
		var outputBus;
		outputBus = synthTree.getOutputBusIndex;
        ^this.xplay(
            synthTree.group,
            outbus: outputBus, 
            fadeTime: synthTree.getFadeTime(fadeTime),
            addAction: \addToHead,
            args: synthTree.synthArgs
        );
    }
	/*
	chuck { | symbol |
		^symbol.asSynthTree...
	}
	*/
	xplay { | target, outbus = 0, fadeTime = 0.02, addAction = 'addToHead', args |
		^{ this.value.adsrOut(attackTime: fadeTime) }
		.play(target, outbus, fadeTime, addAction, args);
	}

	templateArgs {
		^{ this.value.adsrOut }.asSynthDef.allControlNames;
	}
	
}

+ SynthDef {

	asSynthTemplate { ^this }

	asSynth { | synthTree |
        ^Synth.new(this.name, synthTree.synthArgs, synthTree.group, \addToHead);
    }

	templateArgs {
		^this.allControlNames;
	}

	inputSpecs {
		^this.allControlNames
		.select({ | cn | cn.name.asString[..1] == "in" })
		.collect({ | cn | [cn.name, 1] })
		.flat;
	}
}

+ String {
    asSynth { | synthTree |
		^[this].asSynth(synthTree);		
    }
}

+ Array {
    asSynth { | synthTree |
		var args;
		args = this[1..] ++ synthTree.synthArgs;
		// Synth.new(defName, args, target, addAction: 'addToHead');
        ^Synth.new(this[0], args, synthTree.group, \addToHead);
    }
}

+ SimpleNumber {
	=> { | param | param.set(this); }
}
