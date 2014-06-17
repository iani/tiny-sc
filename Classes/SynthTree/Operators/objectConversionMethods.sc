
+ Nil {
    asSynth { /* ^nil */ }
	inputBusIndex { /* ^nil */ }
	onEnd { /* ^nil */ }
	asSynthTree {
		^SynthTree.at(SynthTree.newName, true, \default);
	}
}

+ Symbol {

	asSynthTree { | createIfMissing = true, defaultChuck |
		^SynthTree.at(this, createIfMissing, defaultChuck);
	}

	asSynthTemplate { ^SynthDescLib.global[this].def }
	inputSpecs { ^[] }

    asSynth { | synthTree |
		//		^[this].asSynth(synthTree);
        ^Synth.new(this, synthTree.synthArgs, synthTree.group, \addToHead);
    }
	
	clearPatterns { ^this.asSynthTree.clearPatterns }

	resetParams { ^this.asSynthTree.resetParams }

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
	stopAll {
		/*
			Stop Edef's children, if it exists.
		*/
		^NameSpace.doIfFound(\Edef, this, _.stopAll);
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
		
		//		^this.asSynthTree.buf(bufName, paramName);
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

	xplay { | target, outbus = 0, fadeTime = 0.02, addAction = 'addToHead', args |
		^{ this.value.ladsrOut(fadeIn: fadeTime) }
		.play(target, outbus, fadeTime, addAction, args);
	}

	templateArgs {
		^{ this.value.ladsrOut }.asSynthDef.allControlNames;
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


/*
+ Array {
    asSynth { | synthTree |
		var args;
		args = this[1..] ++ synthTree.synthArgs;
		// Synth.new(defName, args, target, addAction: 'addToHead');
        ^Synth.new(this[0], args, synthTree.group, \addToHead);
    }
}
*/

