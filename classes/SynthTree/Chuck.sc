/* 
Chuck operator and variants.

IZ Sat, Mar  8 2014, 23:40 EET
*/

+ Object {
	=> { | synthTree, replaceAction = \fadeOut |
		^synthTree.asSynthTree.chuck(this, replaceAction);
	}

	=<> { | synthTree, replaceAction = \fadeOut |
		^synthTree.asSynthTree.chuckMakingInput(this, replaceAction);
	}

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
	// test only: chuck control synth
	// TODO ...
	// May not be necessary: adsrOut / xplay recognizes ar/kr
	// but bus allocation?
	// =@> { | synthTree | synthTree.postln; }
}

+ Nil {
    asSynth { /* ^nil */ }
	inputBusIndex { /* ^nil */ }
	onEnd { /* ^nil */ }
}

+ Function {
    asSynth { | synthTree |
		var outputBus;
		outputBus = synthTree.getOutputBusIndex;
        ^this.xplay(
            synthTree.group,
            outbus: outputBus, 
            fadeTime: synthTree.fadeTime,
            addAction: \addToHead,
            args: synthTree.synthArgs
        );
		
    }
	xplay { | target, outbus = 0, fadeTime = 0.02, addAction = 'addToHead', args |
		^{ this.value.adsrOut }.play(target, outbus, fadeTime, addAction, args);
	}
}

+ Symbol {
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
	start {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.start };		
	}
	stop {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.stop };		
	}
	free {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.free };		
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
		var synthTree;
		synthTree = this.asSynthTree(false);
		if (synthTree.notNil) {
			synthTree.view(param, viewName, view, func, onClose, enabled) };
		^synthTree;
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
