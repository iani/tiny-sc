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
		^synthTree.asSynthTree.setTemplate(this, replaceAction = \fadeOut);
	}

}

+ Nil {
    asSynth { /* ^nil */ }
	inputBusIndex { /* ^nil */ }
	onEnd { /* ^nil */ }
}

+ Function {
    asSynth { | synthTree |
		//		^{ Out.ar(0, LPF.ar(In.ar(16), 2000)) }.play;
		// ^{ LPF.ar(In.ar(16), 2000) }.xplay;
		/*	[this, thisMethod.postln, "stg", synthTree.group,
			"rg", RootNode()].postln;
		^{ LPF.ar(In.ar(16), 2000) }.play(
								synthTree.group

		);
		*/
		/*
{ LPF.ar(In.ar(\in.kr(0)), 2000) } =<> \comb;
\comb.asSynthTree.inputs;
b = { Out.ar(\out.kr(0), WhiteNoise.ar(0.1)) }.play;
b.set(\out, 16);
b.set(\out, 24);
a.set(\in, 24);

RootNode();
RootNode().asTarget;
Server.default.asTarget;

	\comb.asSynthTree.synth.inspect;
			default_group;
		*/
		
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
		/* Note: possibly store args and use them when later 
			starting the synth, in which case it makes
			sense to send set to newly created SynthTree instances
		*/
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { 
			synthTree.set(*args)
		}
	}
	asSynthTree { | createIfMissing = true |
		^SynthTree.at(this, createIfMissing);
	}
	start {
		var synthTree;
		^(synthTree = this.asSynthTree(false)) !? { synthTree.start };		
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
