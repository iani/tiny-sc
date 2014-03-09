/* 
Chuck operator and variants.

IZ Sat, Mar  8 2014, 23:40 EET

SynthDef("test", { WhiteNoise.ar().adsrOut }).add;
a = Synth("test");
a.fadeOut(7);

"test" => \test;
\test.fadeOut(5);

\asdf.asSynthTree;


SynthTree.nameSpaces;

a = Synth("test");
a = { SinOsc.ar(440, 0, Adsr()).out }.play; 

a = { WhiteNoise.ar(Adsr()).out }.play; 
a = { WhiteNoise.ar().adsrOut }.play; 

a = { WhiteNoise.ar }.xplay;

a.set(\out, 1);


a.release(30);



NodeWatcher.register(a);
a.isRunning;
a.isPlaying = true;
a.isPlaying;

a.free;

nil !? { 1234 };

\tttt !? { 2345 };

*/

+ Object {
	=> { | synthTree, replaceAction |
		^synthTree.asSynthTree.chuck(this, replaceAction);
	}
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
        this.xplay(
            synthTree.group,
            outBus: outputBus, 
            fadeTime: synthTree.fadeTime,
            addAction: \addToHead,
            args: [out: outputBus]
        );
    }
}

+ Symbol {
    asSynth { | synthTree |
		^[this].asSynth(synthTree);
    }
	asSynthTree { | createIfMissing = true |
		^SynthTree.at(this, createIfMissing);
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
		args = this[1..] ++ [out: synthTree.getOutputBusIndex];
		// Synth.new(defName, args, target, addAction: 'addToHead');
        ^Synth.new(this[0], args, synthTree.group, \addToHead);
    }
}
