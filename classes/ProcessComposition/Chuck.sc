/* 
Chuck operator and variants.

IZ Sat, Mar  8 2014, 23:40 EET

SynthDef("test", { WhiteNoise.ar().adsrOut }).add;
"test" => \test;
\test.fadeOut(5);

\test.asSynthTree.start;

{ WhiteNoise.ar } => \test;

\test.asSynthTree.synth.set(\gate, 0);

\test.asSynthTree.synth.isPlaying;


\test.asSynthTree;

\asdf.asSynthTree;

SynthTree.nameSpaces;

a = Synth("test");
a = { SinOsc.ar(440, 0, Adsr()).out }.play; 

a = { WhiteNoise.ar(Adsr()).out }.play; 
a = { WhiteNoise.ar().adsrOut }.play;

a = { WhiteNoise.ar }.xplay;
a.fadeOut;
a.set(\out, 1);


a.release(30);

NodeWatcher.register(a);
a.isRunning;
a.isPlaying = true;
a.isPlaying;

a.free;

a = { WhiteNoise.ar }.xplay;
a.free(123);


nil !? { 1234 };

{ WhiteNoise.ar } =>.free \x;
{ SinOsc.ar(440) } => \x;
\x.fadeOut(5);
\x.start;
*/

+ Object {
	=> { | synthTree, replaceAction = \fadeOut |
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
        ^this.xplay(
            synthTree.group,
            outbus: outputBus, 
            fadeTime: synthTree.fadeTime,
            addAction: \addToHead,
            args: [out: outputBus]
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
		args = this[1..] ++ [out: synthTree.getOutputBusIndex];
		// Synth.new(defName, args, target, addAction: 'addToHead');
        ^Synth.new(this[0], args, synthTree.group, \addToHead);
    }
}
