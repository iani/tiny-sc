/* 
A private bus that provides level, crossfade, and fx-plugin facilities. 
Uses currentEnvironment.  Works together with Syn class. 

IZ Mon, 03 Mar 2014 00:15:12
*/

MixBus {
	classvar all;

	var <name;
    var <rate;
    var <numChannels;
	var <outSynthFunc;
    var <target;
    var <addAction;
    var <fadeTime;
    var <bus;
    var <index;
  	var <group;
	var <outSynth;

	*all {
		^all ?? { all = IdentityDictionary() };
	}
    
	*new { | name = \default, rate = \audio, numChannels = 1,
		outSynthFunc, target, addAction = \addToTail, fadeTime = 0.02 |
		var instance;
		instance = this.all[name];
		if (instance.isNil) {
			instance = this.newCopyArgs(name, rate, numChannels,
                outSynthFunc, target, addAction, fadeTime).init;
			all[name] = instance;
		};
		^instance;
	}
	
	init {
        target = target.asTarget;
        bus = Bus.alloc(rate, target.server, numChannels);
        index = bus.index;
		group = Group(target, addAction);
		outSynthFunc = outSynthFunc ?? {{
			{ 
                (In.perform(rate, \in.kr(index)) * 
                    Adsr(fadeTime, 0.1, 1, fadeTime, ampName: \level, ampValue: 1)
                ).out(\out, 0)
            }.play(group, 0, fadeTime, \addToTail);
		}};
   		outSynth = outSynthFunc.(this);
	}

    level_ { | argLevel = 1 |
        outSynth.set(\level, argLevel);
    }

	push {
		~mixerIn = index;
		~target = group;
		~mixBus = this;
	}

	fadeOut { | fadeTime |
        // release synth, but do not free self
        if (fadeTime.notNil) { outSynth release: fadeTime } { outSynth.release };
        // TODO: set to nil after receiving notification from freed synth
        // also notify. 
        outSynth = nil;
	}

	release { | fadeTime |
        // fadeOut and free self when fadeSynth is done.
        
	}

	free {
		outSynth.free;
		group.free;
        this.changed(\free);
        super.free;
        this.objectClosed;
        all[name] = nil;
	}

	fadeIn { | fadeTime, synthFunc |
		// (re)start synth, fading in.
        // Use new synthFunc if provided.
        // release old synth if running.
        // This creates crossfade.  
        // Should we call it crossfade? restart ??? what?????

	}

    asString { ^format("MixBus(%, %, %, %)", name, rate, index, numChannels) }
}

+ In {
	*audio { | in = 0 | ^this.ar(in) }
	*control { | in = 0 | ^this.kr(in) }
}

+ Out {
	*audio { | out = 0 | ^this.ar(out) }
	*control { | out = 0 | ^this.kr(out) }
}

+ Symbol {
	push { | rate = \audio, numChannels, 
		outSynthFunc, target, addAction = \addToHead, fadeTime = 0.2, envir |
		^this.mixBus(rate, numChannels, outSynthFunc, 
            target, addAction, fadeTime).push(envir);
	}
	mixBus { | rate = \audio, numChannels = 1, outSynthFunc, 
        target, addAction = \addToHead, fadeTime = 0.2 |
		^MixBus( this, rate, numChannels, outSynthFunc,
			target, addAction, fadeTime
		);
	}
}

+ Synth {
    => { | mixer = \default |
        /* TODO: per default init (fadeIn?) the synth of the mixer
            if that synth is not already running.
            Add a adverb "s" to override this default: 
            i.e. not to re-boot. 
            see for example
            SecuenceableCollection
          + { arg aNumber, adverb; ^this.performBinaryOp('+', aNumber, adverb) }
            finally resolved in method: performBinaryOpOnSeqColl
        */
        if (mixer.isKindOf(MixBus).not) {
            mixer = mixer.mixBus;
        };
        this.moveToHead(mixer.group);
        this.set(\out, mixer.index);
    }
}