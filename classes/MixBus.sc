/* 
A private bus that provides level, crossfade, and fx-plugin facilities. 
Uses currentEnvironment.  Works together with Sin class. 

IZ Mon, 03 Mar 2014 00:15:12
*/

MixBus : Bus {
	classvar all;

	var name;
	var <group;
	var <outSynth;
	var <outSynthFunc;

	*all {
		^all ?? { all = IdentityDictionary() };
	}

	*new { | name = \default, rate = \audio, index = 0, numChannels = 2, server, 
		outSynthFunc, target, addAction = \addToHead, fadeTime = 0.02 |
		var instance;
		instance = this.all(name);
		if (instance.isNil) {
			instance = super.new(rate, index, numChannels, server ? Server.default)
			.init(name, outSynthFunc, target, addAction, fadeTime);
			all[name] = instance;
		};
		^instance;
	}
	
	init { | argName, outSynthFunc, target, addAction = \addToHead, fadeTime = 0.02 |
		name = argName;
		group = Group(target.asTarget, addAction);
		outSynthFunc = outSynthFunc ?? {{
			{ In.perform(rate, \in.kr(index)).out(\out, 0) }
			.play(group, index, fadeTime, \addToTail);
		}};
		outSynth = outSynthFunc.(this);
	}

	push {
		~mixerIn = index;
		~target = group;
		~mixBus = this;
	}

	free {
		outSynth.free;
		group.free;
        this.changed(\free);
        super.free;
        this.objectClosed;
        all[name] = nil;
	}

	release {
        // fadeOut and free when fadeSynth is done.
        
	}

	fadeOut { | fadeTime |
        if (fadeTime.notNil) { outSynth release: fadeTime } { outSynth.release };
        outSynth = nil;
	}

	fadeIn {
		
	}
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
	mixBus { | name = \default, rate = \audio, index = 0, numChannels = 2, server, 
		outSynthFunc, addAction = \addToHead, fadeTime = 0.2 |
		^MixBus( name, rate, index, numChannels, server, 
			outSynthFunc, addAction, fadeTime
		);
	}
	push { | name = \default, rate = \audio, index = 0, numChannels = 2, server, 
		outSynthFunc, addAction = \addToHead, fadeTime = 0.2, envir |
		^this.mixBus.push(envir);
	}

}
