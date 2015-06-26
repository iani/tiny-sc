/*
EventPattern can be embedded in another Pattern, 
in similar way as Pdef can be embedded in a Pseq (see Pdef help entry).

IZ Mon, Apr 21 2014, 09:58 EEST

EventPattern((dur: 0.1, degree: [-5, 12, 1].pbrown + [0, [-2, 3]].prand)).play;

shortcut: 

(dur: 0.1, degree: [-5, 12, 2].pbrown + [0, [-2, 3]].prand).ep;

*/

EventPattern : Pattern {
	var <>event; // contains patterns

	*new { | event | ^this.newCopyArgs(event ?? { () }) }

	asStream { ^EvtStream(event) }

	pattern { ^this }
}

EvtStream : Stream {
	var <>event; // contains streams
	*new { | event |
		^super.new.initEvtStream(event);
	}

	initEvtStream { | inEvent |
		event = ();
		inEvent keysValuesDo: { | key, value | event[key] = value.asStream(this); };
	}

	next {
		var outEvent, outValue;
		outEvent = ();
		event keysValuesDo: { | key, value |
			outValue = value.next(this);
			if (outValue.isNil) { ^nil };
			outEvent[key] = outValue;
		};
		^outEvent;
	}

	/* Needed to embed an EventPattern in a Stream as in: 
		Pseq([EventPattern((degree: (1..8).pseq(2)))]).play;
	*/
	embedInStream { arg inval;
		var outval;
		// this.changed(\started); // Put this in when the need arises.
		while {
			outval = this.next;
			outval.notNil;
		}{
			outval.yield;
		};
		// this.changed(\stopped); // Put this in when the need arises.
		nil;
	}	
}


// !!!!!!!!!!!!!!!! NOT DONE !!!!!!!!!!!!!!!!

EventPatternSynth : Synth {
	classvar <defName = "*arlink1*";
	var <pattern; // EventPattern: Its target is set to my group after that is started
	var <player, <bus;

		*initClass {
		StartUp add: {
			this.makeSynthDefs;
		}
	}

	*makeSynthDefs {
		SynthDef(defName, { | fadeIn = 0 out = 0 fadeTime = 0.02 |
			Out.ar(out, In.ar(fadeIn) * // from GraphBuilder:makeFadeEnv:
				EnvGen.kr(Env.new([fadeTime <= 0, 1, 0], #[1, 1], \lin, 1),
					\gate.kr(1.0), 1.0, 0.0, fadeTime, 2));	
		}).add;
	}
	
	*new { | source, args |
		/*  create own group
			allocate bus
			set source's eventpattern's fadeBus and target
			then send self
			start playing eventpattern
			doneAction is free self, group, and synths contained in group
			onEnd: free bus and stop EventStreamPlayer
			set source's chuck's output to newly created instance of self.
		*/

		var synth, target, group;
		/*
			var target;
			target = args[target]
		synth = this.basicNew(defName, args

		Synth
		*/
	}

	init {
		group = GroupLink.inside(pattern.event[\target]).permanent;
		bus = Bus.audio(Server.default, 1);
		pattern.event[\out] = bus.index;
	}

	play { | target, outBus, fadeTime, addAction, args |
		// Called by ChuckPatternSource:makeSynth
		//	player = pattern.
		thisMethod.notImplemented;
		/*
		^FadeSynth(FadeSynth.defName,
			args ++ [fadeIn: bus.index, fadeTime: fadeTime],
			target, \addToTail
		)
		.onStart(this, {
			player = pattern.play; // ?
		})
		.onEnd(this, {
			player.stop;
		})
		*/
	}	


	moveToTail { | newTarget |
		group.moveToTail(newTarget);
	}
	
}