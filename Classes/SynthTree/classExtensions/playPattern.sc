+ Object {
	playPattern { | patternPlayer, action, startNow = true |
		var stream, pfunc;
		stream = this.asStream;
		action ?? { 
			var synth;
			action = { | value |
				synth = Synth (value.instrument, value.params);
				SystemClock.sched (value.dur, { synth.release });
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}

+ Symbol {
	playPattern { | pattern | ^this.asSynthTree.playPattern(pattern) }
}

+ Pattern {
	playPattern { | patternPlayer |
		^PatternInstrument(patternPlayer, this)
	}
}

+ Routine {
	playPattern { | patternPlayer, action, startNow = true |
		var pfunc;
		action ?? { 
			action = { | value |
				var next, synth;
				next = this.next (value);
				if (next.notNil) {
					synth = Synth (next, value.params);
					SystemClock.sched (value.dur, { synth.release })
				};
			};
		};
		pfunc = PatternFunc (patternPlayer, this, action);
		if (startNow and: { patternPlayer.isPlaying.not }) { patternPlayer.start };
		^pfunc;
	}
}
