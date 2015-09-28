
+ Object {
	asSynthPlayerSource { | synthPlayer | ^SynthPlayerSource(this, synthPlayer) }
}

+ Function {
	asSynthPlayerSource { | synthPlayer | ^SynthPlayerFuncSynthSource(this, synthPlayer) }
}

+ String {
	asSynthPlayerSource { | synthPlayer |  ^SynthPlayerSynthSource(this, synthPlayer) }
	play { | target, outbus, fadeTime, addAction, args |
		^Synth (this, args ++ [out: outbus, fadeTime: fadeTime], target, addAction)
	}
}

+ Event {
	asSynthPlayerSource { | synthPlayer | ^SynthPlayerPatternSource(this, synthPlayer) }
}