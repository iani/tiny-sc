
+ Object {
	asChuckSource { | chuck | ^ChuckSource(this, chuck) }
}

+ Function {
	asChuckSource { | chuck | ^ChuckFuncSynthSource(this, chuck) }
}

+ String {
	asChuckSource { | chuck |  ^ChuckSynthSource(this, chuck) }
	play { | target, outbus, fadeTime, addAction, args |
		^Synth (this, args ++ [out: outbus, fadeTime: fadeTime], target, addAction)
	}
}