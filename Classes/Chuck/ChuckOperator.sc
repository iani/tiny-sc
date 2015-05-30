+ Object {

	=> { | symbol, adverb |
		^Registry.doIfFound (Chuck, symbol, { | chuck |
			chuck.setProcessParameter (adverb, this)
		})
	}

	*> { | symbol, adverb |
		^Registry.doIfFound (Chuck, symbol, { | chuck |
			chuck.setArgs (adverb, this)
		})
	}
}

+ Function {
	=> { | symbol |
		^Chuck (symbol).play (this);
	}

	&> { | symbol |
		^Chuck (symbol).eval (this);
	}
}

