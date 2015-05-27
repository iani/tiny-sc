
ChuckProcess {
	classvar >parentParams; // Parent event holding default parameters for params
	var <chuck, <template, <params;

	*new { | chuck, template, params |
		^this.newCopyArgs (
			chuck,
			template,
			params ?? { ().parent_ (this.parentParams) }
		)
	}

	*parentParams {
		parentParams ?? {
			parentParams = (
				outbus: 0,
				fadeTime: 0.02,
				addAction: \addToHead,
				args: ()
			)
		};
		^parentParams;
	}

	setArgs { | args |
		var theArgs, keysValues;
		theArgs = params [\args];
		args keysValuesDo: { | key, value |
			value = value.asStream;
			keysValues = keysValues add: key;
			keysValues = keysValues add: value;
			theArgs [key] = value;
		};
		^keysValues;
	}
	
	setProcessParameter { | parameter, value |
		params [parameter] = value;
	}
}

Cnil : ChuckProcess {
	// Just a consistent naming for the empty ChuckProcess
}

Cfunc : ChuckProcess {
	var <synth;

	play {
		synth = template.play(
			params [\target].next.asTarget,
			params [\outbus].next,
			params [\fadeTime].next,
			params [\addAction].next,
			params [\args].getPairs
		);
	}

	stop {
		synth !? { synth.release }
	}

	setArgs { | args |
		var nextArgs;
		synth.set (*super.setArgs (args));
	}
	
	setProcessParameter { | parameter, value |
		super.setProcessParameter (parameter, value);
		this.perform ((parameter ++ "_").asSymbol, value);
	}

	target_ { | atarget |
		
	}

	outbus_ { | aoutbus |
		synth.set (\outbus, aoutbus);
	}

	fadeTime_ { | afadeTime |
		synth.set (\fadeTime, afadeTime);
	}

	addAction_ { | aaddAction |
		
	}

	args_ { | aargs |
		// TODO: Review this!
		synth.set (*aargs)
	}
}