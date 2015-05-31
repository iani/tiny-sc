ChuckProcess {
	classvar >parentParams; // Parent event holding default parameters for params
	var <chuck, <template, <params;
	var <paramsTemplate; // TODO: store patterns of the param streams, for cloning

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
				addAction: \addToHead
			)
		};
		^parentParams;
	}

	setArgs { | args |
		var theArgs, keysValues;
		theArgs = params [\args];
		theArgs ?? { params [\args] = theArgs = ().parent_ (params) };
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
	// Just a consistent name for the empty ChuckProcess
}

Csynth : ChuckProcess {
	var <synth;

	play {
		synth = template.play(
			params [\target].next.asTarget,
			params [\outbus].next,
			params [\fadeTime].next,
			params [\addAction].next,
			(params [\args] ?? { () }).getPairs
		);
	}

	stop { synth !? { synth.release } }
	release { | dur |
		synth !? {
			synth.release(dur ?? { params[\fadeTime].next })
		}
	}
	free { synth !? { synth.free } }

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


Cfunc : Csynth {
	play {
		var result;
		result = params [\args] use: template.func;
		if (result isKindOf: Node) {
			synth = result;
		}
	}
}

CfuncTemplate {
	var <func;
	*new { | func | ^this.newCopyArgs(func) }

	chuckProcessClass { ^Cfunc }
}