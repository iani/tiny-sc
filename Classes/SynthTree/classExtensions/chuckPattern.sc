
+ Function {
	asPatternSynth { | target, outbus = 0, fadeTime = 0.02, addAction=\addToHead, args |
		var def, synth, server, bytes, synthMsg;
		target = target.asTarget;
		server = target.server;
		if(server.serverRunning.not) {
			("server '" ++ server.name ++ "' not running.").warn; ^nil
		};
		def = this.asSynthDef(
			fadeTime:fadeTime,
			name: SystemSynthDefs.generateTempName
		);
		synth = PatternSynth.basicNew(def.name, server);
		// if notifications are enabled on the server,
		// use the n_end signal to remove the temp synthdef
		if(server.notified) {
			OSCpathResponder(server.addr, ['/n_end', synth.nodeID], { |time, resp, msg|
				server.sendMsg(\d_free, def.name);
				resp.remove;
			}).add;
		};
		synthMsg = synth.newMsg(target, [\i_out, outbus, \out, outbus] ++ args, addAction);
		def.doSend(server, synthMsg);
		^synth
	}
}

+ Ref {
	receivePatternInstrument { | patternInstrument, numChannels |
		^patternInstrument.instrument = this.value;
	}

	patternParams { | pattern |
		^PatternInstrument(PatternEventPlayer(pattern), this.value);
	}
	
	playPattern { | pattern |
		^PatternInstrument(pattern, this.value);
	}

	asSynthTemplate {
		^Library.at(SynthTemplate.key, '---ALL---', this.value).template;
	}
	
}

+ Symbol {
	receivePatternInstrument { | patternInstrument, numChannels |
		^this.asSynthTree.receivePatternInstrument(patternInstrument, numChannels);
	}
}

+ SynthTree {
	receivePatternInstrument { | patternInstrument, numChannels |
		// TODO:
		/*
			if (template isKindOf: PatternInstrument) {
				patternInstrument = template mergePatternInstrument: patternInstrument;
			};
		*/

		^this.chuck(patternInstrument, numChannels);
		// [this.Method.name, "not implemented"].postln;
		//	this.value.asSynthTree receivePatternInstrument: pi;
	}
}

