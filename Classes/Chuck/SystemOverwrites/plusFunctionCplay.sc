/* Tue, Jun  2 2015, 14:09 EEST
Making Function:play create a static output channel synth is both brain dead and brain removed.

Fixing that.

Tue, Jun  9 2015, 21:03 EEST:  This modification will be abandoned. 
Instad, a new method cplay will be added that will work with ChuckFuncSynthSource 
to create a synthdef and add it on the spot when creating a new instance of 
ChuckFuncSynthSource. 

*/

+ Function {

	cplay { arg target, outbus = 0, fadeTime = 0.02, addAction=\addToHead, args, name;
		/*  1. Do not remove synthdef when synth is done
			2. Use custom name - given by Chuck */
		var def, synth, server, bytes, synthMsg;
		target = target.asTarget;
		server = target.server;
		if(server.serverRunning.not) {
			("server '" ++ server.name ++ "' not running.").warn; ^nil
		};
		def = this.asSynthDef(
			fadeTime:fadeTime,
			name: name ?? { SystemSynthDefs.generateTempName }
		);
		synth = Synth.basicNew(def.name, server);
		synthMsg = synth.newMsg(target, [\out, outbus] ++ args, addAction);
		def.doSend(server, synthMsg);
		^synth
	}

	play { arg target, outbus = 0, fadeTime = 0.02, addAction=\addToHead, args;
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
		synth = Synth.basicNew(def.name, server);
			// if notifications are enabled on the server,
			// use the n_end signal to remove the temp synthdef
		if(server.notified) {
			OSCpathResponder(server.addr, ['/n_end', synth.nodeID], { |time, resp, msg|
				server.sendMsg(\d_free, def.name);
				resp.remove;
			}).add;
		};
		synthMsg = synth.newMsg(target, [\out, outbus] ++ args, addAction);
		def.doSend(server, synthMsg);
		^synth
	}
}

+ GraphBuilder {
	*wrapOut { arg name, func, rates, prependArgs, outClass=\Out, fadeTime;
		^SynthDef.new(name, { arg out=0;
			var result, rate, env;
			result = SynthDef.wrap(func, rates, prependArgs).asUGenInput;
			rate = result.rate;
			if(rate === \scalar,{
				// Out, SendTrig etc. probably a 0.0
				result
			},{
				if(fadeTime.notNil, {
						result = this.makeFadeEnv(fadeTime) * result;
				});
				outClass = outClass.asClass;
				outClass.replaceZeroesWithSilence(result.asArray);
				outClass.multiNewList([rate, out]++result)
			})
		})
	}
}

/*
+ SynthDef {
	cdoSend { |server, completionMsg|
		var bytes = this.asBytes;
		if (bytes.size < (65535 div: 4)) {
			server.sendMsg("/d_recv", bytes, completionMsg)
		} {
			if (server.isLocal) {
				"SynthDef % too big for sending. Retrying via synthdef file".format(name).warn;
				this.writeDefFile(synthDefDir);
				server.sendMsg("/d_load", synthDefDir ++ name ++ ".scsyndef", completionMsg)
			} {
				"SynthDef % too big for sending.".format(name).warn;
			}
		}
	}
}
*/

