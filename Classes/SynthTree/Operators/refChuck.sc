
+ Ref {
	=> { | st |
		^st.asSynthTree chuck: value.asSynthTemplate
	}

	++> { | st, paramName = \buf |		
		^this.perform('+>', st, paramName).set(\loop, 1);
	}

	+> { | st, paramName = \buf |
		//		BufferList.getBuffer(value).play;
		var buf;
		buf = BufferList.getBuffer(value);
		postf("% now plays % at %\n", st, value, paramName);
		st = st.asSynthTree;
		if (st hasParam: paramName) {
			st.set(paramName, buf.bufnum, \trigger, 10000000.0.rand);
		}{
			{	
				PlayBuf.ar(
					buf.numChannels, \buf.kr(buf.bufnum), 
					\rate.kr(1) * BufRateScale.kr(buf.bufnum),
					Changed.kr(\trigger.kr(0)),
					\startPos.kr(0),
					\loop.kr(0),
					0
				)
			} => st;
		};
		^st.set(\loop, 0);
	}
}

+ Symbol {
	asSynthTemplate {
		^Library.at(SynthTemplate, '---ALL---', this);
	}
}

