
+ Ref {
	=> { | st |
		^st.asSynthTree chuck: value.asSynthTemplate
	}

	+> { | st, paramName = \buf |
		//		BufferList.getBuffer(value).play;
		var buf;
		buf = BufferList.getBuffer(value);
		postf("% now plays % at %\n", st, value, paramName);
		st = st.asSynthTree;
		if (st hasParam: paramName) {
			st.set(paramName, buf.bufnum)
		}{
			{	
				PlayBuf.ar(
					buf.numChannels, \buf.kr(buf.bufnum), 
					\rate.kr(1) * BufRateScale.kr(buf.bufnum),
					\trigger.kr(0),
					\startPos.kr(0),
					\loop.kr(1),
					2
				)
			} => st;
		}
	}
}

+ Symbol {
	asSynthTemplate {
		^Library.at(SynthTemplate, '---ALL---', this);
	}
}
