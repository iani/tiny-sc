/*
Shortcuts for PlayBuf and BufRd

IZ Sun, Mar 16 2014, 16:06 EET

PlayBuf	*ar { arg numChannels, bufnum=0, rate=1.0, trigger=1.0, startPos=0.0, loop = 0.0, doneAction=0;
		^this.multiNew('audio', numChannels, bufnum, rate, trigger, startPos, loop, doneAction)
	}

BufRd  *ar { arg numChannels, bufnum=0, phase=0.0, loop=1.0, interpolation=2;
		^this.multiNew('audio', numChannels, bufnum, phase, loop, interpolation)
	}
*/

+ Symbol {
	playBuf {
		| bufnum = 0, numChannels = 1, rate = 1.0, trigger = 1.0, 
		startPos = 0.0, loop = 1, doneAction = 2,
		rateName = \rate, triggerName = \trigger, startPosName = \startPos,
		loopName = \loop |

		var buf;
		if (bufnum.isKindOf(Integer).not) {
			bufnum = bufnum.bufnum;
		};
		buf = this.kr(bufnum);
		^PlayBuf.ar(numChannels, buf, rateName.kr(rate) * BufRateScale.kr(bufnum),
			triggerName.kr(trigger), 
			//			startPosName.kr(startPos) * BufFrames.kr(bufnum),
			startPosName.kr(startPos) * SampleRate.ir * BufRateScale.kr(bufnum),
			loopName.kr(loop),
			doneAction
		)
	}

	bufnum {
		^BufferList.getBuffer(this).bufnum;
	}

	/// TODO: Complete the following ////////////
	playBufKr { 
		| numChannels = 1, bufnum=0, rate=1.0, trigger=1.0,
		startPos=0.0, loop = 0.0, doneAction=0,
		rateName = \rate, triggerName = \trigger, startPosName = \startPos |
	}

	bufRd { 
		| numChannels = 1, bufnum=0, rate=1.0, trigger=1.0, 
		startPos=0.0, loop = 0.0, doneAction=0,
		rateName = \rate, triggerName = \trigger, startPosName = \startPos |
	}

	bufRdKr { 
		| numChannels = 1, bufnum=0, rate=1.0, trigger=1.0, 
		startPos=0.0, loop = 0.0, doneAction=0,
		rateName = \rate, triggerName = \trigger, startPosName = \startPos |
	}	
}

