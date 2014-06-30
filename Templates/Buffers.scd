
SynthTemplates([
buf1: SynthDef(\buf1, { 
	\buf.playBuf.ladsrOut;
}),
buf1s: SynthDef(\buf1s, { 
	Pan2.ar(\buf.playBuf, \pos.kr(0)).ladsrOut;
}),
bufmagabove: SynthDef(\bufmagabove, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagAbove(chain, \magabove.kr(0.000001));
	IFFT(chain).ladsrOut;
}),
bufmagbelow: SynthDef(\bufmagbelow, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagBelow(chain, \magbelow.kr(10));
	IFFT(chain).ladsrOut;
}),
bufmagaboves: SynthDef(\bufmagaboves, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagAbove(chain, \magabove.kr(0.000001));
	Pan2.ar(IFFT(chain), \pos.kr(0)).ladsrOut;
}),
bufmagbelows: SynthDef(\bufmagbelows, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagBelow(chain, \magbelow.kr(10));
	Pan2.ar(IFFT(chain), \pos.kr(0)).ladsrOut;
}),
	// numChans, in, pos = 0.0, level = 1.0, width = 2.0, orientation = 0.5
bufmagaboveq: SynthDef(\bufmagaboveq, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagAbove(chain, \magabove.kr(0.000001));
	PanAz.ar(4, IFFT(chain), \pos.kr(0), 1, \width.kr(2), \orientation.kr(0.5)).ladsrOut;
}),
bufmagbelowq: SynthDef(\bufmagbelowq, {
	var source, chain, output;
	source = \buf.playBuf;
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagBelow(chain, \magbelow.kr(10));
	PanAz.ar(4, IFFT(chain), \pos.kr(0), 1, \width.kr(2), \orientation.kr(0.5)).ladsrOut;
})
], 'buffer')
