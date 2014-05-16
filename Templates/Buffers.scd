
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
})
], 'buffer')
