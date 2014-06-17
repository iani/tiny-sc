/*
Playing around with buffers.

Mon, Jun 16 2014, 22:11 EEST

*/

Server.default.quit;

BufferList.loadFolder;

Server.default.boot;

BufferList.autoload = true;

Server.default.reboot;

BufferList.getBuffer(\bell);

\bell.b;

\crash.b.play;

//:
a = {
	\buf1.playBuf(\boat)
	+ 
	\buf2.playBuf(\bell)
	
}.play(args: [rate: 2]);

//:

a.set(\buf2, \conga.bufnum);
a.set(\buf1, \crash.bufnum);
a.set(\rate, 10.33);

//:
\boat.bufnum;

//:
a = {
	Mix([\boat, \bell, \conga, \tamb, \sd, \ride].collect({ | n, i |
		n.playBuf(n)
	}))
}.play;

//: Now with SynthTree

`\boat +> \bufplayer1;

3 +>.rate \bufplayer1;

-1 +>.rate \bufplayer1;

{ RLPF.ar(\buf.playBuf(\boat), \freq.kr(400)) } => \bufplayer1;

2 +>.rate \bufplayer1;
2000 +>.freq \bufplayer1;

`\conga +> \bufplayer1;

`\bell +> \bufplayer1;

//: FFT fun
// See Examples from PV_Magabove
{
	var source, chain;
	source = \buf.playBuf(\boat);
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagAbove(chain, \thresh.kr(0));
	IFFT(chain)
} => \fftfun;

//:
5 +>.thresh \fftfun;

//:
10 +>.amp \fftfun;

//:

1.5 +>.thresh \fftfun;

0.5 +>.thresh \fftfun;

0 +>.thresh \fftfun;

//: MagBelow
{
	var source, chain;
	source = \buf.playBuf(\boat);
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagBelow(chain, \thresh.kr(0));
	IFFT(chain)
} => \fftfun;

//:

0.1 +>.thresh \fftfun

//: MagFreeze

{
	var source, chain;
	source = \buf.playBuf(\boat);
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagFreeze(chain, SinOsc.kr(\freezerate.kr(10)));
	IFFT(chain)
} => \fftfun;

//: 
1 +>.freezerate \fftfun;
//:
4 +>.freezerate \fftfun;
//:
15 +>.freezerate \fftfun;
//:
25 +>.freezerate \fftfun;
//:
0.15 +>.freezerate \fftfun;

//:
({ myfilter.kr ..... } ==> \magabove) 
=< 
(	\magbelow) =< \magfreeze =< \bufplayer1;


//: 
5 +>.rate  \fftfun;
//: Dust ...
{
	var source, chain;
	source = \buf.playBuf(\boat);
	chain = FFT(LocalBuf(2048), source);
	chain = PV_MagFreeze(chain, Dust.kr(\freezerate.kr(10)));
	IFFT(chain)
} => \fftfun;

//:
0.5 +>.rate  \fftfun;
//: Dust ...
{
	var source, chain;
	source = \buf.playBuf(\bell);
	chain = FFT(LocalBuf(2048), source);
	chain = PV_PhaseShift(chain, LFNoise0.kr(10, 2, 2), 1);
	IFFT(chain)
} => \fftfun;
//:
`\bell +> \fftfun;
//:

1 +>.rate \fftfun;
//:
2 +>.shift \fftfun;


