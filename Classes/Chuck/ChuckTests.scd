{ WhiteNoise.arp } +> \test;

{ SinOsc.arp(\freq.kr(500)) } +> \test;

\test.ft = 0.5;
0.29 +>.amp \test;

\test.chuck.process.args;

6000 +>.freq \test;

Pwhite(500, 6000, inf) +>.freq \test;

Pn(Pseries(500, 30, 100), inf) +>.freq \test;

Pwhite(500, 600, inf) +>.freq \test;

\test.play(0.2);



\test.dur = 0.015;

//:

switch (\reader,

	\reader, { "reader".postln},
	\writer, { "writer".postln;}
)




//:

)




