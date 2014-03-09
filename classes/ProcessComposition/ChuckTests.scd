
SynthDef("test", { WhiteNoise.ar().adsrOut }).add;
"test" => \x;
\x.fadeOut(5);

{ SinOsc.ar(440) } =>.5 \x;
{ WhiteNoise.ar } => \x;
{ PinkNoise.ar } =>.3 \x;
{ WhiteNoise.ar } =>.free \x;
{ LFSaw.ar(400) } =>.5 \x;

\x.fadeOut(5);
\x.start;


{ CombN.ar(In.ar(\in.kr(0))) } =<> \x;




\x.asSynthTree.inputs