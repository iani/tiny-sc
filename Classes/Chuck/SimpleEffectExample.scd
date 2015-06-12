
{ FreeVerb.ar(In.ar(\in.kr(0)), 0.9, 0.99, 0.9) } ++> \reverb;
//:
{ WhiteNoise.arp(Decay.kr(Impulse.kr(1, 0.5))) } ++> \source;
//:
\source +> \reverb;
//:
\source.toRoot;