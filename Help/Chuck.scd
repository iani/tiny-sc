
{ WhiteNoise.ar(0.1) } => \test;
{ BrownNoise.ar(0.1) } => \test;
3 =>.fadeTime \test;
{ SinOsc.ar(400, 0, 0.1) } => \test;
{ SinOsc.ar(\freq.kr (800), 0, 0.1) } => \test;
600 *>.freq \test;
(400..600).prand *>.freq \test;

600.asStream;
