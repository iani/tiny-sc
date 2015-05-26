
{ WhiteNoise.ar(0.1) } => \test;
{ BrownNoise.ar(0.1) } => \test;
3 =>.fadeTime \test;
{ SinOsc.ar(400, 0, 0.1) } => \test;

Library.at (Chuck, \test).process.synth.release;

() respondsTo: 'test_';

().test_ (1)

a = ().parent_ ((a: 1)) make: { ~b = 2 };

a.a;
parent

a = ChuckProcess ();
a.parentParams;
a = FunctionChuck ();
