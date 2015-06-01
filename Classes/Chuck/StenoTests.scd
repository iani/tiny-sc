ServerMeter(Server.default);
//:
t = Steno().push(true);
//:
--a
//:
t.quelle(\a, { WhiteNoise.ar(\volume.kr(0.1)) });

//:
t.quelle(\a, { WhiteNoise.ar(\volume.kr(0.2)) });

//:
t.quelle(\a, { WhiteNoise.ar(\volume.kr(0.3)) });

//:
t.quelle(\a, { WhiteNoise.ar(0.03) });
//:
t.quelle(\a, { WhiteNoise.ar(0.01) });
//:
t.quelle(\a, { BrownNoise.ar(0.01) });
//:
t.quelle(\a, { SinOsc.ar(500, 0, 0.01) });
//:
t.quelle(\a, { WhiteNoise.ar(0.53) });

//:
t.quelle(\a, { WhiteNoise.ar(1.53) });

//:
t.set(\a, \fadeTime, 3);

//:
t.set(\a, \volume, 0.01);

//: Problems with set?

t = Steno.new;
t.quelle(\a, { SinOscFB.ar(2 ** LFTri.kr(\modrate.kr(2)) * \freq.kr(300), \feedback.kr(0.5)) * 0.1 });
t.quelle(\b, { SinOscFB.ar(2 ** LFTri.kr(\modrate.kr(2)) * \freq.kr(300), \feedback.kr(0.5)) * 0.1 });
t.value("a");
t.set(\a, \modrate, 1, \freq, 100, \feedback, 2.1);
t.set(\a, \fadeTime, 3);
t.set(\a, \mix, 1);
//:
t.quelle(\a, { SinOsc.ar(400, 0, 0.1) });
//:
t.quelle(\a, { SinOsc.ar(500, 0, 0.5) });
//:
t.quelle(\a, { SinOsc.ar(600, 0, 0.05) });
//:
t.quelle(\a, { SinOsc.ar(600, 0, 0.95) });
//:

//:
Server.default.scope;
//: Compare levels:
{ SinOsc.ar(400, 0, 0.1) } => \a;

{ SinOsc.ar(500, 0, 0.5) } => \a;

{ SinOsc.ar(600, 0, 0.05) } => \a;

//: THERE IS DEFINITELY COMPRESSION GOING ON!

t.rebuild;