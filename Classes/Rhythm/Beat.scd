
BeatPlayer(\beat1); // .pattern = 1;

Beat.add(\beat1, \test, { "testing".postln });
BeatPlayer(\beat1).start;

BeatPlayer(\beat1).stop;

//: ================

{ SinOsc.ar(Rand(300, 400), 0, 0.1) } %% \c1 %%  \b1;

[0.1, 0.2].pseq %% \b1;

\b1.beat.restart;

\b1.beat;

Beat addDependant: { | ... args | args.postln; };
//:

[Pn(10, 0.05), 1].pseq %% \b2 %% \c1;

\b2.play;


[Pn(10, 0.05), 1].pseq %% \b2;

Beat(\b2).start;

Beat(\b2).addDependant({ | ... args | args.postln; });


{ WhiteNoise.ar(0.1) } %% \c1;

////////////////////////////////////////////////////////////////
[Pn(10, 0.05), 1].pseq %% \b2;

\b2.beat;

\b2.beat.inspect;
////////////////////////////////////////////////////////////////

[Pn(10, 0.05), 1].pseq