//:
Server.default.queryAllNodes;
//: linking

\a => \b;
{ WhiteNoise.arp } => \a;
{ Resonz.arps(In.ar(\in.kr), LFNoise0.kr(30).range(500, 5000)) } => \b;
//:


//:
{ Resonz.arps(In.ar(\in.kr), LFNoise2.kr(9).range(150, 500), 5) * 2 } => \b;
//:
\b.chuck.play;
//:
\b.chuck.source.source.play(\b.chuck.args[\target].asTarget, args:[in: 16]);

\b.chuck.output.set(\in, 16);
//:
\b.chuck.args[\in] = 16;
//:
\b.chuck.play;
//:
`{{ Resonz.arps(In.ar(In.ar(\in.kr(16))), LFNoise0.kr(3).range(500, 5000)) }.play.register } => \b;
//:
\b.chuck.output.set(\in, 16);
//:
\b.chuck.source = { Resonz.arps(In.ar(16), LFNoise0.kr(30).range(500, 5000)) };
//:
\b.chuck.source = { Resonz.arps(In.ar(\in.kr(0)), LFNoise0.kr(30).range(500, 5000)) };
//:
\b.chuck.source = { Resonz.arps(In.ar(\in.kr(16)), LFNoise0.kr(30).range(500, 5000)) };
//:
\b.chuck.args[\in].bus;
//:
\b.chuck addAfter: \a.chuck;
//:
\b.chuck.output.set(\in, 16);
//:
\b.chuck.args[\in];
//:
\b.chuck.output moveToTail: \b.chuck.args[\target].group;
//:
\b.chuck.output moveAfter: \a.chuck.output;
//:
~rg = \a.chuck.args[\target].getReaderGroup;
//:
\b.chuck.output;
//:
\b.chuck.output.moveToTail(~rg.group);

//:
a = { WhiteNoise.arp }.play;
a.set(\out, \b.chuck.args[\in].bus.index);

b = { In.ar(16) }.play;
b.moveAfter(\a.chuck.output);

:
~bg = Group();

\b.chuck.setArgs(\target, ~bg);
~bg.moveAfter(\a.chuck.args[\target].group);

//
\a.chuck.output.moveBefore(\b.chuck.output);
//:
\a.chuck.args.inspect;
//:
\a.chuck.args[\target].group.moveAfter(\b.chuck.args[\target].group);
//:
\a.chuck.output;
//:
\a.chuck.args[\out].bus;

//:
\b.chuck.args[\in].bus;
//:
\b.chuck.args[\out];
//
0 =>.out \a;

//:
Server.default.scope;
//:

\b => \c;
\b => \a;
\c => \a;


\b.chuck.readers;
\a.chuck.readers;

//:
g = Group();
a = { WhiteNoise.arp }.play(target: g, args: [target: g]);



{ Out.ar(100, WhiteNoise.arp) }.play;
//:
{ | in | In.ar(in) }.play(args: [\in, 100], addAction: \addToTail);


