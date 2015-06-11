//:
Server.default.queryAllNodes;
//: linking

\a => \b;
{ WhiteNoise.arp } ==> \a;
{ Resonz.arps(In.ar(\in.kr), LFNoise0.kr(30).range(500, 5000)) } ==> \b;
//:


//:
{ Resonz.arps(In.ar(\in.kr), LFNoise2.kr(9).range(150, 500), 5) * 2 } ==> \b;
//:
{ In.ar(\in.kr) * 5 * Decay.kr(Dust.kr(3), 2)} ==> \c;
//: TODO: Fix it so that the following sequence of switches works
\a => \c;
//:
\b => \c;
//:
\a => \b;
//:

\a.toRoot;

\b.toRoot;