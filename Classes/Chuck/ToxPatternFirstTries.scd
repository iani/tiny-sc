[0.05, 0.3, 0.1].prand *> \t;

// Tox(\tox).addToTask(TaskPlayer(\t)).pattern = "xoox___oooxoxoxx_";

Tox(\tox).addToTask(TaskPlayer(\t)).pattern = Pstutter(4, "xoox___oooxoxoxx_".prand);

{ SinOsc.arps(Rand(400, 1500)) } => \test;

\test.chuck.addToTask(Tox(\tox));

//:
{ Mix(Blip.arp(Rand(400, 1500) * [0.5, 1, 1.2, 8/4, 3]/3, 5)).dup / 3 } => \test;

//:

0.1 *> \t;
{ SinOsc.arps(Rand(400, 1500)) } => \test;
\test *>.xox_ \t;

//:
Library.at(TaskPlayer);

//:
TaskPlayer(\t).addDependant({ |... args | args.postln});
Tox(\_t).addDependant({ |... args | args.postln});

Tox(\_t).parent;

Tox(\_t).inspect;

Tox(\_t).addToTask(TaskPlayer(\t));

