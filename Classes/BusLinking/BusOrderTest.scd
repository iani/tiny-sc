
//: Create 5 groups in order:
[\g5, \g4, \g3, \g2, \g1] do: { | g | currentEnvironment[g] = Group(); };
//: The order of the nodes is: g1, g2, g3, g4, g5
Server.default.queryAllNodes;
//:
~g1;
//:
~g5;
//: Now move ~g3 so that the order becomes g1, g4, g5, g2, g3
~g3.moveAfter(~g5);
~g2.moveAfter(~g5);

//:
Server.default.queryAllNodes;
//:
~g3;
// ////////////////////////////////////////////////////////////////
//: Now the same thing with a test tone:
[\g5, \g4, \g3, \g2, \g1] do: { | g | currentEnvironment[g].free };
//: Create 5 groups in order:
[\g5, \g4, \g3, \g2, \g1] do: { | g | currentEnvironment[g] = Group(); };
//:
{ WhiteNoise.ar(0.1) }.play(target: ~g2, outbus: 50);
{ | in | In.ar(in) }.play(target: ~g3, outbus: 0, args: [in: 50]);
//: try reverse order to see if there is a gap:
~g2.moveAfter(~g5);
//:
~g3.moveAfter(~g5);
