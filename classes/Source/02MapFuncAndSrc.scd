/* Debugging MapFunc, src method.
IZ Tue, Mar  4 2014, 22:25 EET.

*/

~source = Source();
//:
~source.source = (1..10);
~source.start;

//:
~synth = { SinOsc.ar(\freq.kr(400), 0, 0.1) }.play;
~synth.src(~source, \freq);
//:
~synth.src(~source, [400, 800].bimapSet(\freq, [1, 10]));
//:
\monitor.src(~source, \freq);
//:
\monitor.setSource(~source);
//:
~source.stop;
//:
~mapFunc = MapFunc(~source, \monitor, { \set });
//:
\monitor.setSource(~source, ~mapFunc);
//:
\monitor.setSource(~source, \freq); 

//////////////////////////////////////////////////////////
//:
~source.source = (1..10) / 10;
~source.start;
//:
\monitor.setSource(~source);
//:

