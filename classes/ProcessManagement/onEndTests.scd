/* 
Using Notification to notify when a process ends

See ProcessRegistry, Notification

IZ Thu, 27 Feb 2014 16:17:48

*/

//:
a = {  WhiteNoise.ar(0.01) }.play;
NodeWatcher.register(a);
\test.addNotifier(a, \n_end, { | ... args | args.postln; "ended".postln; } );

//:

a.free;
