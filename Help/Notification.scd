//:

w = Window().front;

w.view.onObjectClosed(\test, { | who | who.postln; });

//:

w = Window("blah").front;

w.onObjectClosed(\test, { | who | postf("Hello, % closed\n", who.notifier).postln;});

//:

