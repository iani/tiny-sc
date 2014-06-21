/*

Tries for methods that will: 

1. Update the values stored in an environment (for playing with Task/Routine).

or: 

2. Send the values to a pattern played by a SynthTree with (key: val) +> \synthtree;

Sat, Jun 21 2014, 18:14 EEST
*/

m = (a: 1, b: 2);
c = EventModel();
m keysValuesDo: { | k v | c.addKey(k, nil, v) };
c.keys do: { | key | c.slider(key) };
//:

c addDependant: { | ... args | args.postln; };

//:
c[\a].inspect;

//:
c.keys do: { | key |
	c.slider(key)
	.map(c, key)
};

//:
\test.st.addNotifier(c, \a, { | adapter |
	(dur: adapter.mappedValue + 0.1) +> \test;
});

//:
(degree: (0..7).prand) => \test;

//:
r = () make: {
	{loop { ~a.postln; (~a ? 0.1 + 0.1).wait }}.fork;
};
//:
r.addNotifier(c, \a, { | adapter |
	r[\a] = adapter.mappedValue;
});

//: Towards a setup for a routine/task model: 
[ // use array to preserve order of arguments
a: [0, 1, \lin, 0, 0.1], // use specs instead of values, to initialize
b: 2,  // specs may also be inferred?
freq: 200, // spec inferred from name?
instrument: [\x, \y], // here we need special treatment!: menu spec

] // some messages here like: 
.taskPlayer(taskfunc).gui;

// Alternative way to code this might be: 
{ a function }.taskPlayer(
	[a: ..., b: ...] // specs
).gui;

// I think I prefer the first formulation, which starts with the specs. 
// because it is similar to the practice of declaring variables in the beginning.
// But it would be no trouble to implement both coding versions.

/*
From the above, do the following: 
- Create an EventModel with all keys and appropriate adapters.
- Create a window that allows you to edit/set interactively the value of each key.
- To this EventModel, which always sends this.changed(\key, adapter) when a value changes, 
  may be attached different types of auditory or action "display" objects.
  These objects receive the changed messages and react to them in order to change their 
  state, which results in different audible/visible actions.  They are therefore not 
  models, they are displays.  The model is the EventModel because it contains the data
  (a simplistic view, but good for the present purposes). 

2 types of "action display" objects are foreseen at the moment: 

1. An Event (Environment) in which is running a Routine or Task that uses the environments values. 

2. An event run by EventPattern, running in an Idef in a SynthTree, modified each time with: 
   (key: value) +> synthTree;

A third application is thinkable - setting parameters of a synth running in a SynthTree.  Possible alternative to the early "knobs" interface of SynthTree.

*/