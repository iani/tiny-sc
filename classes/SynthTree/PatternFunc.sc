/* 
Like MIDIFunc, OSCFunc, BufferFunc, ViewFunc.  Plays a pattern in a Task, and broadcasts the obtained values via changed(\value, value).   SynthTrees or other objects can listen through Notification.  

Notes from Roadmap:

PatternFunc notes

#+BEGIN_EXAMPLE
<pfunc template> %> 'pfunc_name' *>.param_name synthtree_name;

<pfunct template %> 'pfunc_name'; // creates PaternFunc and binds it to name

Alternative:

'pfunc_name'.patternFunc(<template>);
'pfunc_name'.pf(<template>); // shorter form

// also:

<pfunc template or name> *>.param_name syntree_name;
#+END_EXAMPLE

The operator *> could be a multi-purpose operator for binding any type of func (osc-, view-, midi-, pattern-funcs) to a parameter.  It could also alternatively be coded with the messages already started:

#+BEGIN_EXAMPLE
<synthtree or name of synthtree>
    .osc(param, <template or name>)
    .midi(param, <template or name>)
    .view(param, <template or name>)
    .pattern(param, <template or name>)
#+END_EXAMPLE

Binding a MultiControl to a PatternFunc: 

- Store the patternfunc under its name in the multicontrol dict. (maybe construct name from name of param ++ name of pattern func to avoid conflicts?.  Must rethink idea of multicontrol as dict, and the problem of naming.  Perhaps there exist alternative names for managing access to different controllers of a multicontrol, that do not involve names?) 

- Attach self to patternFunc via a notification action that goes something like: 

this.addNotifier(patternFunc, \value, { | value |
	this.set(value);
});

Different actions could be added instead of { | value | this.set(value) }.  
These could process (modify) / select / reject the values to be sent to the parameter, and could be composeable with binaryOps.  So one goes: 

multiparam.pattern(<template or name> <operator> <filter>);
alternatively with messages:
multiparam.pattern(<template or name>.add|mul|map|unmap|select|reject(<filter>));

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Important: Alternatively, a PatternFunc may store as currentValue an event with many parameters, and SynthTrees receiving notifications from it could play that event each in its own way. 

One could thus bind a whole SynthTree to a PatternFunc with the same operator: 

<patrernfunc or name> *> <synthtree or name> 

When no parameter is given as adverb to the *> operator, 
then the patternfunc is bound to play the whole synthtree. 

Alternatively: 

<synthtree or name>.patternPlay(<patternfunc or name>); 
shorter form: 
<synthtree or name>.pp(<patternfunc or name>); 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*/

PatternFunc { // Maybe should be PatternPlayer?
	var <name;
	var <valueTemplate;
	var <durationTemplate;
	var <valueStream;
	var <durationStream;
	var <currentValue;
	var <currentDuration;
	var <task;

	*new { | name, valueTemplate, durationTemplate, startNow = false |

	}

	play {
	}
	start {
	}
	stop {
	}

	reset {

	}

	// ???? : 
	enable {
	}

	disable {

	}
	
}