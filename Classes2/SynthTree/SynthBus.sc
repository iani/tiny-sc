/* 
Dict holding a bus and any number of synths that write to it. 
For control busses written to by many synths.
(Could also be used for audio busses / is variant of SynthTree)

For mapping SynthTree parameters to busses into which write many synths. 

Actions: 
- Add a synth
- Replace a synth
- Add a synth source to be played sequentially when the previous one ends 
  (list of synths/synth sources).
- Remove a synth. 
- Restart. (!?)

- Alloc buss / free when all synths are done, and set param to last written value of bus.
  (This unmaps the synth parameter from the freed bus). 

IZ Mon, Mar 24 2014, 15:34 EET
*/

SynthBus : IdentityDictionary {
	var <output; // parameter that gets mapped to my bus
	var <bus;    // bus that the parameter maps to

	*start { | ... keys |
		var synths;
		if (keys.size == 0) { synths = this } { synths = keys collect: this[_] };
		synths.asArray do: { 
			
		}
	}

	*stop { | key |

	}
}


SynthWithTemplate {
	// holds the template so it can restart
	var <template; // Function, synthdef, Env, etc. for creating the synth
	var <synthBus; // synthbus that I send my output to
	// var <name;
	var <synth;    // synth that outputs to the synthBus

}

