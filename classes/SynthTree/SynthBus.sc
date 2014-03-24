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
	var <bus;

}


SynthWithTemplate {
	// holds the template so it can restart
}

