/*
Shortcuts for accessing / registering various source objects from symbol-names

Tue, Jun 17 2014, 17:09 EEST

  - b :: buffer
  - p :: pattern, single, cloned from Edef
  - pm :: pattern, shared from Bdef
  - f :: function template (for kr synth)
  - k :: kr bus receiving input from one or more kr synths?
  - t :: template for ar synth (synthdef) to be played as source of synthtree.
  - o :: osc source (shared by any number of receivers via =changed=).
  - m :: midi source (shared by any number of receivers via =changed=). 
         Following shortcuts can access and or create+register such sources:
    - noteOn(...)
    - noteOff(...)
    - cc(...)
    - afterTouch(...)
    - pitchBend(...)
  - v :: view (shared by any number of receivers via =changed=).
  - st or s :: synthtree: links synthtree as input to right argument 
       (must be tested, alternative or replacement of =<)

*/

+ Symbol {

	b { ^BufferList.getBuffer(this) }
	p { ^Library.at(\patterns, this) }
	pm { ^Library.at(\bdefs, this) }
	f { ^Library.at(\krFuncs, this) }
	//	k { ^Library.at(\krSources, this) } // see KrMap
	t { ^Library.at(\synthTemplates, this) }
	m { ^Library.at(\midiSources, this) }
	o { ^Library.at(\oscSources, this) } // see also: OSCMap, Symbol:osc
	v { ^Library.at(\views, this) }
	s { | createIfMissing = true, defaultChuck |
		^this.st(createIfMissing, defaultChuck)
	}
	st { | createIfMissing = true, defaultChuck |	
		^this.asSynthTree(createIfMissing, defaultChuck);
	}
}