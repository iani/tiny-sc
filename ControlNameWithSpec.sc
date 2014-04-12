/*
Register a new spec in Specs, when creating a NamedControl.

{ SomeUgen.ar(\freq1.cr(\freq), \freq2.cr(\freq) ... ) ... }

\freq1 and \freq2 become namse of specs that return \freq.asSpec, 
so that the GUI makers can create guis with the appropriate mapping specs. 

IZ Sat, Apr 12 2014, 12:28 EEST
*/


+ Symbol {

	cr { | spec, lag, fixedLag = false |
		spec = spec.asSpec;
		Spec.specs.[this] = spec;
		^NamedControl.kr(this, spec.default, lag, fixedLag)
	}
}
