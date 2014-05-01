/*
Experimental JITlib operators. 

For assessment and functionality exploration only. 

To be converterd to tiny-sc operators, replacing the earlier tiny-sc framework, after completing the assessement. 

*/

+ Object {
	=>@ { | chuckee | ^chuckee jchuck: this; }
	asNdefSource { }
}


+ Symbol {
	jchuck { | chucker | ^Ndef(this, chucker.asNdefSource).play }
	asNdefSource { ^Mdef(this) }	
	<<> { | proxy, key = \in |
		^this.asProxy.perform('<<>', proxy.asProxy, key);
	}
	
	asProxy { ^Ndef(this) }
}

+ SequenceableCollection {
	asNdefSource { ^Pbind(*this) }
}

+ Event {
	asNdefSource { ^EventPattern(this) }
}