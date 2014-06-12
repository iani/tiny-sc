/*
Modify SynthTree by adding an object.

Patterns:
- Event: modify BddfInstrument (EventStream).

Parameters: 
- SimpleNumber: Set a parameter
  BUFFER:
- Symbol: Set a buffer parameter
  BUS:
Function:  Play kr function into bus and map to parameter
Ref: Play kr synthdef into bus and map to parameter  
Bus: map to parameter

Thu, Jun 12 2014, 08:11 EEST
*/

+ Event {
	add2SynthTree {  | synthTree |
		// TODO: if template is not BdefInstrument, make one!
		synthTree.template addPlayerMods: this;
	}
}

/* // implemented in simpleNumberChuck, under +> method directly

+ SimpleNumber {
	add2SynthTree {  | synthTree paramName |
		[this, thisMethod.name, synthTree, paramName].postln;
		synthTree.set(paramName, this);
	}
}

*/

+ Symbol {
	add2SynthTree {  | synthTree paramName |

	}
}

+ Bus {
	add2SynthTree {  | synthTree paramName |

	}
}

+ Function {
	add2SynthTree {  | synthTree paramName |

	}
}

+ Ref {
	add2SynthTree {  | synthTree paramName |

	}
}
