/* 

Redo of Idef,
to fix shared stream bug. 

IZ Wed, Apr 30 2014, 19:03 EEST

Will add inheriting and naming from Idef, after making sure that multiple streams from same Edef work correctly. 
*/

Idef2 : EventStreamPlayer {
	var <>name;
	var <parent;
	var <children;
	var <mods; // locally modified elements: apply these on inherited pattern

	*new { | name, parent, protoEvent |
		^NameSpace(\Idef2, name, { 
			super.new(parent.asStream, protoEvent).initIdef(name, parent); 
		});
	}

	initIdef { | argName argParent |
		name = argName;
		this.parent = argParent;
		children = Set();
	}

	parent_ { | argParent |
		parent = argParent;
		parent.addChild(this);
	}

	inherit { | inEvent |
		[this, thisMethod.name, "should be debugged - inheritance causes shared stream."].postln;
		this.applyMods(inEvent);
	}

	applyMods { | inEvent |
		inEvent ?? { inEvent = originalStream.event.copy; };
		mods !? { 
			inEvent use: {
				mods keysValuesDo: { | key value | inEvent[key] = value.value } 
			};
		};
		inEvent keysValuesDo: { | key value | inEvent[key] = value.asStream };
		inEvent[\dur] ?? { inEvent[\dur] = 1 };
		originalStream.event = inEvent;
		this.propagate(inEvent);
	}

	propagate { | inEvent | children do: _.inherit(inEvent) }
}