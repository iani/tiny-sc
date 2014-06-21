/*
Use a Dict as a Model, connect gui elements to that model. 

Also optionally add MapAdapters as values.  This is a minimal scheme for sharing 
specs for multiple controls, GUI or other. 

Note: EventModel's methods could be added to Event, to avoid creating a new subclass.
However, these methods would make it impossible to set keys with the same name in Event. 
So EventModel subclass is defined here to avoid possible incompatibilities resulting
from the shadowing of these keys.

TODO: Use this to play routines and create to change their parameters.

a = EventModel().addKeys(\a, \b);
a.gui;

Fri, May 30 2014, 08:34 EEST

*/

EventModel : Event {

	/* Note: 
		If only used with adapters, then 
		modified put method can be replaced by mapPut and unmapPut
	*/
	/* // this was for EventModel : Event 
	put { | key, value |
		super.put(key, value);
		this.changed(key, value);
	}
	*/
	gui { | initFunc, key = \gui |
		var window;
		this.push;
		window = Window.for(this, key, initFunc);
		this.pop;
		^window;
	}

	getAdapter { | key, spec, default | ^this[key] ?? { this.addKey(key, spec, default); } }

	addKey { | key, spec, default |
		var adapter;
		adapter = MapAdapter(spec ? key, default);
		this.put(key, adapter);
		^adapter;
	}

	addKeys { | ... specs | specs do: this.addKey(*_) }
	// note: do not override default slider for object!
	simpleSlider { | key, spec, default | ^Slider().map(this, key, spec, default) }
	simpleKnob { | key, spec, default | ^Knob().map(this, key, spec, default) }
	simpleNumberBox { | key, spec, default | ^NumberBox().unmap(this, key, spec, default) }

	synthCtl { | synth, key, spec, default |
		this.getAdapter(key, spec, default);
		/* Note: synth releases the notifier when it ends - see Node:addNotifier */
		synth.addNotifier(this, key, { | val | synth.set(key, val.mappedValue) });
	}

	stCtl { | st, key, spec, default |
		var adapter, param;
		st = st.asSynthTree;
		adapter = this.getAdapter(key, spec, default);
		param = st.getParam(key);
		/*
			To update when SynthTree changes parameter from elsewhere,
			one must avoid re-setting the synthtree's parameter.
		*/
		st.addNotifier(this, key, { | val, sender |
			if (sender !== st) { st.set(key, val.mappedValue) };
		});
		this.addNotifier(st.getParam(key), \value, { | val |
			adapter.unmap(val);
			this.changed(key, adapter, st);
		});
	}

	mapPut { | key, value |
		this.changed(key, this[key].map(value));
	}

	unmapPut { | key, value |
		this.changed(key, this[key].unmap(value));
	}
}

MapAdapter {
	var <spec, <mappedValue, <unmappedValue;

	*new { | spec, default |
		^this.newCopyArgs(spec.asSpec ? NullSpec).initMapAdapter(default);
	}

	initMapAdapter { | default |
		this.unmap(default ?? { spec.default });
	}

	map { | argUnmappedValue |
		mappedValue = spec map: argUnmappedValue;
		unmappedValue = argUnmappedValue;
	}

	unmap { | argMappedValue |
		mappedValue = argMappedValue;
		unmappedValue = spec unmap: argMappedValue;
	}
}

+ View {
	
	connect { | model, key, setAction, updateAction |
		// General method with custom set and update actions
		this.action = setAction;
		this.addNotifier(model, key, updateAction);
		this.onClose = { this.objectClosed };		
	}

	map { | model, key, spec, default |
		/*  For sliders, knobs:
			My value is unmapped value (0 - 1).  
			When sending to model, I need to map it.
			When receiving from model, I need to get the unmappedValue.
			Note: adapter is cached.  Must replace manually if changed.
		*/
		var adapter;
		adapter = model[key] ?? { model.addKey(key, spec, default) }; 
		this.connect(model, key,
		{
			adapter map: this.value;
			model.changed(key, adapter, this);
		},
		{ | argAdapter | this.value = argAdapter.unmappedValue }
		);
		this.value = adapter.unmappedValue;
	}

	unmap { | model, key, spec, default |
		/*  For NumberBox:
			My value is mapped value.
			When sending to model, I need to unmap it.
			When receiving from model, I need to get the mappedValue.
			Note: adapter is cached.  Must replace manually if changed.
		*/
		var adapter;
		adapter = model[key] ?? { model.addKey(key, spec, default) }; 
		this.connect(model, key,
		{
			adapter unmap: this.value;
			model.changed(key, adapter, this);
		},
		{ | argAdapter | this.value = argAdapter.mappedValue }
		);
		this.value = adapter.mappedValue;
	}

	st { | st |
		// add behavior for SynthTree
		st = st.asSynthTree;
		this.keyDownAction = { | me, char |
			if (char === $ ) { st.toggle }
		}
	}
}
