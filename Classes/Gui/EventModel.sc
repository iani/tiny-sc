/*
Use a Dict as a Model, connect gui elements to that model. 

Fri, May 30 2014, 08:34 EEST

*/

EventModel : Event {

	put { | key, value |
		super.put(key, value);
		this.changed(key, value);
	}

	gui { | initFunc, key = \gui |
		var window;
		this.push;
		window = Window.for(this, key, initFunc);
		this.pop;
		^window;
	}
}


+ QView {
	// Connecting to SynthTree
	stConnect { | synthTree, parameter |
		// Connect to SynthTree parameter
		parameter = synthTree.getParam(parameter);
		this.connect(
			parameter,
			\value,
			this.stSetAction(parameter),
			this.stUpdateAction
		);
	}

	stSetAction { | parameter | ^{ parameter.set(this.value) } }

	stUpdateAction { ^{ | value | this.value = value } }

	connect { | model, key, setAction, updateAction |
		// General method with custom set and update actions
		this.action = setAction;
		this.addNotifier(model, key, updateAction);
		this.onClose = { this.objectClosed };		
	}


	// Connecting to EventModel
	setter { | key, spec, model |
		model ?? { model = currentEnvironment };
		this.perform(this.connectToModelMethod, model, key, spec);
		this.onClose = { this.objectClosed };
	}

	connectToModelMethod { ^\simpleConnectToModel }

	simpleConnectToModel { | model key |
		this.action = { model[key] = this.value };
		this.addNotifier(model, key, { | value | this.value = value });
	}

	mappingConnectToModel { | model key spec |
		spec = ((spec ? key).asSpec).asSpec;
		this.action = { model[key] = spec.map(this.value) };
		this.addNotifier(model, key, { | value | this.value = spec.unmap(value)});
	}
}

+ QSlider {
	connectToModelMethod { ^\mappingConnectToModel }
}

+ QKnob {
	connectToModelMethod { ^\mappingConnectToModel }
}
