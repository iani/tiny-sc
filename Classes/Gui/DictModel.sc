/*
Use a Dict as a Model, connect gui elements to that model. 

Fri, May 30 2014, 08:34 EEST
*/

DictModel : Event {

	put { | key, value |
		super.put(key, value);
		this.changed(key, value);
	}

	gui { | initFunc, key = \gui |
		var window;
		this.push;
		window = UniqueWindow.for(this, key, initFunc);
		this.pop;
		^window;
	}
}


+ QView {
	setter { | key, spec, model |
		model ?? { model = currentEnvironment };
		this.perform(this.connectToModelMethod.postln, model, key, spec);
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
