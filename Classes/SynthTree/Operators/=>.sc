/* 
Review of => operator - see Roadmap.
Tue, Jun 17 2014, 17:23 EEST
*/

+ Object {
	=> { | chuckee adverb | chuckee.receiveChuck(this, adverb) }
}

+ SimpleNumber {
	=> { | chuckee paramName = \amp | ^chuckee.receiveNumberChuck(this, paramName) }
}

+ SynthTree {
	=> { | chuckee inputName = \in | ^chuckee.asSynthTree.addInputSynth(this, inputName) }
	receiveChuck { | chucker, inputName | this.addInputSynth(chucker, inputName ? \in) }
}

+ Symbol {
	//	receiveChuck { | chucker adverb | chucker.chuckInto(this.asSynthTree, adverb) }
	receiveNumberChuck { | number paramName = \amp | ^this.asSynthTree.set(paramName, number) }
}

+ View {
	=> { | st, paramName = \amp |
		// connect view to parameter of synthtree
		var param;
		param = st.asSynthTree.getParam(paramName);
		param.addNotifier(this, \value, { | val | param.mapSet(val) });
		param.addNotifier(this, \keydown, { | view key |
			switch (key, $ , { param.synthTree.toggle })
		});
		this.addNotifier(param, \value, { | value, unmappedValue | 
			this.value = unmappedValue 
		});
		this.value =  param.unmappedValue;
	}

	>| { | st, paramName = \amp |
		// remove view from parameter of synthtree
		var param;
		param = st.asSynthTree.getParam(paramName);
		param.removeNotifier(this, \value);
		param.removeNotifier(this, \keydown);
		this.removeNotifier(param, \value);
	}
}

+ NumberBox {
	=> { | st, paramName = \amp |
		var param;
		param = st.asSynthTree.getParam(paramName);
		param.addNotifier(this, \value, { | val | param.set(val) });
		param.addNotifier(this, \keydown, { | view key |
			switch (key, $ , { param.synthTree.toggle});
		});
		this.addNotifier(param, \value, { | value | this.value = value });
		this.value = param.nextValue;
	}
}

/*

+ ControlHolder {
	receiveChuck { | chucker, adverb |
	// chucker.chuckInto(this.asSynthTree, adverb);
	}
}

*/