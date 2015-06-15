
//: SC 3.7 version
+ Knob {

	setPlaying {
		{ 
			this.color = [Color.grey(0.7), Color.red, Color.white, Color.red]
		}.defer
	}

	setStopped {
		{ 
			this.color = [Color.white, Color.black, Color.white, Color.black]
		}.defer
	}

	setFadeOut {
		{ 
			this.color = [Color.yellow, Color.yellow, Color.white, Color.black]
		}.defer
	}
}


+ Slider {

	setPlaying {
		{ 
			this.knobColor = Color.red;
			this.background = Color.red;
		}.defer
	}

	setStopped {
		{ 
			this.knobColor = Color.black;
			this.background = Color.white;
		}.defer
	}

	setFadeOut {
		{ 
			this.knobColor = Color.yellow;
			this.background = Color.yellow;
		}.defer
	}

}

// SC 3.6 Compatibility
+ QKnob {

	setPlaying {
		{ 
			this.color = [Color.grey(0.7), Color.red, Color.white, Color.red]
		}.defer
	}

	setStopped {
		{ 
			this.color = [Color.white, Color.black, Color.white, Color.black]
		}.defer
	}

	setFadeOut {
		{ 
			this.color = [Color.yellow, Color.yellow, Color.white, Color.black]
		}.defer
	}
}


+ QSlider {

	setPlaying {
		{ 
			this.knobColor = Color.red;
			this.background = Color.red;
		}.defer
	}

	setStopped {
		{ 
			this.knobColor = Color.black;
			this.background = Color.white;
		}.defer
	}

	setFadeOut {
		{ 
			this.knobColor = Color.yellow;
			this.background = Color.yellow;
		}.defer
	}

}
