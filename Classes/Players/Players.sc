/*

Sun, Jun 22 2014, 00:18 EEST
*/


AbstractPlayer {
	var <model; // EventPlayerModel or TaskPlayerModel or ...
	var <process;


	start {}

	stop {}

	gui {}

}

EventPlayer : AbstractPlayer {

	//	gui {}
}


TaskPlayer : AbstractPlayer {

	//	gui {}

}


+ Object {

	eventPlayer { | specs |
		^specs eventPlayer: this;
	}

}

+ Function {

	taskPlayer { | specs |
		^specs taskPlayer: this;
	}

}

+ Array {

	eventPlayer { | st |
		^EventPlayer(EventPlayerModel(this), st.asSynthTree);
	}

	taskPlayer { | function |
		^TaskPlayer(TaskPlayerModel(this), function);
	}

}


