/*
Sat, Jun 21 2014, 23:46 EEST


An EventModel that knows more about how to create its adapters from specs? 

Abstract class for TaskModel, EventPatternModel?

Experimental...

*/

PlayerModel : EventModel {
	//	var <specs; // for creating the model and the interface to it
	//	var <model; // the data driving the process
	/// var <process; // the playing process (task, synthtree...)
	// ... but multiple processes could be driven ...

	*new { | specs |

	}

	// headless gui - no start stop
	gui {} 
}
