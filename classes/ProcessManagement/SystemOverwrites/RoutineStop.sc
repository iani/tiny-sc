
+ Routine {
	stop {
		// Also notify ProcessRegistry.
		this.changed(\p_end);
		//		thisThread.changed(\p_end);		
		if (this === thisThread) { nil.alwaysYield } { this.prStop };
	}
}