/*
Utility: shift a window's position;

IZ Thu, Mar 27 2014, 09:36 EET
*/

+ QWindow {
	shift { | x = 0, y = 0 |
		var bounds;
		bounds = this.bounds;
		this.bounds = bounds.left_(bounds.left + x).top_(bounds.top + y);
	}
	shiftTo { | x, y |
		var bounds;
		bounds = this.bounds;
		this.bounds = bounds.left_(x ?? { bounds.left }).top_(y ?? { bounds.top });
	}
}