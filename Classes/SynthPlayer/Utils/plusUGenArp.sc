+ UGen {
	*arp { | ... args |  ^this.ar(*args) * \amp.kr(0.1) }
	*arps { | ... args |  ^Pan2.ar(this.ar(*args), \pos.kr(0), \amp.kr(0.1)) }
}