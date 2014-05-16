/* Hack for accessing a buffer stored in Library by BufferList from its symbol name

Fri, May 16 2014, 06:53 CEST
*/

+ Symbol {
	b { ^Library.at(Server.default, this) }
}