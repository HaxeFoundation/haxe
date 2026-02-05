package cs.system;

/** Defines the parts of a URI for the  method. */
@:native("System.UriPartial")
extern enum abstract UriPartial(Int) {
	var Authority = 1;
	var Path = 2;
	var Query = 3;
	var Scheme = 0;
}
