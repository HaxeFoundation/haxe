package cs.system.threading;

/** Specifies the apartment state of a . */
@:native("System.Threading.ApartmentState")
extern enum abstract ApartmentState(Int) {
	var MTA = 1;
	var STA = 0;
	var Unknown = 2;
}
