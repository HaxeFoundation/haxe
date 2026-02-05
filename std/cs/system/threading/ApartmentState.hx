package cs.system.threading;

/** Specifies the apartment state of a . */
@:native("System.Threading.ApartmentState")
extern enum ApartmentState {
	MTA;
	STA;
	Unknown;
}
