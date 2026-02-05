package cs.system.threading;

/** Specifies the scheduling priority of a . */
@:native("System.Threading.ThreadPriority")
extern enum ThreadPriority {
	AboveNormal;
	BelowNormal;
	Highest;
	Lowest;
	Normal;
}
