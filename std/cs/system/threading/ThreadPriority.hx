package cs.system.threading;

/** Specifies the scheduling priority of a . */
@:native("System.Threading.ThreadPriority")
extern enum abstract ThreadPriority(Int) {
	var AboveNormal = 3;
	var BelowNormal = 1;
	var Highest = 4;
	var Lowest = 0;
	var Normal = 2;
}
