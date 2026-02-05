package cs.system.diagnostics;

/** Specified how a new window should appear when the system starts a process. */
@:native("System.Diagnostics.ProcessWindowStyle")
extern enum abstract ProcessWindowStyle(Int) {
	var Hidden = 1;
	var Maximized = 3;
	var Minimized = 2;
	var Normal = 0;
}
