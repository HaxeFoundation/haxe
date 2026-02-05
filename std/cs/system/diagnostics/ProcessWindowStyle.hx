package cs.system.diagnostics;

/** Specified how a new window should appear when the system starts a process. */
@:native("System.Diagnostics.ProcessWindowStyle")
extern enum ProcessWindowStyle {
	Hidden;
	Maximized;
	Minimized;
	Normal;
}
