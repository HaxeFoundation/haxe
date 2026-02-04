package cs.system;

@:native("System.OperatingSystem")
extern class OperatingSystem {
	// Platform returns System.PlatformID enum, use Dynamic and convert via __cs__
	var Platform(default, never):Dynamic;
}
