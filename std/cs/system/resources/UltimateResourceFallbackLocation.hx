package cs.system.resources;

/** Specifies whether a  object looks for the resources of the app's default culture in the main assembly or in a satellite assembly. */
@:native("System.Resources.UltimateResourceFallbackLocation")
extern enum UltimateResourceFallbackLocation {
	MainAssembly;
	Satellite;
}
