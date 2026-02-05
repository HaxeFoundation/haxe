package cs.system.resources;

/** Instructs a  object to ask for a particular version of a satellite assembly. */
@:native("System.Resources.SatelliteContractVersionAttribute")
extern class SatelliteContractVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets the version of the satellite assemblies with the required resources.
	 * @return A string that contains the version of the satellite assemblies with the
	 * required resources.
	 */
	var Version(default, never):String;
	function new(version:String):Void;
}
