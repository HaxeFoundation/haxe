package cs.system.reflection;

/** Defines a copyright custom attribute for an assembly manifest. */
@:native("System.Reflection.AssemblyCopyrightAttribute")
extern class AssemblyCopyrightAttribute extends cs.system.Attribute {
	/**
	 * Gets copyright information.
	 * @return A string containing the copyright information.
	 */
	var Copyright(default, never):String;
	function new(copyright:String):Void;
}
