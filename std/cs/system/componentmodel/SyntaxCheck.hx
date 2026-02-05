package cs.system.componentmodel;

/** Provides methods to verify the machine name and path conform to a specific syntax. This class cannot be inherited. */
@:native("System.ComponentModel.SyntaxCheck")
extern class SyntaxCheck {
	/**
	 * Checks the syntax of the machine name to confirm that it does not contain "\".
	 * @param value A string containing the machine name to check.
	 * @return if  matches the proper machine name format; otherwise, .
	 */
	static function CheckMachineName(value:String):Bool;
	/**
	 * Checks the syntax of the path to see whether it starts with "\\".
	 * @param value A string containing the path to check.
	 * @return if  matches the proper path format; otherwise, .
	 */
	static function CheckPath(value:String):Bool;
	/**
	 * Checks the syntax of the path to see if it starts with "\" or drive letter "C:".
	 * @param value A string containing the path to check.
	 * @return if  matches the proper path format; otherwise, .
	 */
	static function CheckRootedPath(value:String):Bool;
}
