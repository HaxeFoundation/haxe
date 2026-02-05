package cs.system.runtime.compilerservices;

/** Provides a static method to create a  object from a composite format string and its arguments. */
@:native("System.Runtime.CompilerServices.FormattableStringFactory")
extern class FormattableStringFactory {
	/**
	 * Creates a  instance from a composite format string and its arguments.
	 * @param format A composite format string.
	 * @param arguments The arguments whose string representations are to be inserted
	 * in the result string.
	 * @return The object that represents the composite format string and its
	 * arguments.
	 */
	static function Create(format:String, arguments:cs.NativeArray<Dynamic>):cs.system.FormattableString;
}
