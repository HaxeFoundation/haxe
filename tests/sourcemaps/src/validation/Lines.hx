package validation;

/**
 *  Represents some text content split in lines
 */
abstract Lines(Array<String>) from Array<String> {
	/**
	 *  Get content on the specified 1-based line number
	 */
	@:arrayAccess inline function get(lineNumber:Int):String {
		return this[lineNumber - 1];
	}
}