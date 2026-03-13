package hxparse;

/**
	This is the base class of all parser errors.
**/
class ParserError {
	/**
		The position in the input where `this` exception occured.
	**/
	public var pos(default, null):Position;

	public function new(pos:Position) {
		this.pos = pos;
	}

	public function toString() {
		return "Parser error";
	}
}