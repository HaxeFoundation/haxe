package hxparse;

import hxparse.Unexpected;
import hxparse.UnexpectedChar;
import hxparse.NoMatch;

/**
	This class provides some static utility methods.
**/
class Utils {

	/**
		Tries to invoke `f` and return its value, while catching the lexer and
		parser exceptions `hxparse.NoMatch`, `hxparse.Unexpected` and
		`hxparse.UnexpectedChar`.

		If no exception occurs, the result of `f` is returned.

		Otherwise the caught exception is rethrown as `String` in a human-
		readable representation and with positions formatted within `input`.

		If `input` or `f` are null, the result is unspecified.
	**/
	static public function catchErrors<T>(input:byte.ByteData, f:Void->T) {
		try {
			return f();
		} catch(e:ParserError) {
			throw e.pos.format(input) + ": " + e.toString();
		}
	}
}