/*
Copyright (c) 2016 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package json2object;

/**
 * Allow the print of error in the same format as the haxe compiler.
 */
class ErrorUtils {
	public static function convertError (e:Error) : String {
		var pos = switch (e) {
			case IncorrectType(_, _, pos) | IncorrectEnumValue(_, _, pos) | InvalidEnumConstructor(_, _, pos) | UninitializedVariable(_, pos) | UnknownVariable(_, pos) | ParserError(_, pos) | CustomFunctionException(_, pos): pos;
		}

		var header = "";
		if (pos != null) {
			var file = (pos.file == '') ? 'line' : '${pos.file}:';
			if (pos.lines.length == 1) {
				header = '${file}${pos.lines[0].number}: characters ${pos.lines[0].start}-${pos.lines[0].end} : ';
			}
			else if (pos.lines.length > 1) {
				header = '${file}${pos.lines[0].number}: lines ${pos.lines[0].number}-${pos.lines[pos.lines.length-1].number} : ';
			}
		}

		return switch (e)
		{
			case IncorrectType(variable, expected, _):
				header + 'Variable \'$variable\' should be of type \'$expected\'';

			case IncorrectEnumValue(variable, expected, _):
				header + 'Identifier \'$variable\' isn\'t part of \'$expected\'';

			case InvalidEnumConstructor(variable, expected, _):
				header + 'Enum argument \'$variable\' should be of type \'$expected\'';

			case UnknownVariable(variable, _):
				header + 'Variable \'$variable\' isn\'t part of the schema';

			case UninitializedVariable(variable, _):
				header + 'Variable \'$variable\' should be in the json';

			case ParserError(message, _):
				header + 'Parser error: $message';

			case CustomFunctionException(e, _):
				header + 'Custom function exception: $e';
		}
	}

	public static function convertErrorArray (e:Array<Error>) : String {
		return e.map(convertError).join("\n");
	}
}
