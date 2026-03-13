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
 * Error raised during an initialization by JSON.
 */
enum Error {
	/**
	 * Mismatch between instance type and json type.
	 *
	 * @param variable Variable affected.
	 * @param expected Expected type.
	 * @param pos Position in the JSON file.
	 */
	IncorrectType(variable:String, expected:String, pos:Position);

	/**
	 * Incorrect enum value.
	 *
	 * @param variable Value affected.
	 * @param expected Expected type.
	 * @param pos Position in the JSON file.
	 */
	IncorrectEnumValue(value:String, expected:String, pos:Position);

	/**
	 * Incorrect enum contructor.
	 *
	 * @param variable Value affected.
	 * @param expected Expected type.
	 * @param pos Position in the JSON file.
	 */
	InvalidEnumConstructor(value:String, expected:String, pos:Position);

	/**
	 * Variable has not been initialized.
	 *
	 * @param variable Variable affected.
	 * @param pos Position in the JSON file.
	 */
	UninitializedVariable(variable:String, pos:Position);

	/**
	 * No instance variable corresponding to this JSON variable.
	 *
	 * @param variable Variable affected.
	 * @param pos Position in the JSON file.
	 */
	UnknownVariable(variable:String, pos:Position);

	/**
	 * Incorrect JSON file formating.
	 *
	 * @param message Message raised.
	 * @param pos Position in the JSON file.
	 */
	ParserError(message:String, pos:Position);

	/**
	 * Custom function (@:jcustomparse or @:jcustomwrite) exception.
	 *
	 * @param e Exception raised.
	 * @param pos Position in the JSON file.
	 **/
	CustomFunctionException(e:Any, pos:Position);
}

#if haxe4 enum #else @:enum #end abstract ErrorType(Int) {
	var NONE = 0;
	var OBJECTTHROW = 1;
	var THROW = 2;
}

enum InternalError {
	AbstractNoJsonRepresentation(name:String);
	CannotGenerateSchema(name:String);
	HandleExpr;
	ParsingThrow;
	UnsupportedAbstractEnumType(name:String);
	UnsupportedEnumAbstractValue(name:String);
	UnsupportedMapKeyType(name:String);
	UnsupportedSchemaObjectType(name:String);
	UnsupportedSchemaType(type:String);
}

class CustomFunctionError {
	public #if haxe4 final #else var #end message:String;

	@:allow(json2object.reader.DataBuilder)
	@:allow(json2object.writer.DataBuilder)
	function new(message:String) {
		this.message = message;
	}
}
