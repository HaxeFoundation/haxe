package cs.system.componentmodel;

/** Represents a mask-parsing service that can be used by any number of controls that support masking, such as the  control. */
@:native("System.ComponentModel.MaskedTextProvider")
extern class MaskedTextProvider {
	/**
	 * Gets the default password character used obscure user input.
	 * @return A  that represents the default password character.
	 */
	static var DefaultPasswordChar(default, never):cs.Char16;
	/**
	 * Gets the upper bound of the range of invalid indexes.
	 * @return A value representing the largest invalid index, as determined by the
	 * provider implementation. For example, if the lowest valid index is 0, this
	 * property will return -1.
	 */
	static var InvalidIndex(default, never):Int;
	/**
	 * Gets a value indicating whether the prompt character should be treated as a
	 * valid input character or not.
	 * @return if the user can enter  into the control; otherwise, . The default is .
	 */
	var AllowPromptAsInput(default, never):Bool;
	/**
	 * Gets a value indicating whether the mask accepts characters outside of the ASCII
	 * character set.
	 * @return if only ASCII is accepted;  if  can accept any arbitrary Unicode
	 * character. The default is .
	 */
	var AsciiOnly(default, never):Bool;
	/**
	 * Gets the number of editable character positions that have already been
	 * successfully assigned an input value.
	 * @return An  containing the number of editable character positions in the input
	 * mask that have already been assigned a character value in the formatted string.
	 */
	var AssignedEditPositionCount(default, never):Int;
	/**
	 * Gets the number of editable character positions in the input mask that have not
	 * yet been assigned an input value.
	 * @return An  containing the number of editable character positions that not yet
	 * been assigned a character value.
	 */
	var AvailableEditPositionCount(default, never):Int;
	/**
	 * Gets the culture that determines the value of the localizable separators and
	 * placeholders in the input mask.
	 * @return A  containing the culture information associated with the input mask.
	 */
	var Culture(default, never):cs.system.globalization.CultureInfo;
	/**
	 * Gets the number of editable positions in the formatted string.
	 * @return An  containing the number of editable positions in the formatted string.
	 */
	var EditPositionCount(default, never):Int;
	/**
	 * Gets a newly created enumerator for the editable positions in the formatted
	 * string.
	 * @return An  that supports enumeration over the editable positions in the
	 * formatted string.
	 */
	var EditPositions(default, never):cs.system.collections.IEnumerator;
	/**
	 * Gets or sets a value that indicates whether literal characters in the input mask
	 * should be included in the formatted string.
	 * @return if literals are included; otherwise, . The default is .
	 */
	var IncludeLiterals(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether  is used to represent the absence of
	 * user input when displaying the formatted string.
	 * @return if the prompt character is used to represent the positions where no user
	 * input was provided; otherwise, . The default is .
	 */
	var IncludePrompt(default, default):Bool;
	/**
	 * Gets or sets a value that determines whether password protection should be
	 * applied to the formatted string.
	 * @return if the input string is to be treated as a password string; otherwise, .
	 * The default is .
	 */
	var IsPassword(default, default):Bool;
	/**
	 * Gets the index in the mask of the rightmost input character that has been
	 * assigned to the mask.
	 * @return If at least one input character has been assigned to the mask, an 
	 * containing the index of rightmost assigned position; otherwise, if no position
	 * has been assigned, .
	 */
	var LastAssignedPosition(default, never):Int;
	/**
	 * Gets the length of the mask, absent any mask modifier characters.
	 * @return An  containing the number of positions in the mask, excluding characters
	 * that modify mask input.
	 */
	var Length(default, never):Int;
	/**
	 * Gets the input mask.
	 * @return A  containing the full mask.
	 */
	var Mask(default, never):String;
	/**
	 * Gets a value indicating whether all required inputs have been entered into the
	 * formatted string.
	 * @return if all required input has been entered into the mask; otherwise, .
	 */
	var MaskCompleted(default, never):Bool;
	/**
	 * Gets a value indicating whether all required and optional inputs have been
	 * entered into the formatted string.
	 * @return if all required and optional inputs have been entered; otherwise, .
	 */
	var MaskFull(default, never):Bool;
	/**
	 * Gets or sets the character to be substituted for the actual input characters.
	 * @return The  value used as the password character.
	 */
	var PasswordChar(default, default):cs.Char16;
	/**
	 * Gets or sets the character used to represent the absence of user input for all
	 * available edit positions.
	 * @return The character used to prompt the user for input. The default is an
	 * underscore (_).
	 */
	var PromptChar(default, default):cs.Char16;
	/**
	 * Gets or sets a value that determines how an input character that matches the
	 * prompt character should be handled.
	 * @return if the prompt character entered as input causes the current editable
	 * position in the mask to be reset; otherwise,  to indicate that the prompt
	 * character is to be processed as a normal input character. The default is .
	 */
	var ResetOnPrompt(default, default):Bool;
	/**
	 * Gets or sets a value that determines how a space input character should be
	 * handled.
	 * @return if the space input character causes the current editable position in the
	 * mask to be reset; otherwise,  to indicate that it is to be processed as a normal
	 * input character. The default is .
	 */
	var ResetOnSpace(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether literal character positions in the mask
	 * can be overwritten by their same values.
	 * @return to allow literals to be added back; otherwise,  to not allow the user to
	 * overwrite literal characters. The default is .
	 */
	var SkipLiterals(default, default):Bool;
	@:native("get_Item")
	function get_Item(index0:Int):cs.Char16;
	@:overload(function(mask:String):Void {})
	@:overload(function(mask:String, restrictToAscii:Bool):Void {})
	@:overload(function(mask:String, culture:cs.system.globalization.CultureInfo):Void {})
	@:overload(function(mask:String, passwordChar:cs.Char16, allowPromptAsInput:Bool):Void {})
	@:overload(function(mask:String, culture:cs.system.globalization.CultureInfo, restrictToAscii:Bool):Void {})
	@:overload(function(mask:String, culture:cs.system.globalization.CultureInfo, passwordChar:cs.Char16, allowPromptAsInput:Bool):Void {})
	function new(mask:String, culture:cs.system.globalization.CultureInfo, allowPromptAsInput:Bool, promptChar:cs.Char16, passwordChar:cs.Char16, restrictToAscii:Bool):Void;
	/**
	 * Determines whether the specified  denotes success or failure.
	 * @param hint A  value typically obtained as an output parameter from a previous
	 * operation.
	 * @return if the specified  value represents a success; otherwise,  if it
	 * represents failure.
	 */
	static function GetOperationResultFromHint(hint:cs.system.componentmodel.MaskedTextResultHint):Bool;
	/**
	 * Determines whether the specified character is a valid input character.
	 * @param c The  value to test.
	 * @return if the specified character contains a valid input value; otherwise .
	 */
	static function IsValidInputChar(c:cs.Char16):Bool;
	/**
	 * Determines whether the specified character is a valid mask character.
	 * @param c The  value to test.
	 * @return if the specified character contains a valid mask value; otherwise .
	 */
	static function IsValidMaskChar(c:cs.Char16):Bool;
	/**
	 * Determines whether the specified character is a valid password character.
	 * @param c The  value to test.
	 * @return if the specified character contains a valid password value; otherwise .
	 */
	static function IsValidPasswordChar(c:cs.Char16):Bool;
	@:overload(function(input:cs.Char16):Bool {})
	@:overload(function(input:String):Bool {})
	@:overload(function(input:cs.Char16, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool {})
	/**
	 * Adds the specified input character to the end of the formatted string.
	 * @param input A  value to be appended to the formatted string.
	 * @return if the input character was added successfully; otherwise .
	 */
	function Add(input:String, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	@:overload(function():Void {})
	/** Clears all the editable input characters from the formatted string, replacing them with prompt characters. */
	function Clear(resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Void;
	/**
	 * Creates a copy of the current .
	 * @return The  object this method creates, cast as an object.
	 */
	function Clone():Dynamic;
	/**
	 * Returns the position of the first assigned editable position after the specified
	 * position using the specified search direction.
	 * @param position The zero-based position in the formatted string to start the
	 * search.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * assigned editable position encountered; otherwise .
	 */
	function FindAssignedEditPositionFrom(position:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first assigned editable position between the
	 * specified positions using the specified search direction.
	 * @param startPosition The zero-based position in the formatted string where the
	 * search starts.
	 * @param endPosition The zero-based position in the formatted string where the
	 * search ends.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * assigned editable position encountered; otherwise .
	 */
	function FindAssignedEditPositionInRange(startPosition:Int, endPosition:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first editable position after the specified position
	 * using the specified search direction.
	 * @param position The zero-based position in the formatted string to start the
	 * search.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * editable position encountered; otherwise .
	 */
	function FindEditPositionFrom(position:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first editable position between the specified
	 * positions using the specified search direction.
	 * @param startPosition The zero-based position in the formatted string where the
	 * search starts.
	 * @param endPosition The zero-based position in the formatted string where the
	 * search ends.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * editable position encountered; otherwise .
	 */
	function FindEditPositionInRange(startPosition:Int, endPosition:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first non-editable position after the specified
	 * position using the specified search direction.
	 * @param position The zero-based position in the formatted string to start the
	 * search.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * literal position encountered; otherwise .
	 */
	function FindNonEditPositionFrom(position:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first non-editable position between the specified
	 * positions using the specified search direction.
	 * @param startPosition The zero-based position in the formatted string where the
	 * search starts.
	 * @param endPosition The zero-based position in the formatted string where the
	 * search ends.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * literal position encountered; otherwise .
	 */
	function FindNonEditPositionInRange(startPosition:Int, endPosition:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first unassigned editable position after the
	 * specified position using the specified search direction.
	 * @param position The zero-based position in the formatted string to start the
	 * search.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * unassigned editable position encountered; otherwise .
	 */
	function FindUnassignedEditPositionFrom(position:Int, direction:Bool):Int;
	/**
	 * Returns the position of the first unassigned editable position between the
	 * specified positions using the specified search direction.
	 * @param startPosition The zero-based position in the formatted string where the
	 * search starts.
	 * @param endPosition The zero-based position in the formatted string where the
	 * search ends.
	 * @param direction A  indicating the search direction; either  to search forward
	 * or  to search backward.
	 * @return If successful, an  representing the zero-based position of the first
	 * unassigned editable position encountered; otherwise .
	 */
	function FindUnassignedEditPositionInRange(startPosition:Int, endPosition:Int, direction:Bool):Int;
	@:overload(function(input:cs.Char16, position:Int):Bool {})
	@:overload(function(input:String, position:Int):Bool {})
	@:overload(function(input:cs.Char16, position:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool {})
	/**
	 * Inserts the specified character at the specified position within the formatted
	 * string.
	 * @param input The  to be inserted.
	 * @param position The zero-based position in the formatted string to insert the
	 * character.
	 * @return if the insertion was successful; otherwise, .
	 */
	function InsertAt(input:String, position:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	/**
	 * Determines whether the specified position is available for assignment.
	 * @param position The zero-based position in the mask to test.
	 * @return if the specified position in the formatted string is editable and has
	 * not been assigned to yet; otherwise .
	 */
	function IsAvailablePosition(position:Int):Bool;
	/**
	 * Determines whether the specified position is editable.
	 * @param position The zero-based position in the mask to test.
	 * @return if the specified position in the formatted string is editable; otherwise
	 * .
	 */
	function IsEditPosition(position:Int):Bool;
	@:overload(function():Bool {})
	/**
	 * Removes the last assigned character from the formatted string.
	 * @return if the character was successfully removed; otherwise, .
	 */
	function Remove(testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	@:overload(function(position:Int):Bool {})
	@:overload(function(startPosition:Int, endPosition:Int):Bool {})
	/**
	 * Removes the assigned character at the specified position from the formatted
	 * string.
	 * @param position The zero-based position of the assigned character to remove.
	 * @return if the character was successfully removed; otherwise, .
	 */
	function RemoveAt(startPosition:Int, endPosition:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	@:overload(function(input:cs.Char16, position:Int):Bool {})
	@:overload(function(input:String, position:Int):Bool {})
	@:overload(function(input:cs.Char16, position:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool {})
	@:overload(function(input:String, position:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool {})
	@:overload(function(input:cs.Char16, startPosition:Int, endPosition:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool {})
	/**
	 * Replaces a single character at or beyond the specified position with the
	 * specified character value.
	 * @param input The  value that replaces the existing value.
	 * @param position The zero-based position to search for the first editable
	 * character to replace.
	 * @return if the character was successfully replaced; otherwise, .
	 */
	function Replace(input:String, startPosition:Int, endPosition:Int, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	@:overload(function(input:String):Bool {})
	/**
	 * Sets the formatted string to the specified input string.
	 * @param input The  value used to set the formatted string.
	 * @return if all the characters were successfully set; otherwise, .
	 */
	function Set(input:String, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	/**
	 * Returns the formatted string in a displayable form.
	 * @return The formatted  that includes prompts and mask literals.
	 */
	function ToDisplayString():String;
	@:overload(function():String {})
	@:overload(function(ignorePasswordChar:Bool):String {})
	@:overload(function(includePrompt:Bool, includeLiterals:Bool):String {})
	@:overload(function(startPosition:Int, length:Int):String {})
	@:overload(function(ignorePasswordChar:Bool, startPosition:Int, length:Int):String {})
	@:overload(function(includePrompt:Bool, includeLiterals:Bool, startPosition:Int, length:Int):String {})
	/**
	 * Returns the formatted string that includes all the assigned character values.
	 * @return The formatted  that includes all the assigned character values.
	 */
	function ToString(ignorePasswordChar:Bool, includePrompt:Bool, includeLiterals:Bool, startPosition:Int, length:Int):String;
	/**
	 * Tests whether the specified character could be set successfully at the specified
	 * position.
	 * @param input The  value to test.
	 * @param position The position in the mask to test the input character against.
	 * @param hint A  that succinctly describes the result of the operation. An output
	 * parameter.
	 * @return if the specified character is valid for the specified position;
	 * otherwise, .
	 */
	function VerifyChar(input:cs.Char16, position:Int, hint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
	/**
	 * Tests whether the specified character would be escaped at the specified
	 * position.
	 * @param input The  value to test.
	 * @param position The position in the mask to test the input character against.
	 * @return if the specified character would be escaped at the specified position;
	 * otherwise, .
	 */
	function VerifyEscapeChar(input:cs.Char16, position:Int):Bool;
	@:overload(function(input:String):Bool {})
	/**
	 * Tests whether the specified string could be set successfully.
	 * @param input The  value to test.
	 * @return if the specified string represents valid input; otherwise, .
	 */
	function VerifyString(input:String, testPosition:cs.Ref<Int>, resultHint:cs.Ref<cs.system.componentmodel.MaskedTextResultHint>):Bool;
}
