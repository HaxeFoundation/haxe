package cs.system.componentmodel;

/** Specifies values that succinctly describe the results of a masked text parsing operation. */
@:native("System.ComponentModel.MaskedTextResultHint")
extern enum abstract MaskedTextResultHint(Int) {
	var AlphanumericCharacterExpected = -2;
	var AsciiCharacterExpected = -1;
	var CharacterEscaped = 1;
	var DigitExpected = -3;
	var InvalidInput = -51;
	var LetterExpected = -4;
	var NoEffect = 2;
	var NonEditPosition = -54;
	var PositionOutOfRange = -55;
	var PromptCharNotAllowed = -52;
	var SideEffect = 3;
	var SignedDigitExpected = -5;
	var Success = 4;
	var UnavailableEditPosition = -53;
	var Unknown = 0;
}
