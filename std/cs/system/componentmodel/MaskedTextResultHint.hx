package cs.system.componentmodel;

/** Specifies values that succinctly describe the results of a masked text parsing operation. */
@:native("System.ComponentModel.MaskedTextResultHint")
extern enum MaskedTextResultHint {
	AlphanumericCharacterExpected;
	AsciiCharacterExpected;
	CharacterEscaped;
	DigitExpected;
	InvalidInput;
	LetterExpected;
	NoEffect;
	NonEditPosition;
	PositionOutOfRange;
	PromptCharNotAllowed;
	SideEffect;
	SignedDigitExpected;
	Success;
	UnavailableEditPosition;
	Unknown;
}
