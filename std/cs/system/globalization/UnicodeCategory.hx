package cs.system.globalization;

/** Defines the Unicode category of a character. */
@:native("System.Globalization.UnicodeCategory")
extern enum abstract UnicodeCategory(Int) {
	var ClosePunctuation = 21;
	var ConnectorPunctuation = 18;
	var Control = 14;
	var CurrencySymbol = 26;
	var DashPunctuation = 19;
	var DecimalDigitNumber = 8;
	var EnclosingMark = 7;
	var FinalQuotePunctuation = 23;
	var Format = 15;
	var InitialQuotePunctuation = 22;
	var LetterNumber = 9;
	var LineSeparator = 12;
	var LowercaseLetter = 1;
	var MathSymbol = 25;
	var ModifierLetter = 3;
	var ModifierSymbol = 27;
	var NonSpacingMark = 5;
	var OpenPunctuation = 20;
	var OtherLetter = 4;
	var OtherNotAssigned = 29;
	var OtherNumber = 10;
	var OtherPunctuation = 24;
	var OtherSymbol = 28;
	var ParagraphSeparator = 13;
	var PrivateUse = 17;
	var SpaceSeparator = 11;
	var SpacingCombiningMark = 6;
	var Surrogate = 16;
	var TitlecaseLetter = 2;
	var UppercaseLetter = 0;
}
