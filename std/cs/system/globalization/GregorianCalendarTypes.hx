package cs.system.globalization;

/** Defines the different language versions of the Gregorian calendar. */
@:native("System.Globalization.GregorianCalendarTypes")
extern enum abstract GregorianCalendarTypes(Int) {
	var Arabic = 10;
	var Localized = 1;
	var MiddleEastFrench = 9;
	var TransliteratedEnglish = 11;
	var TransliteratedFrench = 12;
	var USEnglish = 2;
}
