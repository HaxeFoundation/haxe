package cs.system.componentmodel.design;

/** Defines identifiers that indicate the type of a Help keyword. */
@:native("System.ComponentModel.Design.HelpKeywordType")
extern enum abstract HelpKeywordType(Int) {
	var F1Keyword = 0;
	var FilterKeyword = 2;
	var GeneralKeyword = 1;
}
