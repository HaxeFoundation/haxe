// from vshaxe
import haxe.display.Position.Range;

enum abstract UnresolvedIdentifierSuggestion(Int) {
	var UISImport;
	var UISTypo;
}

enum abstract DiagnosticKind<T>(Int) from Int to Int {
	var DKUnusedImport:DiagnosticKind<Void>;
	var DKUnresolvedIdentifier:DiagnosticKind<Array<{kind:UnresolvedIdentifierSuggestion, name:String}>>;
	var DKCompilerError:DiagnosticKind<String>;
	var DKRemovableCode:DiagnosticKind<{description:String, range:Range}>;
	var DKParserError:DiagnosticKind<String>;
}

enum abstract DiagnosticSeverity(Int) {
	var Error = 1;
	var Warning;
	var Information;
	var Hint;
}

typedef Diagnostic<T> = {
	var kind:DiagnosticKind<T>;
	var range:Range;
	var severity:DiagnosticSeverity;
	var args:T;
}
