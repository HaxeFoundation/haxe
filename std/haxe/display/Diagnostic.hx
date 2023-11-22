// from vshaxe
package haxe.display;

import haxe.display.Position.Location;
import haxe.display.Position.Range;
import haxe.display.JsonModuleTypes;

enum abstract UnresolvedIdentifierSuggestion(Int) {
	var UISImport;
	var UISTypo;
}

enum abstract MissingFieldCauseKind<T>(String) {
	final AbstractParent:MissingFieldCauseKind<{parent:JsonTypePathWithParams}>;
	final ImplementedInterface:MissingFieldCauseKind<{parent:JsonTypePathWithParams}>;
	final PropertyAccessor:MissingFieldCauseKind<{property:JsonClassField, isGetter:Bool}>;
	final FieldAccess:MissingFieldCauseKind<{}>;
	final FinalFields:MissingFieldCauseKind<{fields:Array<JsonClassField>}>;
}

typedef MissingFieldCause<T> = {
	var kind:MissingFieldCauseKind<T>;
	var args:T;
}

typedef MissingField = {
	var field:JsonClassField;
	var type:JsonType<Dynamic>;

	/**
		When implementing multiple interfaces, there can be field duplicates among them. This flag is only
		true for the first such occurrence of a field, so that the "Implement all" code action doesn't end
		up implementing the same field multiple times.
	**/
	var unique:Bool;
}

typedef MissingFieldDiagnostic = {
	var fields:Array<MissingField>;
	var cause:MissingFieldCause<Dynamic>;
}

typedef MissingFieldDiagnostics = {
	var moduleType:JsonModuleType<Dynamic>;
	var moduleFile:String;
	var entries:Array<MissingFieldDiagnostic>;
}

typedef ReplacableCode = {
	var description:String;
	var range:Range;
	var ?newCode:String;
}

enum abstract DiagnosticKind<T>(Int) from Int to Int {
	final DKUnusedImport:DiagnosticKind<Void>;
	final DKUnresolvedIdentifier:DiagnosticKind<Array<{kind:UnresolvedIdentifierSuggestion, name:String}>>;
	final DKCompilerError:DiagnosticKind<String>;
	final ReplacableCode:DiagnosticKind<ReplacableCode>;
	final DKParserError:DiagnosticKind<String>;
	final DeprecationWarning:DiagnosticKind<String>;
	final InactiveBlock:DiagnosticKind<Void>;
	final MissingFields:DiagnosticKind<MissingFieldDiagnostics>;
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
	var code:Null<String>;
	var relatedInformation:Array<DiagnosticRelatedInformation>;
}

typedef DiagnosticRelatedInformation = {
	var location:Location;
	var message:String;
	var depth:Int;
}
