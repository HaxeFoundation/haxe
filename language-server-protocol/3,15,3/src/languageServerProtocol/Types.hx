package languageServerProtocol;

/**
	Position in a text document expressed as zero-based line and character offset.
	The offsets are based on a UTF-16 string representation. So a string of the form
	`a𐐀b` the character offset of the character `a` is 0, the character offset of `𐐀`
	is 1 and the character offset of b is 3 since `𐐀` is represented using two code
	units in UTF-16.

	Positions are line end character agnostic. So you can not specify a position that
	denotes `\r|\n` or `\n|` where `|` represents the character offset.
**/
typedef Position = {
	/**
		Line position in a document (zero-based).
		If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
		If a line number is negative, it defaults to 0.
	**/
	var line:Int;

	/**
		Character offset on a line in a document (zero-based). Assuming that the line is
		represented as a string, the `character` value represents the gap between the
		`character` and `character + 1`.

		If the character value is greater than the line length it defaults back to the
		line length.
		If a line number is negative, it defaults to 0.
	**/
	var character:Int;
}

/**
	A range in a text document expressed as (zero-based) start and end positions.

	If you want to specify a range that contains a line including the line ending
	character(s) then use an end position denoting the start of the next line.
	For example:
	```haxe
	{
		start: { line: 5, character: 23 }
		end : { line 6, character : 0 }
	}
	```
**/
typedef Range = {
	/**
		The range's start position
	**/
	var start:Position;

	/**
		The range's end position
	**/
	var end:Position;
}

/**
	Represents a location inside a resource, such as a line inside a text file.
**/
typedef Location = {
	var uri:DocumentUri;
	var range:Range;
}

/**
	Represents the connection of two locations. Provides additional metadata over normal [locations](#Location),
	including an origin range.
**/
typedef LocationLink = {
	/**
		Span of the origin of this link.

		Used as the underlined span for mouse definition hover. Defaults to the word range at
		the definition position.
	**/
	var ?originSelectionRange:Range;

	/**
		The target resource identifier of this link.
	**/
	var targetUri:DocumentUri;

	/**
		The full target range of this link. If the target for example is a symbol then target range is the
		range enclosing this symbol not including leading/trailing whitespace but everything else
		like comments. This information is typically used to highlight the range in the editor.
	**/
	var targetRange:Range;

	/**
		The range that should be selected and revealed when this link is being followed, e.g the name of a function.
		Must be contained by the the `targetRange`. See also `DocumentSymbol#range`
	**/
	var targetSelectionRange:Range;
}

/**
	Represents a color in RGBA space.
**/
typedef Color = {
	/**
		The red component of this color in the range [0-1].
	**/
	final red:Float;

	/**
		The green component of this color in the range [0-1].
	**/
	final green:Float;

	/**
		The blue component of this color in the range [0-1].
	**/
	final blue:Float;

	/**
		The alpha component of this color in the range [0-1].
	**/
	final alpha:Float;
}

/**
	Represents a color range from a document.
**/
typedef ColorInformation = {
	/**
		The range in the document where this color appers.
	**/
	var range:Range;

	/**
		The actual color value for this color range.
	**/
	var color:Color;
}

typedef ColorPresentation = {
	/**
		The label of this color presentation. It will be shown on the color
		picker header. By default this is also the text that is inserted when selecting
		this color presentation.
	**/
	var label:String;

	/**
		An edit which is applied to a document when selecting
		this presentation for the color. When `falsy` the `label`
		is used.
	**/
	var ?textEdit:TextEdit;

	/**
		An optional array of additional text edits that are applied when
		selecting this color presentation. Edits must not overlap with the main `edit` nor with themselves.
	**/
	var ?additionalTextEdits:Array<TextEdit>;
}

/**
	Enum of known range kinds
**/
enum abstract FoldingRangeKind(String) from String {
	/**
		Folding range for a comment
	**/
	var Comment = 'comment';

	/**
		Folding range for a imports or includes
	**/
	var Imports = 'imports';

	/**
		Folding range for a region (e.g. `#region`)
	**/
	var Region = 'region';
}

/**
	Represents a folding range.
**/
typedef FoldingRange = {
	/**
		The zero-based line number from where the folded range starts.
	**/
	var startLine:Int;

	/**
		The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
	**/
	var ?startCharacter:Int;

	/**
		The zero-based line number where the folded range ends.
	**/
	var endLine:Int;

	/**
		The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
	**/
	var ?endCharacter:Int;

	/**
		Describes the kind of the folding range such as `comment' or 'region'. The kind
		is used to categorize folding ranges and used by commands like 'Fold all comments'. See
		[FoldingRangeKind](#FoldingRangeKind) for an enumeration of standardized kinds.
	**/
	var ?kind:FoldingRangeKind;
}

/**
	Represents a related message and source code location for a diagnostic. This should be
	used to point to code locations that cause or related to a diagnostics, e.g when duplicating
	a symbol in a scope.
**/
typedef DiagnosticRelatedInformation = {
	/**
		The location of this related diagnostic information.
	**/
	var location:Location;

	/**
		The message of this related diagnostic information.
	**/
	var message:String;
}

/**
	The diagnostic's serverity.
**/
enum abstract DiagnosticSeverity(Int) {
	/**
		Reports an error.
	**/
	var Error = 1;

	/**
		Reports a warning.
	**/
	var Warning;

	/**
		Reports an information.
	**/
	var Information;

	/**
		Reports a hint.
	**/
	var Hint;
}

/**
	The diagnostic tags.

	@since 3.15.0
**/
enum abstract DiagnosticTag(Int) {
	/**
		Unused or unnecessary code.

		Clients are allowed to render diagnostics with this tag faded out instead of having
		an error squiggle.
	**/
	var Unnecessary = 1;

	/**
		Deprecated or obsolete code.

		Clients are allowed to rendered diagnostics with this tag strike through.
	**/
	var Deprecated = 2;
}

/**
	Represents a diagnostic, such as a compiler error or warning.
	Diagnostic objects are only valid in the scope of a resource.
**/
typedef Diagnostic = {
	/**
		The range at which the message applies
	**/
	var range:Range;

	/**
		The diagnostic's severity.
		If omitted it is up to the client to interpret diagnostics as error, warning, info or hint.
	**/
	var ?severity:DiagnosticSeverity;

	/**
		The diagnostic's code, which usually appear in the user interface.
	**/
	var ?code:EitherType<Int, String>;

	/**
		A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
	**/
	var ?source:String;

	/**
		The diagnostic's message. It usually appears in the user interface
	**/
	var message:String;

	/** 
		Additional metadata about the diagnostic.
	**/
	var ?tags:Array<DiagnosticTag>;

	/**
		An array of related diagnostic information, e.g. when symbol-names within
		a scope collide all definitions can be marked via this property.
	**/
	var ?relatedInformation:Array<DiagnosticRelatedInformation>;
}

/**
	Represents a reference to a command.
	Provides a title which will be used to represent a command in the UI and,
	optionally, an array of arguments which will be passed to the command handler function when invoked.
**/
typedef Command = {
	/**
		Title of the command, like `save`.
	**/
	var title:String;

	/**
		The identifier of the actual command handler.
	**/
	var command:String;

	/**
		Arguments that the command handler should be invoked with.
	**/
	var ?arguments:Array<Dynamic>;
}

/**
	A textual edit applicable to a text document.
**/
typedef TextEdit = {
	/**
		The range of the text document to be manipulated.
		To insert text into a document create a range where start == end.
	**/
	var range:Range;

	/**
		The string to be inserted.
		For delete operations use an empty string.
	**/
	var newText:String;
}

/**
	Describes textual changes on a text document. A TextDocumentEdit describes all changes
	on a document version Si and after they are applied move the document to version Si+1.
	So the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any
	kind of ordering. However the edits must be non overlapping.
**/
typedef TextDocumentEdit = {
	/**
		The text document to change.
	**/
	var textDocument:VersionedTextDocumentIdentifier;

	/**
		The edits to be applied.
	**/
	var edits:Array<TextEdit>;
}

enum abstract CreateFileKind(String) {
	var Create = "create";
}

/**
	Options to create a file.
**/
typedef CreateFileOptions = {
	/**
		Overwrite existing file. Overwrite wins over `ignoreIfExists`
	**/
	var ?overwrite:Bool;

	/**
		Ignore if exists.
	**/
	var ?ignoreIfExists:Bool;
}

/**
	Create file operation.
**/
typedef CreateFile = {
	/**
		A create
	**/
	var kind:CreateFileKind;

	/**
		The resource to create.
	**/
	var uri:DocumentUri;

	/**
		Additional options
	**/
	var ?options:CreateFileOptions;
}

enum abstract RenameFileKind(String) {
	var Kind = "rename";
}

/**
	Rename file options
**/
typedef RenameFileOptions = {
	/**
		Overwrite target if existing. Overwrite wins over `ignoreIfExists`
	**/
	var ?overwrite:Bool;

	/**
		Ignores if target exists.
	**/
	var ?ignoreIfExists:Bool;
}

/**
	Rename file operation
**/
typedef RenameFile = {
	/**
		A rename
	**/
	var kind:RenameFileKind;

	/**
		The old (existing) location.
	**/
	var oldUri:DocumentUri;

	/**
		The new location.
	**/
	var newUri:DocumentUri;

	/**
		Rename options.
	**/
	var ?options:RenameFileOptions;
}

enum abstract DeleteFileKind(String) {
	var Delete = "Delete";
}

/**
	Delete file options
**/
typedef DeleteFileOptions = {
	/**
		Delete the content recursively if a folder is denoted.
	**/
	var ?recursive:Bool;

	/**
		Ignore the operation if the file doesn't exist.
	**/
	var ?ignoreIfNotExists:Bool;
}

/**
	Delete file operation
**/
typedef DeleteFile = {
	/**
		A delete
	**/
	var kind:DeleteFileKind;

	/**
		The file to delete.
	**/
	var uri:DocumentUri;

	/**
		Delete options.
	**/
	var ?options:DeleteFileOptions;
}

/**
	A workspace edit represents changes to many resources managed in the workspace.
	The edit should either provide `changes` or `documentChanges`.
	If `documentChanges` are present they are preferred over `changes` if the client
	can handle versioned document edits.
**/
typedef WorkspaceEdit = {
	/**
		Holds changes to existing resources.
	**/
	var ?changes:haxe.DynamicAccess<Array<TextEdit>>;

	/**
		Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
		are either an array of `TextDocumentEdit`s to express changes to n different text documents
		where each text document edit addresses a specific version of a text document. Or it can contain
		above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.

		Whether a client supports versioned document edits is expressed via
		`workspace.workspaceEdit.documentChanges` client capability.

		If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
		only plain `TextEdit`s using the `changes` property are supported.
	**/
	var ?documentChanges:Array<EitherType<TextDocumentEdit, EitherType<CreateFile, EitherType<RenameFile, DeleteFile>>>>;
}

/**
	A tagging type for string properties that are actually URIs.
**/
abstract DocumentUri(String) {
	public inline function new(uri:String) {
		this = uri;
	}

	public inline function toString() {
		return this;
	}
}

/**
	A literal to identify a text document in the client.
**/
typedef TextDocumentIdentifier = {
	/**
		The text document's uri.
	**/
	var uri:DocumentUri;
}

/**
	An identifier to denote a specific version of a text document.
**/
typedef VersionedTextDocumentIdentifier = TextDocumentIdentifier & {
	/**
		The version number of this document. If a versioned text document identifier
		is sent from the server to the client and the file is not open in the editor
		(the server has not received an open notification before) the server can send
		`null` to indicate that the version is known and the content on disk is the
		truth (as speced with document content ownership).
	**/
	var version:Int;
}

/**
	An item to transfer a text document from the client to the server.
**/
typedef TextDocumentItem = {
	/**
		The text document's uri.
	**/
	var uri:DocumentUri;

	/**
		The text document's language identifier.
	**/
	var languageId:String;

	/**
		The version number of this document (it will strictly increase after each change, including undo/redo).
	**/
	var version:Int;

	/**
		The content of the opened text document.
	**/
	var text:String;
}

/**
	Describes the content type that a client supports in various
	result literals like `Hover`, `ParameterInfo` or `CompletionItem`.

	Please note that `MarkupKinds` must not start with a `$`. This kinds
	are reserved for internal usage.
**/
enum abstract MarkupKind(String) {
	/**
		Plain text is supported as a content format
	**/
	var PlainText = "plaintext";

	/**
		Markdown is supported as a content format
	**/
	var MarkDown = "markdown";
}

/**
	A `MarkupContent` literal represents a string value which content is interpreted base on its
	kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.

	If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
	See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

	Here is an example how such a string can be constructed using JavaScript / TypeScript:
	```ts
	let markdown: MarkdownContent = {
		kind: MarkupKind.Markdown,
		value: [
			'# Header',
			'Some text',
			'```typescript',
			'someCode();',
			'```'
		].join('\n')
	};
	```

	*Please Note* that clients might sanitize the return markdown. A client could decide to
	remove HTML from the markdown to avoid script execution.
**/
typedef MarkupContent = {
	/**
		The type of the Markup
	**/
	var kind:MarkupKind;

	/**
		The content itself
	**/
	var value:String;
}

/**
	The kind of a completion entry.
**/
enum abstract CompletionItemKind(Int) to Int {
	var Text = 1;
	var Method;
	var Function;
	var Constructor;
	var Field;
	var Variable;
	var Class;
	var Interface;
	var Module;
	var Property;
	var Unit;
	var Value;
	var Enum;
	var Keyword;
	var Snippet;
	var Color;
	var File;
	var Reference;
	var Folder;
	var EnumMember;
	var Constant;
	var Struct;
	var Event;
	var Operator;
	var TypeParameter;
}

/**
	Defines whether the insert text in a completion item should be interpreted as
	plain text or a snippet.
**/
enum abstract InsertTextFormat(Int) {
	/**
		The primary text to be inserted is treated as a plain string.
	**/
	var PlainText = 1;

	/**
		The primary text to be inserted is treated as a snippet.

		A snippet can define tab stops and placeholders with `$1`, `$2`
		and `${3:foo}`. `$0` defines the final tab stop, it defaults to
		the end of the snippet. Placeholders with equal identifiers are linked,
		that is typing in one will update others too.

		See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
	**/
	var Snippet = 2;
}

/**
	Completion item tags are extra annotations that tweak the rendering of a completion
	item.

	@since 3.15.0
**/
enum abstract CompletionItemTag(Int) {
	/**
		Render a completion as obsolete, usually using a strike-out.
	**/
	var Deprecated = 1;
}

/**
	A completion item represents a text snippet that is
	proposed to complete text that is being typed.
**/
typedef CompletionItem = {
	/**
		The label of this completion item.
		By default also the text that is inserted when selecting this completion.
	**/
	var label:String;

	/**
		The kind of this completion item.
		Based of the kind an icon is chosen by the editor.
	**/
	var ?kind:CompletionItemKind;

	/**
		Tags for this completion item.

		@since 3.15.0
	**/
	var ?tags:Array<CompletionItemTag>;

	/**
		A human-readable string with additional information about this item, like type or symbol information.
	**/
	var ?detail:String;

	/**
		A human-readable string that represents a doc-comment.
	**/
	var ?documentation:EitherType<String, MarkupContent>;

	/**
		Indicates if this item is deprecated.
		@deprecated Use `tags` instead.
	**/
	var ?deprecated:Bool;

	/**
		Select this item when showing.

		*Note* that only one completion item can be selected and that the
		tool / client decides which item that is. The rule is that the *first*
		item of those that match best is selected.
	**/
	var ?preselect:Bool;

	/**
		A string that should be used when comparing this item with other items.
		When `falsy` the label is used.
	**/
	var ?sortText:String;

	/**
		A string that should be used when filtering a set of completion items.
		When `falsy` the label is used.
	**/
	var ?filterText:String;

	/**
		A string that should be inserted into a document when selecting
		this completion. When `falsy` the [label](#CompletionItem.label)
		is used.

		The `insertText` is subject to interpretation by the client side.
		Some tools might not take the string literally. For example
		VS Code when code complete is requested in this example `con<cursor position>`
		and a completion item with an `insertText` of `console` is provided it
		will only insert `sole`. Therefore it is recommended to use `textEdit` instead
		since it avoids additional client side interpretation.
	**/
	var ?insertText:String;

	/**
		The format of the insert text. The format applies to both the `insertText` property
		and the `newText` property of a provided `textEdit`. If ommitted defaults to
		`InsertTextFormat.PlainText`.
	**/
	var ?insertTextFormat:InsertTextFormat;

	/**
		A `TextEdit` which is applied to a document when selecting
		this completion. When an edit is provided the value of
		`insertText` is ignored.

		*Note:* The text edit's range must be a [single line] and it must contain the position
		at which completion has been requested.
	**/
	var ?textEdit:TextEdit;

	/**
		An optional array of additional [text edits](#TextEdit) that are applied when
		selecting this completion. Edits must not overlap (including the same insert position)
		with the main [edit](#CompletionItem.textEdit) nor with themselves.

		Additional text edits should be used to change text unrelated to the current cursor position
		(for example adding an import statement at the top of the file if the completion item will
		insert an unqualified type).
	**/
	var ?additionalTextEdits:Array<TextEdit>;

	/**
		An optional set of characters that when pressed while this completion is active will accept it first and
		then type that character. *Note* that all commit characters should have `length=1` and that superfluous
		characters will be ignored.
	**/
	var ?commitCharacters:Array<String>;

	/**
		An optional command that is executed *after* inserting this completion. *Note* that
		additional modifications to the current document should be described with the
		additionalTextEdits-property.
	**/
	var ?command:Command;

	/**
		An data entry field that is preserved on a completion item between a completion and a completion resolve request.
	**/
	var ?data:Dynamic;
}

/**
	Represents a collection of completion items to be presented in the editor.
**/
typedef CompletionList = {
	/**
		This list it not complete. Further typing should result in recomputing this list.
	**/
	var isIncomplete:Bool;

	/**
		The completion items.
	**/
	var items:Array<CompletionItem>;
}

/**
	MarkedString can be used to render human readable text. It is either a markdown string
	or a code-block that provides a language and a code snippet. The language identifier
	is semantically equal to the optional language identifier in fenced code blocks in GitHub
	issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

	The pair of a language and a value is an equivalent to markdown:
	```${language}
	${value}
	```

	Note that markdown strings will be sanitized - that means html will be escaped.
	@deprecated use MarkupContent instead
**/
typedef MarkedString = EitherType<String, {language:String, value:String}>;

/**
	The result of a hover request.
**/
typedef Hover = {
	/**
		The hover's content.
	**/
	var contents:EitherType<MarkupContent, EitherType<MarkedString, Array<MarkedString>>>;

	/**
		An optional range.
	**/
	var ?range:Range;
}

/**
	Represents a parameter of a callable-signature.
	A parameter can have a label and a doc-comment.
**/
typedef ParameterInformation = {
	/**
		The label of this parameter information.

		Either a string or an inclusive start and exclusive end offsets within its containing
		signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
		string representation as `Position` and `Range` does.

		*Note*: a label of type string should be a substring of its containing signature label.
		Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.
	**/
	var label:EitherType<String, Array<Int>>;

	/**
		The human-readable doc-comment of this signature.
		Will be shown in the UI but can be omitted.
	**/
	var ?documentation:EitherType<String, MarkupContent>;
}

/**
	Represents the signature of something callable.
	A signature can have a label, like a function-name, a doc-comment, and a set of parameters.
**/
typedef SignatureInformation = {
	/**
		The label of this signature.
		Will be shown in the UI.
	**/
	var label:String;

	/**
		The human-readable doc-comment of this signature.
		Will be shown in the UI but can be omitted.
	**/
	var ?documentation:EitherType<String, MarkupContent>;

	/**
		The parameters of this signature.
	**/
	var ?parameters:Array<ParameterInformation>;
}

/**
	Signature help represents the signature of something callable.
	There can be multiple signature but only one active and only one active parameter.
**/
typedef SignatureHelp = {
	/**
		One or more signatures.
	**/
	var signatures:Array<SignatureInformation>;

	/**
		The active signature. Set to `null` if no
		signatures exist.
	**/
	var ?activeSignature:Int;

	/**
		The active parameter of the active signature. Set to `null`
		if the active signature has no parameters.
	**/
	var ?activeParameter:Int;
}

/**
	The definition of a symbol represented as one or many [locations](#Location).
	For most programming languages there is only one location at which a symbol is
	defined.

	Servers should prefer returning `DefinitionLink` over `Definition` if supported
	by the client.
**/
typedef Definition = Null<EitherType<Location, Array<Location>>>;

/**
	Information about where a symbol is defined.

	Provides additional metadata over normal [location](#Location) definitions, including the range of
	the defining symbol
**/
typedef DefinitionLink = LocationLink;

/**
	The declaration of a symbol representation as one or many [locations](#Location).
**/
typedef Declaration = EitherType<Location, Array<Location>>;

/**
	Information about where a symbol is declared.

	Provides additional metadata over normal [location](#Location) declarations, including the range of
	the declaring symbol.

	Servers should prefer returning `DeclarationLink` over `Declaration` if supported
	by the client.
**/
typedef DeclarationLink = LocationLink;

/**
	Value-object that contains additional information when
	requesting references.
**/
typedef ReferenceContext = {
	/**
		Include the declaration of the current symbol.
	**/
	var includeDeclaration:Bool;
}

/**
	A document highlight kind.
**/
enum abstract DocumentHighlightKind(Int) to Int {
	/**
		A textual occurrence.
	**/
	var Text = 1;

	/**
		Read-access of a symbol, like reading a variable.
	**/
	var Read;

	/**
		Write-access of a symbol, like writing to a variable.
	**/
	var Write;
}

/**
	A document highlight is a range inside a text document which deserves special attention.
	Usually a document highlight is visualized by changing the background color of its range.
**/
typedef DocumentHighlight = {
	/**
		The range this highlight applies to.
	**/
	var range:Range;

	/**
		The highlight kind, default is `DocumentHighlightKind.Text`.
	**/
	var ?kind:DocumentHighlightKind;
}

/**
	A symbol kind.
**/
enum abstract SymbolKind(Int) to Int {
	var File = 1;
	var Module;
	var Namespace;
	var Package;
	var Class;
	var Method;
	var Property;
	var Field;
	var Constructor;
	var Enum;
	var Interface;
	var Function;
	var Variable;
	var Constant;
	var String;
	var Number;
	var Boolean;
	var Array;
	var Object;
	var Key;
	var Null;
	var EnumMember;
	var Struct;
	var Event;
	var Operator;
	var TypeParameter;
}

/**
	Symbol tags are extra annotations that tweak the rendering of a symbol.
	@since 3.15
**/
enum abstract SymbolTag(Int) {
	/**
		Render a symbol as obsolete, usually using a strike-out.
	**/
	var Deprecated = 1;
}

/**
	Represents information about programming constructs like variables, classes, interfaces etc.
**/
typedef SymbolInformation = {
	/**
		The name of this symbol.
	**/
	var name:String;

	/**
		The kind of this symbol.
	**/
	var kind:SymbolKind;

	/**
		Indicates if this symbol is deprecated.
	**/
	var ?deprecated:Bool;

	/**
		The location of this symbol. The location's range is used by a tool
		to reveal the location in the editor. If the symbol is selected in the
		tool the range's start information is used to position the cursor. So
		the range usually spans more than the actual symbol's name and does
		normally include thinks like visibility modifiers.

		The range doesn't have to denote a node range in the sense of a abstract
		syntax tree. It can therefore not be used to re-construct a hierarchy of
		the symbols.
	**/
	var location:Location;

	/**
		The name of the symbol containing this symbol. This information is for
		user interface purposes (e.g. to render a qualifier in the user interface
		if necessary). It can't be used to re-infer a hierarchy for the document
		symbols.
	**/
	var ?containerName:String;
}

/**
	Represents programming constructs like variables, classes, interfaces etc.
	that appear in a document. Document symbols can be hierarchical and they
	have two ranges: one that encloses its definition and one that points to
	its most interesting range, e.g. the range of an identifier.
**/
typedef DocumentSymbol = {
	/**
		The name of this symbol. Will be displayed in the user interface and therefore must not be
		an empty string or a string only consisting of white spaces.
	**/
	var name:String;

	/**
		More detail for this symbol, e.g the signature of a function.
	**/
	var ?detail:String;

	/**
		The kind of this symbol.
	**/
	var kind:SymbolKind;

	/**
		Indicates if this symbol is deprecated.
	**/
	var ?deprecated:Bool;

	/**
		The range enclosing this symbol not including leading/trailing whitespace but everything else
		like comments. This information is typically used to determine if the the clients cursor is
		inside the symbol to reveal in the symbol in the UI.
	**/
	var range:Range;

	/**
		The range that should be selected and reveal when this symbol is being picked, e.g the name of a function.
		Must be contained by the the `range`.
	**/
	var selectionRange:Range;

	/**
		Children of this symbol, e.g. properties of a class.
	**/
	var ?children:Array<DocumentSymbol>;
}

/**
	The kind of a code action.

	Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.

	The set of kinds is open and client needs to announce the kinds it supports to the server during
	initialization.

	This enum has a set of predefined code action kinds.
**/
enum abstract CodeActionKind(String) from String to String {
	/**
		Empty kind.
	**/
	var Empty = '';

	/**
		Base kind for quickfix actions: 'quickfix'
	**/
	var QuickFix = 'quickfix';

	/**
		Base kind for refactoring actions: 'refactor'
	**/
	var Refactor = 'refactor';

	/**
		Base kind for refactoring extraction actions: 'refactor.extract'

		Example extract actions:

		- Extract method
		- Extract function
		- Extract variable
		- Extract interface from class
		- ...
	**/
	var RefactorExtract = 'refactor.extract';

	/**
		Base kind for refactoring inline actions: 'refactor.inline'

		Example inline actions:

		- Inline function
		- Inline variable
		- Inline constant
		- ...
	**/
	var RefactorInline = 'refactor.inline';

	/**
		Base kind for refactoring rewrite actions: 'refactor.rewrite'

		Example rewrite actions:

		- Convert JavaScript function to class
		- Add or remove parameter
		- Encapsulate field
		- Make method static
		- Move method to base class
		- ...
	**/
	var RefactorRewrite = 'refactor.rewrite';

	/**
		Base kind for source actions: `source`

		Source code actions apply to the entire file.
	**/
	var Source = 'source';

	/**
		Base kind for an organize imports source action: `source.organizeImports`
	**/
	var SourceOrganizeImports = 'source.organizeImports';

	/**
		Base kind for auto-fix source actions: `source.fixAll`.

		Fix all actions automatically fix errors that have a clear fix that do not require user input.
		They should not suppress errors or perform unsafe fixes such as generating new types or classes.

		@since 3.15.0
	**/
	var SourceFixAll = 'source.fixAll';
}

/**
	Contains additional diagnostic information about the context in which a code action is run.
**/
typedef CodeActionContext = {
	/**
		An array of diagnostics known on the client side overlapping the range provided to the
		`textDocument/codeAction` request. They are provied so that the server knows which
		errors are currently presented to the user for the given range. There is no guarantee
		that these accurately reflect the error state of the resource. The primary parameter
		to compute code actions is the provided range.
	**/
	var diagnostics:Array<Diagnostic>;

	/**
		Requested kind of actions to return.

		Actions not of this kind are filtered out by the client before being shown. So servers
		can omit computing them.
	**/
	var ?only:Array<CodeActionKind>;
}

/**
	A code action represents a change that can be performed in code, e.g. to fix a problem or
	to refactor code.

	A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.
**/
typedef CodeAction = {
	/**
		A short, human-readable, title for this code action.
	**/
	var title:String;

	/**
		The kind of the code action.

		Used to filter code actions.
	**/
	var ?kind:CodeActionKind;

	/**
		The diagnostics that this code action resolves.
	**/
	var ?diagnostics:Array<Diagnostic>;

	/**
		Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
		by keybindings.

		A quick fix should be marked preferred if it properly addresses the underlying error.
		A refactoring should be marked preferred if it is the most reasonable choice of actions to take.

		@since 3.15.0
	**/
	var ?isPreferred:Bool;

	/**
		The workspace edit this code action performs.
	**/
	var ?edit:WorkspaceEdit;

	/**
		A command this code action executes. If a code action
		provides a edit and a command, first the edit is
		executed and then the command.
	**/
	var ?command:Command;
}

/**
	A code lens represents a command that should be shown along with source text,
	like the number of references, a way to run tests, etc.

	A code lens is _unresolved_ when no command is associated to it.
	For performance reasons the creation of a code lens and resolving should be done to two stages.
**/
typedef CodeLens = {
	/**
		The range in which this code lens is valid.
		Should only span a single line.
	**/
	var range:Range;

	/**
		The command this code lens represents.
	**/
	var ?command:Command;

	/**
		An data entry field that is preserved on a code lens item between a code lens and a code lens resolve request.
	**/
	var ?data:Dynamic;
}

/**
	Value-object describing what options formatting should use.
	This object can contain additional fields of type Bool/Int/Float/String.
**/
typedef FormattingOptions = {
	/**
		Size of a tab in spaces.
	**/
	var tabSize:Int;

	/**
		Prefer spaces over tabs.
	**/
	var insertSpaces:Bool;

	/**
		Trim trailing whitespaces on a line.

		@since 3.15.0
	**/
	var ?trimTrailingWhitespace:Bool;

	/**
		Insert a newline character at the end of the file if one does not exist.

		@since 3.15.0
	**/
	var ?insertFinalNewline:Bool;

	/**
		Trim all newlines after the final newline at the end of the file.

		@since 3.15.0
	**/
	var ?trimFinalNewlines:Bool;
}

/**
	A document link is a range in a text document that links to an internal or external resource, like another
	text document or a web site.
**/
typedef DocumentLink = {
	/**
		The range this link applies to.
	**/
	var range:Range;

	/**
		The uri this link points to. If missing a resolve request is sent later.
	**/
	var ?target:DocumentUri;

	/**
		The tooltip text when you hover over this link.

		If a tooltip is provided, is will be displayed in a string that includes instructions on how to
		trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
		user settings, and localization.

		@since 3.15.0
	**/
	var ?tooltip:String;

	/**
		A data entry field that is preserved on a document link between a
		DocumentLinkRequest and a DocumentLinkResolveRequest.
	**/
	var ?data:Dynamic;
}

/**
	A selection range represents a part of a selection hierarchy. A selection range
	may have a parent selection range that contains it.
**/
typedef SelectionRange = {
	/**
		The [range](#Range) of this selection range.
	**/
	var range:Range;

	/**
		The parent selection range containing this range. Therefore `parent.range` must contain `this.range`.
	**/
	var ?parent:SelectionRange;
}

/**
	Represents reasons why a text document is saved.
**/
enum abstract TextDocumentSaveReason(Int) {
	/**
		Manually triggered, e.g. by the user pressing save, by starting debugging, or by an API call.
	**/
	var Manual = 1;

	/**
		Automatic after a delay.
	**/
	var AfterDelay;

	/**
		When the editor lost focus.
	**/
	var FocusOut;
}

/**
	An event describing a change to a text document. If range and rangeLength are omitted
	the new text is considered to be the full content of the document.
**/
typedef TextDocumentContentChangeEvent = {
	/**
		The range of the document that changed.
	**/
	var ?range:Range;

	/**
		The optional length of the range that got replaced.

		@deprecated use range instead.
	**/
	var ?rangeLength:Int;

	/**
		The new text of the range/document.
	**/
	var text:String;
}
