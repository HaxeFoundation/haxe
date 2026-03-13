package languageServerProtocol.protocol.proposed;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

/**
	A set of predefined token types. This set is not fixed
	an clients can specify additional token types via the
	corresponding client capabilities.

	@since 3.16.0 - Proposed state
**/
enum abstract SemanticTokenTypes(String) {
	var Comment = "comment";
	var Keyword = "keyword";
	var String = "string";
	var Number = "number";
	var Regexp = "regexp";
	var Operator = "operator";
	var Namespace = "namespace";
	var Type = "type";
	var Struct = "struct";
	var Class = "class";
	var Interface = "interface";
	var Enum = "enum";
	var TypeParameter = "typeParameter";
	var Function = "function";
	var Member = "member";
	var Property = "property";
	var Macro = "macro";
	var Variable = "variable";
	var Parameter = "parameter";
	var Label = "label";
}

/**
	A set of predefined token modifiers. This set is not fixed
	an clients can specify additional token types via the
	corresponding client capabilities.
**/
enum abstract SemanticTokenModifiers(String) {
	var Documentation = "documentation";
	var Declaration = "declaration";
	var Definition = "definition";
	var Reference = "reference";
	var Static = "static";
	var Abstract = "abstract";
	var Deprecated = "deprected";
	var Async = "async";
	var Volatile = "volatile";
	var Final = "final";
}

typedef SemanticTokensLegend = {
	/**
		The token types a server uses.
	**/
	var tokenTypes:Array<String>;

	/**
		The token modifiers a server uses.
	**/
	var tokenModifiers:Array<String>;
}

typedef SemanticTokens = {
	/**
		An optional result id. If provided and clients support delta updating
		the client will include the result id in the next semantic token request.
		A server can then instead of computing all sematic tokens again simply
		send a delta.
	**/
	var ?resultId:String;

	/**
		The actual tokens
	**/
	var data:Array<Int>;
}

typedef SemanticTokensPartialResult = {
	var data:Array<Int>;
}

typedef SemanticTokensEdit = {
	var start:Int;
	var deleteCount:Int;
	var ?data:Array<Int>;
}

typedef SemanticTokensEdits = {
	final ?resultId:String;
	var edits:Array<SemanticTokensEdit>;
}

typedef SemanticTokensEditsPartialResult = {
	var edits:Array<SemanticTokensEdit>;
}

//------- 'textDocument/semanticTokens' -----

typedef SemanticTokensClientCapabilities = {
	/**
		The text document client capabilities
	**/
	var ?textDocument:{
		/**
			Capabilities specific to the `textDocument/semanticTokens`
		**/
		var ?semanticTokens:{
			/**
				Whether implementation supports dynamic registration. If this is set to `true`
				the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
				return value for the corresponding server capability as well.
			**/
			var ?dynamicRegistration:Bool;

			/**
				The token types know by the client.
			**/
			var tokenTypes:Array<String>;

			/**
				The token modifiers know by the client.
			**/
			var tokenModifiers:Array<String>;
		};
	};
}

typedef SemanticTokensOptions = WorkDoneProgressOptions & {
	/**
		The legend used by the server
	**/
	var legend:SemanticTokensLegend;

	/**
		Server supports providing semantic tokens for a sepcific range
		of a document.
	**/
	var ?rangeProvider:Bool;

	/**
		Server supports providing semantic tokens for a full document.
	**/
	var ?documentProvider:EitherType<Bool, {
		/**
			The server supports deltas for full documents.
		**/
		var ?edits:Bool;
	}>;
}

typedef SemanticTokensRegistrationOptions = TextDocumentRegistrationOptions & SemanticTokensOptions & StaticRegistrationOptions;

typedef SemanticTokensServerCapabilities = {
	var semanticTokensProvider:EitherType<SemanticTokensOptions, SemanticTokensRegistrationOptions>;
}

//------- 'textDocument/semanticTokens' -----

typedef SemanticTokensParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;
}

class SemanticTokensRequest {
	public static inline var type = new ProtocolRequestType<SemanticTokensParams, Null<SemanticTokens>, SemanticTokensPartialResult, NoData,
		SemanticTokensRegistrationOptions>("textDocument/semanticTokens");
}

//------- 'textDocument/semanticTokens/edits' -----

typedef SemanticTokensEditsParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The previous result id.
	**/
	var previousResultId:String;
}

class SemanticTokensEditsRequest {
	public static inline var type = new ProtocolRequestType<SemanticTokensEditsParams, Null<EitherType<SemanticTokens, SemanticTokensEdits>>,
		EitherType<SemanticTokensPartialResult, SemanticTokensEditsPartialResult>, NoData,
		SemanticTokensRegistrationOptions>("textDocument/semanticTokens/edits");
}

//------- 'textDocument/semanticTokens/range' -----

typedef SemanticTokensRangeParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The range the semantic tokens are requested for.
	**/
	var range:Range;
}

class SemanticTokensRangeRequest {
	public static inline var type = new ProtocolRequestType<SemanticTokensRangeParams, Null<SemanticTokens>, SemanticTokensPartialResult, NoData,
		NoData>("textDocument/semanticTokens/range");
}
