package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

// ---- capabilities

typedef FoldingRangeClientCapabilities = {
	/**
		Whether implementation supports dynamic registration for folding range providers. If this is set to `true`
		the client supports the new `FoldingRangeRegistrationOptions` return value for the corresponding server
		capability as well.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The maximum number of folding ranges that the client prefers to receive per document. The value serves as a
		hint, servers are free to follow the limit.
	**/
	var ?rangeLimit:Int;

	/**
		If set, the client signals that it only supports folding complete lines. If set, client will
		ignore specified `startCharacter` and `endCharacter` properties in a FoldingRange.
	**/
	var ?lineFoldingOnly:Bool;
}

typedef FoldingRangeOptions = WorkDoneProgressOptions & {};
typedef FoldingRangeRegistrationOptions = TextDocumentRegistrationOptions & FoldingRangeOptions & StaticRegistrationOptions;

/**
	Parameters for a [FoldingRangeRequest](#FoldingRangeRequest).
**/
typedef FoldingRangeParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;
}

/**
	A request to provide folding ranges in a document. The request's
	parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
	response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
	that resolves to such.
**/
class FoldingRangeRequest {
	public static inline var type = new RequestType<FoldingRangeParams, Null<Array<FoldingRange>>, NoData,
		FoldingRangeRegistrationOptions>("textDocument/foldingRange");

	public static final resultType = new ProgressType<Array<FoldingRange>>();
}
