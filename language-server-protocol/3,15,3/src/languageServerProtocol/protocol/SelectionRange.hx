package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

// ---- capabilities

typedef SelectionRangeClientCapabilities = {
	/**
		Whether implementation supports dynamic registration for selection range providers. If this is set to `true`
		the client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server
		capability as well.
	**/
	var ?dynamicRegistration:Bool;
}

typedef SelectionRangeOptions = WorkDoneProgressOptions & {};
typedef SelectionRangeRegistrationOptions = SelectionRangeOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions;

/**
	A parameter literal used in selection range requests.
**/
typedef SelectionRangeParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The positions inside the text document.
	**/
	var positions:Array<Position>;
}

/**
	A request to provide selection ranges in a document. The request's
	parameter is of type [SelectionRangeParams](#SelectionRangeParams), the
	response is of type [SelectionRange[]](#SelectionRange[]) or a Thenable
	that resolves to such.
**/
class SelectionRangeRequest {
	public static inline var type = new ProtocolRequestType<SelectionRangeParams, Null<Array<SelectionRange>>, Array<SelectionRange>, NoData,
		SelectionRangeRegistrationOptions>("textDocument/selectionRange");

	/** @deprecated  Use SelectionRangeRequest.type */
	public static final resultType = new ProgressType<Array<SelectionRange>>();
}
