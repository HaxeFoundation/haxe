package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

/**
	Since 3.6.0
**/
typedef ImplementationClientCapabilities = {
	/**
		Whether implementation supports dynamic registration. If this is set to `true`
		the client supports the new `ImplementationRegistrationOptions` return value
		for the corresponding server capability as well.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports additional metadata in the form of definition links.

		Since 3.14.0
	**/
	var ?linkSupport:Bool;
}

typedef ImplementationOptions = WorkDoneProgressOptions & {};
typedef ImplementationRegistrationOptions = TextDocumentRegistrationOptions & ImplementationOptions & StaticRegistrationOptions;
typedef ImplementationParams = TextDocumentPositionParams & WorkDoneProgressParams & PartialResultParams;

/**
	A request to resolve the implementation locations of a symbol at a given text
	document position. The request's parameter is of type [TextDocumentPositioParams]
	(#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
	Thenable that resolves to such.
**/
class ImplementationRequest {
	public static inline var type = new RequestType<ImplementationParams, Null<Definition>, NoData,
		TextDocumentRegistrationOptions>("textDocument/implementation");

	public static final resultType = new ProgressType<EitherType<Array<Location>, Array<DefinitionLink>>>();
}
