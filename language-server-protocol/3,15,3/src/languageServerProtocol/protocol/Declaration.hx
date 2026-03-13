package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

/**
	Since 3.14.0
**/
typedef DeclarationClientCapabilities = {
	/**
		Whether declaration supports dynamic registration. If this is set to `true`
		the client supports the new `DeclarationRegistrationOptions` return value
		for the corresponding server capability as well.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports additional metadata in the form of declaration links.
	**/
	var ?linkSupport:Bool;
}

typedef DeclarationOptions = WorkDoneProgressOptions & {};
typedef DeclarationRegistrationOptions = DeclarationOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions;
typedef DeclarationParams = TextDocumentPositionParams & WorkDoneProgressParams & PartialResultParams;

/**
	A request to resolve the type definition locations of a symbol at a given text
	document position. The request's parameter is of type [TextDocumentPositioParams]
	(#TextDocumentPositionParams) the response is of type [Declaration](#Declaration)
	or a typed array of [DeclarationLink](#DeclarationLink) or a Thenable that resolves
	to such.
**/
class DeclarationRequest {
	public static inline var type = new RequestType<DeclarationParams, Null<EitherType<Declaration, Array<DeclarationLink>>>, NoData,
		DeclarationRegistrationOptions>("textDocument/declaration");

	public static final resultType = new ProgressType<EitherType<Array<Location>, Array<DeclarationLink>>>();
}
