package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

/**
	Since 3.6.0
**/
typedef TypeDefinitionClientCapabilities = {
	/**
		Whether implementation supports dynamic registration. If this is set to `true`
		the client supports the new `TypeDefinitionRegistrationOptions` return value
		for the corresponding server capability as well.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports additional metadata in the form of definition links.

		Since 3.14.0
	**/
	var ?linkSupport:Bool;
}

typedef TypeDefinitionOptions = WorkDoneProgressOptions & {};
typedef TypeDefinitionRegistrationOptions = TextDocumentRegistrationOptions & TypeDefinitionOptions & StaticRegistrationOptions;
typedef TypeDefinitionParams = TextDocumentPositionParams & WorkDoneProgressParams & PartialResultParams;

/**
	A request to resolve the type definition locations of a symbol at a given text
	document position. The request's parameter is of type [TextDocumentPositioParams]
	(#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a
	Thenable that resolves to such.
**/
class TypeDefinitionRequest {
	public static inline var type = new RequestType<TypeDefinitionParams, Null<EitherType<Definition, Array<DefinitionLink>>>, NoData,
		TypeDefinitionRegistrationOptions>("textDocument/typeDefinition");

	public static final resultType = new ProgressType<EitherType<Array<Location>, Array<DefinitionLink>>>();
}
