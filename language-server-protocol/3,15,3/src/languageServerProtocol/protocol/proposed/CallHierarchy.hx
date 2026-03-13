package languageServerProtocol.protocol.proposed;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.Protocol;

/**
	Represents programming constructs like functions or constructors in the context
	of call hierarchy.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyItem = {
	/**
		The name of this item.
	**/
	var name:String;

	/**
		The kind of this item.
	**/
	var kind:SymbolKind;

	/**
		Tags for this item.
	**/
	var ?tags:Array<SymbolTag>;

	/**
		More detail for this item, e.g. the signature of a function.
	**/
	var ?detail:String;

	/**
		The resource identifier of this item.
	**/
	var uri:DocumentUri;

	/**
		The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.
	**/
	var range:Range;

	/**
		The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
		Must be contained by the [`range`](#CallHierarchyItem.range).
	**/
	var selectionRange:Range;
}

/**
	Represents an incoming call, e.g. a caller of a method or constructor.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyIncomingCall = {
	/**
		The item that makes the call.
	**/
	var from:CallHierarchyItem;

	/**
		The range at which at which the calls appears. This is relative to the caller
		denoted by [`this.from`](#CallHierarchyIncomingCall.from).
	**/
	var fromRanges:Array<Range>;
}

/**
	Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyOutgoingCall = {
	/**
		The item that is called.
	**/
	var to:CallHierarchyItem;

	/**
		The range at which this item is called. This is the range relative to the caller, e.g the item
		passed to [`provideCallHierarchyOutgoingCalls`](#CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls)
		and not [`this.to`](#CallHierarchyOutgoingCall.to).
	**/
	var fromRanges:Array<Range>;
}

/**
	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyClientCapabilities = {
	/**
		Capabilities specific to the `textDocument/callHierarchy`

		@since 3.16.0 - Proposed state
	**/
	var ?callHierarchy:{
		/**
			Whether implementation supports dynamic registration. If this is set to `true`
			the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
			return value for the corresponding server capability as well.
		**/
		var ?dynamicRegistration:Bool;
	};
}

/**
	Call hierarchy options used during static registration.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyOptions = WorkDoneProgressOptions;

/**
	Call hierarchy options used during static or dynamic registration.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyRegistrationOptions = TextDocumentRegistrationOptions & CallHierarchyOptions;

/**
	The call hierarchy server capabilities.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyServerCapabilities = {
	/**
		The server provides Call Hierarchy support.
	**/
	var ?callHierarchyProvider:EitherType<Bool, EitherType<CallHierarchyOptions, CallHierarchyRegistrationOptions & StaticRegistrationOptions>>;
}

/**
	The parameter of a `textDocument/prepareCallHierarchy` request.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyPrepareParams = TextDocumentPositionParams & WorkDoneProgressParams;

/**
	A request to result a `CallHierarchyItem` in a document at a given position.
	Can be used as an input to a incoming or outgoing call hierarchy.

	@since 3.16.0 - Proposed state
**/
class CallHierarchyPrepareRequest {
	public static inline var type = new ProtocolRequestType<CallHierarchyPrepareParams, Null<Array<CallHierarchyItem>>, Never, NoData,
		CallHierarchyRegistrationOptions>("textDocument/prepareCallHierarchy");
}

/**
	The parameter of a `callHierarchy/incomingCalls` request.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyIncomingCallsParams = WorkDoneProgressParams &
	PartialResultParams & {
	var item:CallHierarchyItem;
}

/**
	A request to resolve the incoming calls for a given `CallHierarchyItem`.

	@since 3.16.0 - Proposed state
**/
class CallHierarchyIncomingCallsRequest {
	public static inline var type = new ProtocolRequestType<CallHierarchyIncomingCallsParams, Null<Array<CallHierarchyIncomingCall>>,
		Array<CallHierarchyIncomingCall>, NoData, NoData>("callHierarchy/incomingCalls");
}

/**
	The parameter of a `callHierarchy/outgoingCalls` request.

	@since 3.16.0 - Proposed state
**/
typedef CallHierarchyOutgoingCallsParams = WorkDoneProgressParams &
	PartialResultParams & {
	var item:CallHierarchyItem;
}

/**
	A request to resolve the outgoing calls for a given `CallHierarchyItem`.

	@since 3.16.0 - Proposed state
**/
class CallHierarchyOutgoingCallsRequest {
	public static inline var type = new ProtocolRequestType<CallHierarchyOutgoingCallsParams, Null<Array<CallHierarchyOutgoingCall>>,
		Array<CallHierarchyOutgoingCall>, NoData, NoData>("callHierarchy/outgoingCalls");
}
