package languageServerProtocol.protocol;

import languageServerProtocol.Types;
import languageServerProtocol.protocol.ColorProvider;
import languageServerProtocol.protocol.Configuration;
import languageServerProtocol.protocol.Declaration;
import languageServerProtocol.protocol.FoldingRange;
import languageServerProtocol.protocol.Implementation;
import languageServerProtocol.protocol.Progress;
import languageServerProtocol.protocol.SelectionRange;
import languageServerProtocol.protocol.TypeDefinition;
import languageServerProtocol.protocol.WorkspaceFolders;

typedef RequestType<TParams, TResponse, TError, TRegistrationOptions> = jsonrpc.Types.RequestType<TParams, TResponse, TError>;
typedef NotificationType<TParams, TRegistrationOptions> = jsonrpc.Types.NotificationType<TParams>;

abstract ResponseError<T>(ResponseErrorData) to ResponseErrorData {
	public static inline var RequestCancelled = -32800;
	public static inline var ContentModified = -32801;
}

/**
	A document filter denotes a document by different properties like
	the `TextDocument.languageId`, the `Uri.scheme` of
	its resource, or a glob-pattern that is applied to the `TextDocument.fileName`.

	Glob patterns can have the following syntax:
	- `*` to match one or more characters in a path segment
	- `?` to match on one character in a path segment
	- `**` to match any number of path segments, including none
	- `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
	- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
	- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

	@sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
	@sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`
**/
typedef DocumentFilter = {
	/**
		A language id, like `haxe`.
	**/
	var ?language:String;

	/**
		A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
	**/
	var ?scheme:String;

	/**
		A glob pattern, like `*.{hx,hxml}`.
	**/
	var ?pattern:String;
}

/**
	A document selector is the combination of one or many document filters.

	@sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;
**/
typedef DocumentSelector = Array<EitherType<String, DocumentFilter>>;

/**
	General parameters to to register for an notification or to register a provider.
**/
typedef Registration = {
	/**
		The id used to register the request. The id can be used to deregister
		the request again.
	**/
	var id:String;

	/**
		The method to register for.
	**/
	var method:String;

	/**
		Options necessary for the registration.
	**/
	var ?registerOptions:Dynamic;
}

typedef RegistrationParams = {
	var registrations:Array<Registration>;
}

/**
	The `client/registerCapability` request is sent from the server to the client to register a new capability
	handler on the client side.
**/
class RegistrationRequest {
	public static inline var type = new RequestType<RegistrationParams, NoData, NoData, NoData>("client/registerCapability");
}

/**
	General parameters to unregister a request or notification.
**/
typedef Unregistration = {
	/**
		The id used to unregister the request or notification. Usually an id
		provided during the register request.
	**/
	var id:String;

	/**
		The method / capability to unregister for.
	**/
	var method:String;
}

typedef UnregistrationParams = {
	// Should correctly be named `unregistrations`. However
	// this is a breaking change which has to wait for
	// protocol version 4.0.
	var unregisterations:Array<Unregistration>;
}

/**
	The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability
	handler on the client side.
**/
class UnregistrationRequest {
	public static inline var type = new RequestType<UnregistrationParams, NoData, NoData, NoData>("client/unregisterCapability");
}

typedef WorkDoneProgressParams = {
	/**
		An optional token that a server can use to report work done progress.
	**/
	var ?workDoneToken:ProgressToken;
}

typedef PartialResultParams = {
	/**
		An optional token that a server can use to report partial results (e.g. streaming) to
		the client.
	**/
	var ?partialResultToken:ProgressToken;
}

/**
	A parameter literal used in requests to pass a text document and a position inside that document.
**/
typedef TextDocumentPositionParams = {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The position inside the text document.
	**/
	var position:Position;
}

//---- Initialize Method ----

/**
	The kind of resource operations supported by the client.
**/
enum abstract ResourceOperationKind(String) {
	/**
		Supports creating new files and folders.
	**/
	var Create = "create";

	/**
		Supports renaming existing files and folders.
	**/
	var Rename = "rename";

	/**
		Supports deleting existing files and folders.
	**/
	var Delete = "delete";
}

enum abstract FailureHandlingKind(String) {
	/**
		Applying the workspace change is simply aborted if one of the changes provided
		fails. All operations executed before the failing operation stay executed.
	**/
	var Abort = "abort";

	/**
		All operations are executed transactional. That means they either all
		succeed or no changes at all are applied to the workspace.
	**/
	var Transactional = "transactional";

	/**
		If the workspace edit contains only textual file changes they are executed transactional.
		If resource changes (create, rename or delete file) are part of the change the failure
		handling startegy is abort.
	**/
	var TextOnlyTransactional = "textOnlyTransactional";

	/**
		The client tries to undo the operations already executed. But there is no
		guaruntee that this is succeeding.
	**/
	var Undo = "undo";
}

/**
	Workspace specific client capabilities.

	Define capabilities the editor / tool provides on the workspace.
**/
typedef WorkspaceClientCapabilites = ConfigurationClientCapabilities &
	WorkspaceFoldersClientCapabilities & {
	/**
		The client supports applying batch edits to the workspace by supporting
		the request 'workspace/applyEdit'
	**/
	var ?applyEdit:Bool;

	/**
		Capabilities specific to `WorkspaceEdit`s
	**/
	var ?workspaceEdit:WorkspaceEditClientCapabilities;

	/**
		Capabilities specific to the `workspace/didChangeConfiguration` notification.
	**/
	var ?didChangeConfiguration:DidChangeConfigurationClientCapabilities;

	/**
		Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
	**/
	var ?didChangeWatchedFiles:DidChangeWatchedFilesClientCapabilities;

	/**
		Capabilities specific to the `workspace/symbol` request.
	**/
	var ?symbol:WorkspaceSymbolClientCapabilities;

	/**
		Capabilities specific to the `workspace/executeCommand` request.
	**/
	var ?executeCommand:ExecuteCommandClientCapabilities;
}

/**
	Text document specific client capabilities.
**/
typedef TextDocumentClientCapabilities = {
	/**
		Defines which synchronization capabilities the client supports.
	**/
	var ?synchronization:TextDocumentSyncClientCapabilities;

	/**
		Capabilities specific to the `textDocument/completion`
	**/
	var ?completion:CompletionClientCapabilities;

	/**
		Capabilities specific to the `textDocument/hover`
	**/
	var ?hover:HoverClientCapabilities;

	/**
		Capabilities specific to the `textDocument/signatureHelp`
	**/
	var ?signatureHelp:SignatureHelpClientCapabilities;

	/**
		Capabilities specific to the `textDocument/declaration`

		@since 3.14.0
	**/
	var ?declaration:DeclarationClientCapabilities;

	/**
		Capabilities specific to the `textDocument/definition`
	**/
	var ?definition:DefinitionClientCapabilities;

	/**
		Capabilities specific to the `textDocument/typeDefinition`

		@since 3.6.0
	**/
	var ?typeDefinition:TypeDefinitionClientCapabilities;

	/**
		Capabilities specific to the `textDocument/implementation`

		@since 3.6.0
	**/
	var ?implementation:ImplementationClientCapabilities;

	/**
		Capabilities specific to the `textDocument/references`
	**/
	var ?references:ReferenceClientCapabilities;

	/**
		Capabilities specific to the `textDocument/documentHighlight`
	**/
	var ?documentHighlight:DocumentHighlightClientCapabilities;

	/**
		Capabilities specific to the `textDocument/documentSymbol`
	**/
	var ?documentSymbol:DocumentSymbolClientCapabilities;

	/**
		Capabilities specific to the `textDocument/codeAction`
	**/
	var ?codeAction:CodeActionClientCapabilities;

	/**
		Capabilities specific to the `textDocument/codeLens`
	**/
	var ?codeLens:CodeLensClientCapabilities;

	/**
		Capabilities specific to the `textDocument/documentLink`
	**/
	var ?documentLink:DocumentLinkClientCapabilities;

	/**
		Capabilities specific to the `textDocument/documentColor`
	**/
	var ?colorProvider:DocumentColorClientCapabilities;

	/**
		Capabilities specific to the `textDocument/formatting`
	**/
	var ?formatting:DocumentFormattingClientCapabilities;

	/**
		Capabilities specific to the `textDocument/rangeFormatting`
	**/
	var ?rangeFormatting:DocumentRangeFormattingClientCapabilities;

	/**
		Capabilities specific to the `textDocument/onTypeFormatting`
	**/
	var ?onTypeFormatting:DocumentOnTypeFormattingClientCapabilities;

	/**
		Capabilities specific to the `textDocument/rename`
	**/
	var ?rename:RenameClientCapabilities;

	/**
		Capabilities specific to `textDocument/foldingRange` requests.

		@since 3.10.0
	**/
	var ?foldingRange:FoldingRangeClientCapabilities;

	/**
		Capabilities specific to `textDocument/selectionRange` requests

		@since 3.15.0
	**/
	var ?selectionRange:SelectionRangeClientCapabilities;

	/**
		Capabilities specific to `textDocument/publishDiagnostics`.
	**/
	var ?publishDiagnostics:PublishDiagnosticsClientCapabilities;
}

/**
	Define capabilities for dynamic registration, workspace and text document features the client supports.
	The `experimental` can be used to pass experimential capabilities under development.
	For future compatibility a `ClientCapabilities` object literal can have more properties set than currently defined.
	Servers receiving a `ClientCapabilities` object literal with unknown properties should ignore these properties.
	A missing property should be interpreted as an absence of the capability.
**/
typedef ClientCapabilities = {
	/**
		Workspace specific client capabilities.
	**/
	var ?workspace:WorkspaceClientCapabilites;

	/**
		Text document specific client capabilities.
	**/
	var ?textDocument:TextDocumentClientCapabilities;

	/**
		Window specific client capabilities.
	**/
	var ?window:WorkDoneProgressClientCapabilities;

	/**
		Experimental client capabilities.
	**/
	var ?experimental:Dynamic;
}

/**
	Static registration options to be returned in the initialize
	request.
**/
typedef StaticRegistrationOptions = {
	/**
		The id used to register the request. The id can be used to deregister
		the request again. See also Registration#id.
	**/
	var ?id:String;
}

/**
	General text document registration options.
**/
typedef TextDocumentRegistrationOptions = {
	/**
		A document selector to identify the scope of the registration. If set to null
		the document selector provided on the client side will be used.
	**/
	var documentSelector:Null<DocumentSelector>;
}

/**
	Save options.
**/
typedef SaveOptions = {
	/**
		The client is supposed to include the content on save.
	**/
	var ?includeText:Bool;
}

typedef WorkDoneProgressOptions = {
	var ?workDoneProgress:Bool;
}

/**
	Defines the capabilities provided by a language
	server.
**/
typedef ServerCapabilities = WorkspaceFoldersServerCapabilities & {
	/**
		Defines how text documents are synced.
		Is either a detailed structure defining each notification or for backwards compatibility the TextDocumentSyncKind number.
	**/
	var ?textDocumentSync:EitherType<TextDocumentSyncOptions, TextDocumentSyncKind>;

	/**
		The server provides hover support.
	**/
	var ?hoverProvider:EitherType<Bool, HoverOptions>;

	/**
		The server provides completion support.
	**/
	var ?completionProvider:CompletionOptions;

	/**
		The server provides signature help support.
	**/
	var ?signatureHelpProvider:SignatureHelpOptions;

	/**
		The server provides Goto Declaration support.
	**/
	var ?declarationProvider:EitherType<Bool, EitherType<DeclarationOptions, DeclarationRegistrationOptions>>;

	/**
		The server provides goto definition support.
	**/
	var ?definitionProvider:EitherType<Bool, DefinitionOptions>;

	/**
		The server provides Goto Type Definition support.
	**/
	var ?typeDefinitionProvider:EitherType<Bool, EitherType<TypeDefinitionOptions, TypeDefinitionRegistrationOptions>>;

	/**
		The server provides Goto Implementation support.
	**/
	var ?implementationProvider:EitherType<Bool, EitherType<ImplementationOptions, ImplementationRegistrationOptions>>;

	/**
		The server provides find references support.
	**/
	var ?referencesProvider:EitherType<Bool, ReferenceOptions>;

	/**
		The server provides document highlight support.
	**/
	var ?documentHighlightProvider:EitherType<Bool, DocumentHighlightOptions>;

	/**
		The server provides document symbol support.
	**/
	var ?documentSymbolProvider:EitherType<Bool, DocumentSymbolOptions>;

	/**
		The server provides workspace symbol support.
	**/
	var ?workspaceSymbolProvider:EitherType<Bool, WorkspaceSymbolOptions>;

	/**
		The server provides code actions. CodeActionOptions may only be
		specified if the client states that it supports
		`codeActionLiteralSupport` in its initial `initialize` request.
	**/
	var ?codeActionProvider:EitherType<Bool, CodeActionOptions>;

	/**
		The server provides code lens.
	**/
	var ?codeLensProvider:CodeLensOptions;

	/**
		The server provides document link support.
	**/
	var ?documentLinkProvider:DocumentLinkOptions;

	/**
		The server provides color provider support.
	**/
	var ?colorProvider:EitherType<Bool, EitherType<DocumentColorOptions, DocumentColorRegistrationOptions>>;

	/**
		The server provides document formatting.
	**/
	var ?documentFormattingProvider:EitherType<Bool, DocumentFormattingOptions>;

	/**
		The server provides document range formatting.
	**/
	var ?documentRangeFormattingProvider:EitherType<Bool, DocumentRangeFormattingOptions>;

	/**
		The server provides document formatting on typing.
	**/
	var ?documentOnTypeFormattingProvider:DocumentOnTypeFormattingOptions;

	/**
		The server provides rename support. RenameOptions may only be
		specified if the client states that it supports
		`prepareSupport` in its initial `initialize` request.
	**/
	var ?renameProvider:EitherType<Bool, RenameOptions>;

	/**
		The server provides folding provider support.
	**/
	var ?foldingRangeProvider:EitherType<Bool, EitherType<FoldingRangeOptions, FoldingRangeRegistrationOptions>>;

	/**
		The server provides selection range support.
	**/
	var ?selectionRangeProvider:EitherType<Bool, EitherType<SelectionRangeOptions, SelectionRangeRegistrationOptions>>;

	/**
		The server provides execute command support.
	**/
	var ?executeCommandProvider:ExecuteCommandOptions;

	/**
		Experimental server capabilities.
	**/
	var ?experimental:Dynamic;
}

/**
	The initialize request is sent from the client to the server.
	It is sent once as the request after starting up the server.
	The requests parameter is of type [InitializeParams](#InitializeParams)
	the response if of type [InitializeResult](#InitializeResult) of a Thenable that
	resolves to such.
**/
class InitializeRequest {
	public static inline var type = new RequestType<InitializeParams & WorkDoneProgressParams, InitializeResult, InitializeError, NoData>("initialize");
}

/**
	The initialize parameters
**/
typedef InitializeParams = WorkspaceFoldersInitializeParams & {
	/**
		The process Id of the parent process that started the server.
		Is null if the process has not been started by another process.
		If the parent process is not alive then the server should exit (see exit notification) its process.
	**/
	var processId:Null<Int>;

	/**
		Information about the client

		@since 3.15.0
	**/
	var ?clientInfo:{
		/**
			The name of the client as defined by the client.
		**/
		var name:String;

		/**
			The client's version as defined by the client.
		**/
		var ?version:String;
	};

	/**
		The rootPath of the workspace.
		Is null if no folder is open.
	**/
	@:deprecated("deprecated in favour of rootUri")
	var rootPath:Null<String>;

	/**
		The rootUri of the workspace.
		Is null if no folder is open.
		If both `rootPath` and `rootUri` are set `rootUri` wins.
	**/
	// @:deprecated("deprecated in favour of workspaceFolders")
	var rootUri:Null<DocumentUri>;

	/**
		The capabilities provided by the client (editor or tool).
	**/
	var capabilities:ClientCapabilities;

	/**
		User provided initialization options.
	**/
	var ?initializationOptions:Dynamic;

	/**
		The initial trace setting.
		If omitted trace is disabled ('off').
	**/
	var ?trace:TraceMode;
}

enum abstract TraceMode(String) to String {
	var Off = "off";
	var Messages = "messages";
	var Verbose = "verbose";
}

/**
	The result returned from an initialize request.
	This object can contain additional fields of type String.
**/
typedef InitializeResult = {
	/**
		The capabilities the language server provides.
	**/
	var capabilities:ServerCapabilities;

	/**
		Information about the server.

		@since 3.15.0
	**/
	var ?serverInfo:{
		/**
			The name of the server as defined by the server.
		**/
		var name:String;

		/**
			The servers's version as defined by the server.
		**/
		var ?version:String;
	};
}

/**
	The data type of the ResponseError if the
	initialize request fails.
**/
typedef InitializeError = {
	/**
		Indicates whether the client execute the following retry logic:
		(1) show the message provided by the ResponseError to the user
		(2) user selects retry or cancel
		(3) if user selected retry the initialize method is sent again.
	**/
	var retry:Bool;
}

typedef InitializedParams = {}

/**
	The intialized notification is sent from the client to the
	server after the client is fully initialized and the server
	is allowed to send requests from the server to the client.
**/
class InitializedNotification {
	public static inline var type = new NotificationType<InitializedParams, NoData>("initialized");
}

//---- Shutdown Method ----

/**
	A shutdown request is sent from the client to the server.
	It is sent once when the client decides to shutdown the
	server. The only notification that is sent after a shutdown request
	is the exit event.
**/
class ShutdownRequest {
	public static inline var type = new RequestType<NoData, NoData, NoData, NoData>("shutdown");
}

//---- Exit Notification ----

/**
	The exit event is sent from the client to the server to
	ask the server to exit its process.
**/
class ExitNotification {
	public static inline var type = new NotificationType<NoData, NoData>("exit");
}

//---- Configuration notification ----

typedef DidChangeConfigurationClientCapabilities = {
	/**
		Did change configuration notification supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The configuration change notification is sent from the client to the server
	when the client's configuration has changed. The notification contains
	the changed configuration as defined by the language client.
**/
class DidChangeConfigurationNotification {
	public static inline var type = new NotificationType<DidChangeConfigurationParams,
		DidChangeConfigurationRegistrationOptions>("workspace/didChangeConfiguration");
}

typedef DidChangeConfigurationRegistrationOptions = {
	var ?section:EitherType<String, Array<String>>;
}

/**
	The parameters of a change configuration notification.
**/
typedef DidChangeConfigurationParams = {
	/**
		The actual changed settings.
	**/
	var settings:Dynamic;
}

//---- Message show and log notifications ----

/**
	The message type
**/
enum abstract MessageType(Int) to Int {
	/**
		An error message.
	**/
	var Error = 1;

	/**
		A warning message.
	**/
	var Warning;

	/**
		An information message.
	**/
	var Info;

	/**
		A log message.
	**/
	var Log;
}

typedef ShowMessageParams = {
	/**
		The message type.
	**/
	var type:MessageType;

	/**
		The actual message.
	**/
	var message:String;
}

/**
	The show message notification is sent from a server to a client to ask
	the client to display a particular message in the user interface.
**/
class ShowMessageNotification {
	public static inline var type = new NotificationType<ShowMessageParams, NoData>("window/showMessage");
}

typedef MessageActionItem = {
	/**
		A short title like 'Retry', 'Open Log' etc.
	**/
	var title:String;
}

typedef ShowMessageRequestParams = {
	/**
		The message type. See {@link MessageType}
	**/
	var type:MessageType;

	/**
		The actual message.
	**/
	var message:String;

	/**
		The message action items to present.
	**/
	var ?actions:Array<MessageActionItem>;
}

/**
	The show message request is sent from the server to the client to show a message
	and a set of options actions to the user.
**/
class ShowMessageRequest {
	public static inline var type = new RequestType<ShowMessageRequestParams, Null<MessageActionItem>, NoData, NoData>("window/showMessageRequest");
}

/**
	The log message notification is sent from the server to the client to ask
	the client to log a particular message.
**/
class LogMessageNotification {
	public static inline var type = new NotificationType<LogMessageParams, NoData>("window/logMessage");
}

typedef LogMessageParams = {
	/**
		The message type.
	**/
	var type:MessageType;

	/**
		The actual message.
	**/
	var message:String;
}

//---- Telemetry notification

/**
	The telemetry event notification is sent from the server to the client to ask
	the client to log telemetry data.
**/
class TelemetryEventNotification {
	public static inline var type = new NotificationType<Dynamic, NoData>("telemetry/event");
}

//---- Text document notifications ----

typedef TextDocumentSyncClientCapabilities = {
	/**
		Whether text document synchronization supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports sending will save notifications.
	**/
	var ?willSave:Bool;

	/**
		The client supports sending a will save request and
		waits for a response providing text edits which will
		be applied to the document before it is saved.
	**/
	var ?willSaveWaitUntil:Bool;

	/**
		The client supports did save notifications.
	**/
	var ?didSave:Bool;
}

/**
	Defines how the host (editor) should sync document changes to the language server.
**/
enum abstract TextDocumentSyncKind(Int) {
	/**
		Documents should not be synced at all.
	**/
	var None;

	/**
		Documents are synced by always sending the full content of the document.
	**/
	var Full;

	/**
		Documents are synced by sending the full content on open.
		After that only incremental updates to the document are send.
	**/
	var Incremental;
}

typedef TextDocumentSyncOptions = {
	/**
		Open and close notifications are sent to the server. If omitted open close notification should not
		be sent.
	**/
	var ?openClose:Bool;

	/**
		Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
		and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.
	**/
	var ?change:TextDocumentSyncKind;

	/**
		If present will save notifications are sent to the server. If omitted the notification should not be
		sent.
	**/
	var ?willSave:Bool;

	/**
		If present will save wait until requests are sent to the server. If omitted the request should not be
		sent.
	**/
	var ?willSaveWaitUntil:Bool;

	/**
		If present save notifications are sent to the server. If omitted the notification should not be
		sent.
	**/
	var ?save:SaveOptions;
}

/**
	The parameters send in a open text document notification
**/
typedef DidOpenTextDocumentParams = {
	/**
		The document that was opened.
	**/
	var textDocument:TextDocumentItem;
}

/**
	The document open notification is sent from the client to the server to signal
	newly opened text documents. The document's truth is now managed by the client
	and the server must not try to read the document's truth using the document's
	uri. Open in this sense means it is managed by the client. It doesn't necessarily
	mean that its content is presented in an editor. An open notification must not
	be sent more than once without a corresponding close notification send before.
	This means open and close notification must be balanced and the max open count
	is one.
**/
class DidOpenTextDocumentNotification {
	public static inline var type = new NotificationType<DidOpenTextDocumentParams, TextDocumentRegistrationOptions>("textDocument/didOpen");
}

/**
	The change text document notification's parameters.
**/
typedef DidChangeTextDocumentParams = {
	/**
		The document that did change.
		The version number points to the version after all provided content changes have been applied.
	**/
	var textDocument:VersionedTextDocumentIdentifier;

	/**
		The actual content changes. The content changes describe single state changes
		to the document. So if there are two content changes c1 (at array index 0) and
		c2 (at array index 1) for a document in state S then c1 moves the document from
		S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
		on the state S'.

		To mirror the content of a document using change events use the following approach:
		- start with the same initial content
		- apply the 'textDocument/didChange' notifications in the order you recevie them.
		- apply the `TextDocumentContentChangeEvent`s in a single notification in the order
		  you receive them.
	**/
	var contentChanges:Array<TextDocumentContentChangeEvent>;
}

/**
	Describe options to be used when registered for text document change events.
**/
typedef TextDocumentChangeRegistrationOptions = TextDocumentRegistrationOptions & {
	/**
		How documents are synced to the server.
	**/
	var syncKind:TextDocumentSyncKind;
}

/**
	The document change notification is sent from the client to the server to signal
	changes to a text document.
**/
class DidChangeTextDocumentNotification {
	public static inline var type = new NotificationType<DidChangeTextDocumentParams, TextDocumentChangeRegistrationOptions>("textDocument/didChange");
}

/**
	The parameters send in a close text document notification
**/
typedef DidCloseTextDocumentParams = {
	/**
		The document that was closed.
	**/
	var textDocument:TextDocumentIdentifier;
}

/**
	The document close notification is sent from the client to the server when
	the document got closed in the client. The document's truth now exists where
	the document's uri points to (e.g. if the document's uri is a file uri the
	truth now exists on disk). As with the open notification the close notification
	is about managing the document's content. Receiving a close notification
	doesn't mean that the document was open in an editor before. A close
	notification requires a previous open notification to be sent.
**/
class DidCloseTextDocumentNotification {
	public static inline var type = new NotificationType<DidCloseTextDocumentParams, TextDocumentRegistrationOptions>("textDocument/didClose");
}

/**
	The parameters send in a save text document notification
**/
typedef DidSaveTextDocumentParams = {
	/**
		The document that was saved.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		Optional the content when saved. Depends on the `includeText` value when the save notifcation was requested.
	**/
	var ?text:String;
}

/**
	Save registration options.
**/
typedef TextDocumentSaveRegistrationOptions = TextDocumentRegistrationOptions & SaveOptions;

/**
	The document save notification is sent from the client to the server when
	the document got saved in the client.
**/
class DidSaveTextDocumentNotification {
	public static inline var type = new NotificationType<DidSaveTextDocumentParams, TextDocumentSaveRegistrationOptions>("textDocument/didSave");
}

/**
	The parameters send in a will save text document notification.
**/
typedef WillSaveTextDocumentParams = {
	/**
		The document that will be saved.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The 'TextDocumentSaveReason'.
	**/
	var reason:TextDocumentSaveReason;
}

/**
	A document will save notification is sent from the client to the server before
	the document is actually saved.
**/
class WillSaveTextDocumentNotification {
	public static inline var type = new NotificationType<WillSaveTextDocumentParams, TextDocumentRegistrationOptions>("textDocument/willSave");
}

/**
	A document will save request is sent from the client to the server before
	the document is actually saved. The request can return an array of TextEdits
	which will be applied to the text document before it is saved. Please note that
	clients might drop results if computing the text edits took too long or if a
	server constantly fails on this request. This is done to keep the save fast and
	reliable.
**/
class WillSaveTextDocumentWaitUntilRequest {
	public static inline var type = new RequestType<WillSaveTextDocumentParams, Null<Array<TextEdit>>, NoData,
		TextDocumentRegistrationOptions>("textDocument/willSaveWaitUntil");
}

//---- File eventing ----

typedef DidChangeWatchedFilesClientCapabilities = {
	/**
		Did change watched files notification supports dynamic registration. Please note
		that the current protocol doesn't support static configuration for file changes
		from the server side.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The watched files notification is sent from the client to the server when
	the client detects changes to file watched by the language client.
**/
class DidChangeWatchedFilesNotification {
	public static inline var type = new NotificationType<DidChangeWatchedFilesParams,
		DidChangeWatchedFilesRegistrationOptions>("workspace/didChangeWatchedFiles");
}

/**
	The watched files change notification's parameters.
**/
typedef DidChangeWatchedFilesParams = {
	/**
		The actual file events.
	**/
	var changes:Array<FileEvent>;
}

/**
	The file event type.
**/
enum abstract FileChangeType(Int) to Int {
	/**
		The file got created.
	**/
	var Created = 1;

	/**
		The file got changed.
	**/
	var Changed;

	/**
		The file got deleted.
	**/
	var Deleted;
}

/**
	An event describing a file change.
**/
typedef FileEvent = {
	/**
		The file's uri.
	**/
	var uri:DocumentUri;

	/**
		The change type.
	**/
	var type:FileChangeType;
}

/**
	Describe options to be used when registered for text document change events.
**/
typedef DidChangeWatchedFilesRegistrationOptions = {
	/**
		The watchers to register.
	**/
	var watchers:Array<FileSystemWatcher>;
}

typedef FileSystemWatcher = {
	/**
		The  glob pattern to watch. Glob patterns can have the following syntax:
		- `*` to match one or more characters in a path segment
		- `?` to match on one character in a path segment
		- `**` to match any number of path segments, including none
		- `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
		- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
		- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
	**/
	var globPattern:String;

	/**
		The kind of events of interest. If omitted it defaults
		to WatchKind.Create | WatchKind.Change | WatchKind.Delete
		which is 7.
	**/
	var ?kind:Int;
}

enum abstract WatchKind(Int) to Int {
	/**
		Interested in create events.
	**/
	var Create = 1;

	/**
		Interested in change events
	**/
	var Change;

	/**
		Interested in delete events
	**/
	var Delete;
}

//---- Diagnostic notification ----

/**
	The publish diagnostic client capabilities.
**/
typedef PublishDiagnosticsClientCapabilities = {
	/**
		Whether the clients accepts diagnostics with related information.
	**/
	var ?relatedInformation:Bool;

	/**
		Client supports the tag property to provide meta data about a diagnostic.
		Clients supporting tags have to handle unknown tags gracefully.

		@since 3.15.0
	**/
	var ?tagSupport:{
		/**
			The tags supported by the client.
		**/
		var valueSet:Array<DiagnosticTag>;
	};

	/**
		Whether the client interprets the version property of the
		`textDocument/publishDiagnostics` notification`s parameter.

		@since 3.15.0
	**/
	var ?versionSupport:Bool;
}

/**
	The publish diagnostic notification's parameters.
**/
typedef PublishDiagnosticsParams = {
	/**
		The URI for which diagnostic information is reported.
	**/
	var uri:DocumentUri;

	/**
		Optional the version number of the document the diagnostics are published for.

		@since 3.15.0
	**/
	var ?version:Int;

	/**
		An array of diagnostic information items.
	**/
	var diagnostics:Array<Diagnostic>;
}

/**
	Diagnostics notification are sent from the server to the client to signal
	results of validation runs.
**/
class PublishDiagnosticsNotification {
	public static inline var type = new NotificationType<PublishDiagnosticsParams, NoData>("textDocument/publishDiagnostics");
}

//---- Completion Support --------------------------

/**
	Completion client capabilities
**/
typedef CompletionClientCapabilities = {
	/**
		Whether completion supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports the following `CompletionItem` specific
		capabilities.
	**/
	var ?completionItem:{
		/**
			Client supports snippets as insert text.

			A snippet can define tab stops and placeholders with `$1`, `$2`
			and `${3:foo}`. `$0` defines the final tab stop, it defaults to
			the end of the snippet. Placeholders with equal identifiers are linked,
			that is typing in one will update others too.
		**/
		var ?snippetSupport:Bool;

		/**
			Client supports commit characters on a completion item.
		**/
		var ?commitCharactersSupport:Bool;

		/**
			Client supports the follow content formats for the documentation
			property. The order describes the preferred format of the client.
		**/
		var ?documentationFormat:Array<MarkupKind>;

		/**
			Client supports the deprecated property on a completion item.
		**/
		var ?deprecatedSupport:Bool;

		/**
			Client supports the preselect property on a completion item.
		**/
		var ?preselectSupport:Bool;

		/**
			Client supports the tag property on a completion item. Clients supporting
			tags have to handle unknown tags gracefully. Clients especially need to
			preserve unknown tags when sending a completion item back to the server in
			a resolve call.

			@since 3.15.0
		**/
		var ?tagSupport:{
			/**
				The tags supported by the client.
			**/
			var valueSet:Array<CompletionItemTag>;
		}
	};

	var ?completionItemKind:{
		/**
			The completion item kind values the client supports. When this
			property exists the client also guarantees that it will
			handle values outside its set gracefully and falls back
			to a default value when unknown.

			If this property is not present the client only supports
			the completion items kinds from `Text` to `Reference` as defined in
			the initial version of the protocol.
		**/
		var ?valueSet:Array<CompletionItemKind>;
	};

	/**
		The client supports to send additional context information for a
		`textDocument/completion` requestion.
	**/
	var ?contextSupport:Bool;
}

/**
	How a completion was triggered
**/
enum abstract CompletionTriggerKind(Int) {
	/**
		Completion was triggered by typing an identifier (24x7 code
		complete), manual invocation (e.g Ctrl+Space) or via API.
	**/
	var Invoked = 1;

	/**
		Completion was triggered by a trigger character specified by
		the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
	**/
	var TriggerCharacter;

	/**
		Completion was re-triggered as current completion list is incomplete
	**/
	var TriggerForIncompleteCompletions;
}

/**
	Contains additional information about the context in which a completion request is triggered.
**/
typedef CompletionContext = {
	/**
		How the completion was triggered.
	**/
	var triggerKind:CompletionTriggerKind;

	/**
		The trigger character (a single character) that has trigger code complete.
		Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
	**/
	var ?triggerCharacter:String;
}

/**
	Completion parameters
**/
typedef CompletionParams = TextDocumentPositionParams &
	WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The completion context. This is only available it the client specifies
		to send this using the client capability `textDocument.completion.contextSupport === true`
	**/
	var ?context:CompletionContext;
}

/**
	Completion options.
**/
typedef CompletionOptions = WorkDoneProgressOptions & {
	/**
		Most tools trigger completion request automatically without explicitly requesting
		it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
		starts to type an identifier. For example if the user types `c` in a JavaScript file
		code complete will automatically pop up present `console` besides others as a
		completion item. Characters that make up identifiers don't need to be listed here.

		If code complete should automatically be trigger on characters not being valid inside
		an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
	**/
	var ?triggerCharacters:Array<String>;

	/**
		The list of all possible characters that commit a completion. This field can be used
		if clients don't support individual commmit characters per completion item. See
		`ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`

		If a server provides both `allCommitCharacters` and commit characters on an individual
		completion item the ones on the completion item win.

		@since 3.2.0
	**/
	var ?allCommitCharacters:Array<String>;

	/**
		The server provides support to resolve additional information for a completion item.
	**/
	var ?resolveProvider:Bool;
}

/**
	Registration options for a [CompletionRequest](#CompletionRequest).
**/
typedef CompletionRegistrationOptions = TextDocumentRegistrationOptions & CompletionOptions;

/**
	Request to request completion at a given text document position. The request's
	parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response
	is of type [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)
	or a Thenable that resolves to such.

	The request can delay the computation of the [`detail`](#CompletionItem.detail)
	and [`documentation`](#CompletionItem.documentation) properties to the `completionItem/resolve`
	request. However, properties that are needed for the initial sorting and filtering, like `sortText`,
	`filterText`, `insertText`, and `textEdit`, must not be changed during resolve.
**/
class CompletionRequest {
	public static inline var type = new RequestType<CompletionParams, Null<EitherType<Array<CompletionItem>, CompletionList>>, NoData,
		CompletionRegistrationOptions>("textDocument/completion");

	public static final resultType = new ProgressType<Array<CompletionItem>>();
}

/**
	Request to resolve additional information for a given completion item.The request's
	parameter is of type [CompletionItem](#CompletionItem) the response
	is of type [CompletionItem](#CompletionItem) or a Thenable that resolves to such.
**/
class CompletionResolveRequest {
	public static inline var type = new RequestType<CompletionItem, CompletionItem, NoData, NoData>("completionItem/resolve");
}

//---- Hover Support -------------------------------

typedef HoverClientCapabilities = {
	/**
		Whether hover supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		Client supports the follow content formats for the content
		property. The order describes the preferred format of the client.
	**/
	var ?contentFormat:Array<MarkupKind>;
}

/**
	Hover options.
**/
typedef HoverOptions = WorkDoneProgressOptions;

/**
	Parameters for a [HoverRequest](#HoverRequest).
**/
typedef HoverParams = TextDocumentPositionParams & WorkDoneProgressParams;

/**
	Registration options for a [HoverRequest](#HoverRequest).
**/
typedef HoverRegistrationOptions = TextDocumentRegistrationOptions & HoverOptions;

/**
	Request to request hover information at a given text document position. The request's
	parameter is of type [TextDocumentPosition](#TextDocumentPosition) the response is of
	type [Hover](#Hover) or a Thenable that resolves to such.
**/
class HoverRequest {
	public static inline var type = new RequestType<HoverParams, Null<Hover>, NoData, HoverRegistrationOptions>("textDocument/hover");
}

//---- SignatureHelp ----------------------------------

/**
	Client Capabilities for a [SignatureHelpRequest](#SignatureHelpRequest).
**/
typedef SignatureHelpClientCapabilities = {
	/**
		Whether signature help supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports the following `SignatureInformation`
		specific properties.
	**/
	var ?signatureInformation:{
		/**
			Client supports the follow content formats for the documentation
			property. The order describes the preferred format of the client.
		**/
		var ?documentationFormat:Array<MarkupKind>;

		/**
			Client capabilities specific to parameter information.
		**/
		var ?parameterInformation:{
			/**
				The client supports processing label offsets instead of a
				simple label string.

				@since 3.14.0
			**/
			var ?labelOffsetSupport:Bool;
		}
	};

	/**
		The client supports to send additional context information for a
		`textDocument/signatureHelp` request. A client that opts into
		contextSupport will also support the `retriggerCharacters` on
		`SignatureHelpOptions`.

		@since 3.15.0
	**/
	var ?contextSupport:Bool;
}

/**
	Signature help options.
**/
typedef SignatureHelpOptions = WorkDoneProgressOptions & {
	/**
		The characters that trigger signature help automatically.
	**/
	var ?triggerCharacters:Array<String>;

	/**
		List of characters that re-trigger signature help.

		These trigger characters are only active when signature help is already showing. All trigger characters
		are also counted as re-trigger characters.

		@since 3.15.0
	**/
	var ?retriggerCharacters:Array<String>;
}

/**
	How a signature help was triggered.

	@since 3.15.0
**/
enum abstract SignatureHelpTriggerKind(Int) {
	/**
		Signature help was invoked manually by the user or by a command.
	**/
	var Invoked = 1;

	/**
		Signature help was triggered by a trigger character.
	**/
	var TriggerCharacter;

	/**
		Signature help was triggered by the cursor moving or by the document content changing.
	**/
	var ContentChange;
}

/**
	Additional information about the context in which a signature help request was triggered.

	@since 3.15.0
**/
typedef SignatureHelpContext = {
	/**
		Action that caused signature help to be triggered.
	**/
	var triggerKind:SignatureHelpTriggerKind;

	/**
		Character that caused signature help to be triggered.

		This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`
	**/
	var ?triggerCharacter:String;

	/**
		`true` if signature help was already showing when it was triggered.

		Retriggers occur when the signature help is already active and can be caused by actions such as
		typing a trigger character, a cursor move, or document content changes.
	**/
	var isRetrigger:Bool;

	/**
		The currently active `SignatureHelp`.

		The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
		the user navigating through available signatures.
	**/
	var ?activeSignatureHelp:SignatureHelp;
}

/**
	Parameters for a [SignatureHelpRequest](#SignatureHelpRequest).
**/
typedef SignatureHelpParams = TextDocumentPositionParams &
	WorkDoneProgressParams & {
	/**
		The signature help context. This is only available if the client specifies
		to send this using the client capability `textDocument.signatureHelp.contextSupport === true`

		@since 3.15.0
	**/
	var ?context:SignatureHelpContext;
}

/**
	Registration options for a [SignatureHelpRequest](#SignatureHelpRequest).
**/
typedef SignatureHelpRegistrationOptions = TextDocumentRegistrationOptions & SignatureHelpOptions;

class SignatureHelpRequest {
	public static inline var type = new RequestType<SignatureHelpParams, Null<SignatureHelp>, NoData,
		SignatureHelpRegistrationOptions>("textDocument/signatureHelp");
}

//---- Goto Definition -------------------------------------

/**
	Client Capabilities for a [DefinitionRequest](#DefinitionRequest).
**/
typedef DefinitionClientCapabilities = {
	/**
		Whether definition supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client supports additional metadata in the form of definition links.

		@since 3.14.0
	**/
	var ?linkSupport:Bool;
}

/**
	Server Capabilities for a [DefinitionRequest](#DefinitionRequest).
**/
typedef DefinitionOptions = WorkDoneProgressOptions;

/**
	Parameters for a [DefinitionParams](#DefinitionParams).
**/
typedef DefinitionParams = TextDocumentPositionParams & WorkDoneProgressParams & PartialResultParams;

/**
	Registration options for a [DefinitionRequest](#DefinitionRequest).
**/
typedef DefinitionRegistrationOptions = TextDocumentRegistrationOptions & DefinitionOptions;

/**
	A request to resolve the definition location of a symbol at a given text
	document position. The request's parameter is of type [TextDocumentPosition]
	(#TextDocumentPosition) the response is of either type [Definition](#Definition)
	or a typed array of [DefinitionLink](#DefinitionLink) or a Thenable that resolves
	to such.
**/
class DefinitionRequest {
	public static inline var type = new RequestType<DefinitionParams, Null<EitherType<Definition, DefinitionLink>>, NoData,
		DefinitionRegistrationOptions>("textDocument/definition");

	public static final resultType = new ProgressType<EitherType<Array<Location>, Array<DefinitionLink>>>();
}

//---- Reference Provider ----------------------------------

/**
	Client Capabilities for a [ReferencesRequest](#ReferencesRequest).
**/
typedef ReferenceClientCapabilities = {
	/**
		Whether references supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	Parameters for a [ReferencesRequest](#ReferencesRequest).
**/
typedef ReferenceParams = TextDocumentPositionParams &
	WorkDoneProgressParams &
	PartialResultParams & {
	var context:ReferenceContext;
}

/**
	Reference options.
**/
typedef ReferenceOptions = WorkDoneProgressOptions;

/** 
	Registration options for a [ReferencesRequest](#ReferencesRequest).
**/
typedef ReferenceRegistrationOptions = TextDocumentRegistrationOptions & ReferenceOptions;

/**
	A request to resolve project-wide references for the symbol denoted
	by the given text document position. The request's parameter is of
	type [ReferenceParams](#ReferenceParams) the response is of type
	[Location[]](#Location) or a Thenable that resolves to such.
**/
class ReferencesRequest {
	public static inline var type = new RequestType<ReferenceParams, Null<Array<Location>>, NoData, ReferenceRegistrationOptions>("textDocument/references");

	public static final resultType = new ProgressType<Array<Location>>();
}

//---- Document Highlight ----------------------------------

/**
	Client Capabilities for a [DocumentHighlightRequest](#DocumentHighlightRequest).
**/
typedef DocumentHighlightClientCapabilities = {
	/**
		Whether document highlight supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	Parameters for a [DocumentHighlightRequest](#DocumentHighlightRequest).
**/
typedef DocumentHighlightParams = TextDocumentPositionParams & WorkDoneProgressParams & PartialResultParams;

/**
	Provider options for a [DocumentHighlightRequest](#DocumentHighlightRequest).
**/
typedef DocumentHighlightOptions = WorkDoneProgressOptions;

/**
	Registration options for a [DocumentHighlightRequest](#DocumentHighlightRequest).
**/
typedef DocumentHighlightRegistrationOptions = TextDocumentRegistrationOptions & DocumentHighlightOptions;

/**
	Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given
	text document position. The request's parameter is of type [TextDocumentPosition]
	(#TextDocumentPosition) the request response is of type [DocumentHighlight[]]
	(#DocumentHighlight) or a Thenable that resolves to such.
**/
class DocumentHighlightRequest {
	public static inline var type = new RequestType<DocumentHighlightParams, Null<Array<DocumentHighlight>>, NoData,
		DocumentHighlightRegistrationOptions>("textDocument/documentHighlight");

	public static final resultType = new ProgressType<Array<DocumentHighlight>>();
}

//---- Document Symbol Provider ---------------------------

/**
	Client Capabilities for a [DocumentSymbolRequest](#DocumentSymbolRequest).
**/
typedef DocumentSymbolClientCapabilities = {
	/**
		Whether document symbol supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		Specific capabilities for the `SymbolKind`.
	**/
	var ?symbolKind:{
		/**
			The symbol kind values the client supports. When this
			property exists the client also guarantees that it will
			handle values outside its set gracefully and falls back
			to a default value when unknown.

			If this property is not present the client only supports
			the symbol kinds from `File` to `Array` as defined in
			the initial version of the protocol.
		**/
		var ?valueSet:Array<SymbolKind>;
	};

	/**
		The client support hierarchical document symbols.
	**/
	var ?hierarchicalDocumentSymbolSupport:Bool;
}

/**
	Parameters for a [DocumentSymbolRequest](#DocumentSymbolRequest).
**/
typedef DocumentSymbolParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The text document.
	**/
	var textDocument:TextDocumentIdentifier;
}

/**
	Provider options for a [DocumentSymbolRequest](#DocumentSymbolRequest).
**/
typedef DocumentSymbolOptions = WorkDoneProgressOptions;

/**
	Registration options for a [DocumentSymbolRequest](#DocumentSymbolRequest).
**/
typedef DocumentSymbolRegistrationOptions = TextDocumentRegistrationOptions & DocumentSymbolOptions;

/**
	A request to list all symbols found in a given text document. The request's
	parameter is of type [TextDocumentIdentifier](#TextDocumentIdentifier) the
	response is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
	that resolves to such.
**/
class DocumentSymbolRequest {
	public static inline var type = new RequestType<DocumentSymbolParams, Null<Array<EitherType<SymbolInformation, DocumentSymbol>>>, NoData,
		DocumentSymbolRegistrationOptions>("textDocument/documentSymbol");

	public static final resultType = new ProgressType<EitherType<Array<SymbolInformation>, Array<DocumentSymbol>>>();
}

//---- Code Action Provider ----------------------------------

/**
	The Client Capabilities of a [CodeActionRequest](#CodeActionRequest).
**/
typedef CodeActionClientCapabilities = {
	/**
		Whether code action supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		The client support code action literals as a valid
		response of the `textDocument/codeAction` request.

		@since 3.8.0
	**/
	var ?codeActionLiteralSupport:{
		/**
			The code action kind is support with the following value
			set.
		**/
		var codeActionKind:{
			/**
				The code action kind values the client supports. When this
				property exists the client also guarantees that it will
				handle values outside its set gracefully and falls back
				to a default value when unknown.
			**/
			var valueSet:Array<CodeActionKind>;
		};
	};

	/**
		Whether code action supports the `isPreferred` property.
		@since 3.15.0
	**/
	var ?isPreferredSupport:Bool;
}

/**
	The parameters of a [CodeActionRequest](#CodeActionRequest).
**/
typedef CodeActionParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		The document in which the command was invoked.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The range for which the command was invoked.
	**/
	var range:Range;

	/**
		Context carrying additional information.
	**/
	var context:CodeActionContext;
}

/**
	Provider options for a [CodeActionRequest](#CodeActionRequest).
**/
typedef CodeActionOptions = WorkDoneProgressOptions & {
	/**
		CodeActionKinds that this server may return.

		The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
		may list out every specific kind they provide.
	**/
	var ?codeActionKinds:Array<CodeActionKind>;
}

/**
	Registration options for a [CodeActionRequest](#CodeActionRequest).
**/
typedef CodeActionRegistrationOptions = TextDocumentRegistrationOptions & CodeActionOptions;

/**
	A request to provide commands for the given text document and range.
**/
class CodeActionRequest {
	public static inline var type = new RequestType<CodeActionParams, Null<Array<EitherType<Command, CodeAction>>>, NoData,
		CodeActionRegistrationOptions>("textDocument/codeAction");

	public static final resultType = new ProgressType<Array<EitherType<Command, CodeAction>>>();
}

//---- Workspace Symbol Provider ---------------------------

/**
	Client capabilities for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
**/
typedef WorkspaceSymbolClientCapabilities = {
	/**
		Symbol request supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
	**/
	var ?symbolKind:{
		/**
			The symbol kind values the client supports. When this
			property exists the client also guarantees that it will
			handle values outside its set gracefully and falls back
			to a default value when unknown.

			If this property is not present the client only supports
			the symbol kinds from `File` to `Array` as defined in
			the initial version of the protocol.
		**/
		var ?valueSet:Array<SymbolKind>;
	};
}

/**
	The parameters of a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
**/
typedef WorkspaceSymbolParams = WorkDoneProgressParams &
	PartialResultParams & {
	/**
		A query string to filter symbols by. Clients may send an empty
		string here to request all symbols.
	**/
	var query:String;
}

/**
	Server capabilities for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
**/
typedef WorkspaceSymbolOptions = WorkDoneProgressOptions;

/**
	Registration options for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
**/
typedef WorkspaceSymbolRegistrationOptions = WorkspaceSymbolOptions;

/**
	A request to list project-wide symbols matching the query string given
	by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is
	of type [SymbolInformation[]](#SymbolInformation) or a Thenable that
	resolves to such.
**/
class WorkspaceSymbolRequest {
	public static inline var type = new RequestType<WorkspaceSymbolParams, Null<Array<SymbolInformation>>, NoData,
		WorkspaceSymbolRegistrationOptions>("workspace/symbol");

	public static final resultType = new ProgressType<Array<SymbolInformation>>();
}

//---- Code Lens Provider -------------------------------------------

/**
	The client capabilities  of a [CodeLensRequest](#CodeLensRequest).
**/
typedef CodeLensClientCapabilities = {
	/**
		Whether code lens supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The parameters of a [CodeLensRequest](#CodeLensRequest).
**/
typedef CodeLensParams = {
	/**
		The document to request code lens for.
	**/
	var textDocument:TextDocumentIdentifier;
}

/**
	Code Lens provider options of a [CodeLensRequest](#CodeLensRequest).
**/
typedef CodeLensOptions = WorkDoneProgressOptions & {
	/**
		Code lens has a resolve provider as well.
	**/
	var ?resolveProvider:Bool;
}

/**
	Registration options for a [CodeLensRequest](#CodeLensRequest).
**/
typedef CodeLensRegistrationOptions = TextDocumentRegistrationOptions & CodeLensOptions;

/**
	A request to provide code lens for the given text document.
**/
class CodeLensRequest {
	public static inline var type = new RequestType<CodeLensParams, Array<CodeLens>, NoData, CodeLensRegistrationOptions>("textDocument/codeLens");

	public static final resultType = new ProgressType<Array<CodeLens>>();
}

/**
	A request to resolve a command for a given code lens.
**/
class CodeLensResolveRequest {
	public static inline var type = new RequestType<CodeLens, CodeLens, NoData, NoData>("codeLens/resolve");
}

//---- Document Links ----------------------------------------------

/**
	The client capabilities of a [DocumentLinkRequest](#DocumentLinkRequest).
**/
typedef DocumentLinkClientCapabilities = {
	/**
		Whether document link supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		Whether the client support the `tooltip` property on `DocumentLink`.

		@since 3.15.0
	**/
	var ?tooltipSupport:Bool;
}

/**
	The parameters of a [DocumentLinkRequest](#DocumentLinkRequest).
**/
typedef DocumentLinkParams = {
	/**
		The document to provide document links for.
	**/
	var textDocument:TextDocumentIdentifier;
}

/**
	Provider options for a [DocumentLinkRequest](#DocumentLinkRequest).
**/
typedef DocumentLinkOptions = WorkDoneProgressOptions & {
	/**
		Document links have a resolve provider as well.
	**/
	var ?resolveProvider:Bool;
}

/**
	Registration options for a [DocumentLinkRequest](#DocumentLinkRequest).
**/
typedef DocumentLinkRegistrationOptions = TextDocumentRegistrationOptions & DocumentLinkOptions;

/**
	A request to provide document links
**/
class DocumentLinkRequest {
	public static inline var type = new RequestType<DocumentLinkParams, Null<Array<DocumentLink>>, NoData,
		DocumentLinkRegistrationOptions>("textDocument/documentLink");

	public static final resultType = new ProgressType<Array<DocumentLink>>();
}

/**
	Request to resolve additional information for a given document link. The request's
	parameter is of type [DocumentLink](#DocumentLink) the response
	is of type [DocumentLink](#DocumentLink) or a Thenable that resolves to such.
**/
class DocumentLinkResolveRequest {
	public static inline var type = new RequestType<DocumentLink, DocumentLink, NoData, NoData>("documentLink/resolve");
}

//---- Formatting ----------------------------------------------

/**
	Client capabilities of a [DocumentFormattingRequest](#DocumentFormattingRequest).
**/
typedef DocumentFormattingClientCapabilities = {
	/**
		Whether formatting supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The parameters of a [DocumentFormattingRequest](#DocumentFormattingRequest).
**/
typedef DocumentFormattingParams = {
	/**
		The document to format.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The format options.
	**/
	var options:FormattingOptions;
}

/**
	Provider options for a [DocumentFormattingRequest](#DocumentFormattingRequest).
**/
typedef DocumentFormattingOptions = WorkDoneProgressOptions;

/**
	Registration options for a [DocumentFormattingRequest](#DocumentFormattingRequest).
**/
typedef DocumentFormattingRegistrationOptions = TextDocumentRegistrationOptions & DocumentFormattingOptions;

/**
	A request to to format a whole document.
**/
class DocumentFormattingRequest {
	public static inline var type = new RequestType<DocumentFormattingParams, Null<Array<TextEdit>>, NoData,
		DocumentFormattingRegistrationOptions>("textDocument/formatting");
}

/**
	Client capabilities of a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).
**/
typedef DocumentRangeFormattingClientCapabilities = {
	/**
		Whether range formatting supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The parameters of a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).
**/
typedef DocumentRangeFormattingParams = {
	/**
		The document to format.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The range to format.
	**/
	var range:Range;

	/**
		The format options.
	**/
	var options:FormattingOptions;
}

/**
	Provider options for a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).
**/
typedef DocumentRangeFormattingOptions = WorkDoneProgressOptions;

/**
	Registration options for a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).
**/
typedef DocumentRangeFormattingRegistrationOptions = TextDocumentRegistrationOptions & DocumentRangeFormattingOptions;

/**
	A request to to format a range in a document.
**/
class DocumentRangeFormattingRequest {
	public static inline var type = new RequestType<DocumentRangeFormattingParams, Null<Array<TextEdit>>, NoData,
		DocumentRangeFormattingRegistrationOptions>("textDocument/rangeFormatting");
}

/**
	Client capabilities of a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).
**/
typedef DocumentOnTypeFormattingClientCapabilities = {
	/**
		Whether on type formatting supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The parameters of a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).
**/
typedef DocumentOnTypeFormattingParams = {
	/**
		The document to format.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The position at which this request was send.
	**/
	var position:Position;

	/**
		The character that has been typed.
	**/
	var ch:String;

	/**
		The format options.
	**/
	var options:FormattingOptions;
}

/**
	Provider options for a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).
**/
typedef DocumentOnTypeFormattingOptions = {
	/**
		A character on which formatting should be triggered, like `}`.
	**/
	var firstTriggerCharacter:String;

	/**
		More trigger characters.
	**/
	var ?moreTriggerCharacter:Array<String>;
}

/**
	Registration options for a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).
**/
typedef DocumentOnTypeFormattingRegistrationOptions = TextDocumentRegistrationOptions & DocumentOnTypeFormattingOptions;

/**
	A request to format a document on type.
**/
class DocumentOnTypeFormattingRequest {
	public static inline var type = new RequestType<DocumentOnTypeFormattingParams, Null<Array<TextEdit>>, NoData,
		DocumentOnTypeFormattingRegistrationOptions>("textDocument/onTypeFormatting");
}

//---- Rename ----------------------------------------------

typedef RenameClientCapabilities = {
	/**
		Whether rename supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;

	/**
		Client supports testing for validity of rename operations
		before execution.

		@since version 3.12.0
	**/
	var ?prepareSupport:Bool;
}

/**
	The parameters of a [RenameRequest](#RenameRequest).
**/
typedef RenameParams = {
	/**
		The document to format.
	**/
	var textDocument:TextDocumentIdentifier;

	/**
		The position at which this request was send.
	**/
	var position:Position;

	/**
		The new name of the symbol.
		If the given name is not valid the request must return a `ResponseError` with an appropriate message set.
	**/
	var newName:String;
}

/**
	Provider options for a [RenameRequest](#RenameRequest).
**/
typedef RenameOptions = WorkDoneProgressOptions & {
	/**
		Renames should be checked and tested before being executed.

		@since version 3.12.0
	**/
	var ?prepareProvider:Bool;
}

/**
	Registration options for a [RenameRequest](#RenameRequest).
**/
typedef RenameRegistrationOptions = TextDocumentRegistrationOptions & RenameOptions;

/**
	A request to rename a symbol.
**/
class RenameRequest {
	public static inline var type = new RequestType<RenameParams, Null<WorkspaceEdit>, NoData, RenameRegistrationOptions>("textDocument/rename");
}

typedef PrepareRenameParams = TextDocumentPositionParams & WorkDoneProgressParams;

/**
	A request to test and perform the setup necessary for a rename.
**/
class PrepareRenameRequest {
	public static inline var type = new RequestType<PrepareRenameParams, Null<EitherType<Range, {range:Range, placeholder:String}>>, NoData,
		NoData>("textDocument/prepareRename");
}

//---- Command Execution -------------------------------------------

/**
	The client capabilities of a [ExecuteCommandRequest](#ExecuteCommandRequest).
**/
typedef ExecuteCommandClientCapabilities = {
	/**
		Execute command supports dynamic registration.
	**/
	var ?dynamicRegistration:Bool;
}

/**
	The parameters of a [ExecuteCommandRequest](#ExecuteCommandRequest).
**/
typedef ExecuteCommandParams = {
	/**
		The identifier of the actual command handler.
	**/
	var command:String;

	/**
		Arguments that the command should be invoked with.
	**/
	var ?arguments:Array<Dynamic>;
}

/**
	The server capabilities of a [ExecuteCommandRequest](#ExecuteCommandRequest).
**/
typedef ExecuteCommandOptions = WorkDoneProgressOptions & {
	/**
		The commands to be executed on the server
	**/
	var commands:Array<String>;
}

/**
	Registration options for a [ExecuteCommandRequest](#ExecuteCommandRequest).
**/
typedef ExecuteCommandRegistrationOptions = ExecuteCommandOptions;

/**
	A request send from the client to the server to execute a command. The request might return
	a workspace edit which the client will apply to the workspace.
**/
class ExecuteCommandRequest {
	public static inline var type = new RequestType<ExecuteCommandParams, Null<Dynamic>, NoData,
		ExecuteCommandRegistrationOptions>("workspace/executeCommand");
}

//---- Apply Edit request ----------------------------------------

typedef WorkspaceEditClientCapabilities = {
	/**
		The client supports versioned document changes in `WorkspaceEdit`s
	**/
	var ?documentChanges:Bool;

	/**
		The resource operations the client supports. Clients should at least
		support 'create', 'rename' and 'delete' files and folders.

		@since 3.13.0
	**/
	var ?resourceOperations:Array<ResourceOperationKind>;

	/**
		The failure handling strategy of a client if applying the workspace edit
		failes.

		@since 3.13.0
	**/
	var ?failureHandling:FailureHandlingKind;
}

/**
	The parameters passed via a apply workspace edit request.
**/
typedef ApplyWorkspaceEditParams = {
	/**
		The edits to apply.
	**/
	var edit:WorkspaceEdit;
}

/**
	A response returned from the apply workspace edit request.
**/
typedef ApplyWorkspaceEditResponse = {
	/**
		Indicates whether the edit was applied or not.
	**/
	var applied:Bool;

	/**
		An optional textual description for why the edit was not applied.
		This may be used by the server for diagnostic logging or to provide
		a suitable error for a request that triggered the edit.
	**/
	var ?failureReason:String;

	/**
		Depending on the client's failure handling strategy `failedChange` might
		contain the index of the change that failed. This property is only available
		if the client signals a `failureHandlingStrategy` in its client capabilities.
	**/
	var ?failedChange:Int;
}

/**
	A request sent from the server to the client to modified certain resources.
**/
class ApplyWorkspaceEditRequest {
	public static inline var type = new RequestType<ApplyWorkspaceEditParams, ApplyWorkspaceEditResponse, NoData, NoData>("workspace/applyEdit");
}
