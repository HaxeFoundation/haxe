package languageServerProtocol.protocol;

import languageServerProtocol.Types.DocumentUri;
import languageServerProtocol.protocol.Protocol;

typedef WorkspaceFoldersInitializeParams = {
	/**
		The actual configured workspace folders.
	**/
	var ?workspaceFolders:Array<WorkspaceFolder>;
}

typedef WorkspaceFoldersClientCapabilities = {
	/**
		The client has support for workspace folders
	**/
	var ?workspaceFolders:Bool;
}

typedef WorkspaceFoldersServerCapabilities = {
	var ?workspaceFolders:{
		/**
			The Server has support for workspace folders
		**/
		var ?supported:Bool;

		/**
			Whether the server wants to receive workspace folder
			change notifications.

			If a strings is provided the string is treated as a ID
			under which the notification is registed on the client
			side. The ID can be used to unregister for these events
			using the `client/unregisterCapability` request.
		**/
		var ?changeNotifications:EitherType<String, Bool>;
	};
}

typedef WorkspaceFolder = {
	/**
		The associated URI for this workspace folder.
	**/
	var uri:DocumentUri;

	/**
		The name of the workspace folder. Used to refer to this
		workspace folder in thge user interface.
	**/
	var name:String;
}

/**
	The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders.
**/
class WorkspaceFoldersRequest {
	public static inline var type = new RequestType<NoData, Null<Array<WorkspaceFolder>>, NoData, NoData>("workspace/workspaceFolders");
}

/**
	The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace
	folder configuration changes.
**/
class DidChangeWorkspaceFoldersNotification {
	public static inline var type = new NotificationType<DidChangeWorkspaceFoldersParams, NoData>("workspace/didChangeWorkspaceFolders");
}

/**
	The parameters of a `workspace/didChangeWorkspaceFolders` notification.
**/
typedef DidChangeWorkspaceFoldersParams = {
	/**
		The actual workspace folder change event.
	**/
	var event:WorkspaceFoldersChangeEvent;
}

/**
	The workspace folder change event.
**/
typedef WorkspaceFoldersChangeEvent = {
	/**
		The array of added workspace folders
	**/
	var added:Array<WorkspaceFolder>;

	/**
		The array of the removed workspace folders
	**/
	var removed:Array<WorkspaceFolder>;
}
