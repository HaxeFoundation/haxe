package languageServerProtocol.protocol;

import languageServerProtocol.Types.DocumentUri;
import languageServerProtocol.protocol.Protocol;

//---- Get Configuration request ----

typedef ConfigurationClientCapabilities = {
	/**
		The client supports `workspace/configuration` requests.
	**/
	var ?configuration:Bool;
}

/**
	The 'workspace/configuration' request is sent from the server to the client to fetch a certain
	configuration setting.

	This pull model replaces the old push model were the client signaled configuration change via an
	event. If the server still needs to react to configuration changes (since the server caches the
	result of `workspace/configuration` requests) the server should register for an empty configuration
	change event and empty the cache if such an event is received.
**/
class ConfigurationRequest {
	public static inline var type = new ProtocolRequestType<ConfigurationParams & PartialResultParams, Array<Any>, Never, NoData,
		NoData>("workspace/configuration");
}

typedef ConfigurationItem = {
	/**
		The scope to get the configuration section for.
	**/
	var ?scopeUri:DocumentUri;

	/**
		The configuration section asked for.
	**/
	var ?section:String;
}

/**
	The parameters of a configuration request.
**/
typedef ConfigurationParams = {
	var items:Array<ConfigurationItem>;
}
