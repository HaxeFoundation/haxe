package languageServerProtocol.protocol;

import languageServerProtocol.protocol.Protocol;

typedef WorkDoneProgressClientCapabilities = {
	/**
		Whether client supports handling progress notifications. If set servers are allowed to
		report in `workDoneProgress` property in the request specific server capabilities.

		Since 3.15.0
	**/
	var ?workDoneProgress:Bool;
}

enum abstract WorkDoneProgressBeginKind(String) {
	var Begin = 'begin';
}

typedef WorkDoneProgressBegin = {
	var kind:WorkDoneProgressBeginKind;

	/**
		Mandatory title of the progress operation. Used to briefly inform about
		the kind of operation being performed.

		Examples: "Indexing" or "Linking dependencies".
	**/
	var title:String;

	/**
		Controls if a cancel button should show to allow the user to cancel the
		long running operation. Clients that don't support cancellation are allowed
		to ignore the setting.
	**/
	var ?cancellable:Bool;

	/**
		Optional, more detailed associated progress message. Contains
		complementary information to the `title`.

		Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
		If unset, the previous progress message (if any) is still valid.
	**/
	var ?message:String;

	/**
		Optional progress percentage to display (value 100 is considered 100%).
		If not provided infinite progress is assumed and clients are allowed
		to ignore the `percentage` value in subsequent in report notifications.

		The value should be steadily rising. Clients are free to ignore values
		that are not following this rule.
	**/
	var ?percentage:Float;
}

enum abstract WorkDoneProgressReportKind(String) {
	var Report = 'report';
}

typedef WorkDoneProgressReport = {
	var kind:WorkDoneProgressReportKind;

	/**
		Controls enablement state of a cancel button. This property is only valid if a cancel
		button got requested in the `WorkDoneProgressStart` payload.

		Clients that don't support cancellation or don't support control the button's
		enablement state are allowed to ignore the setting.
	**/
	var ?cancellable:Bool;

	/**
		Optional, more detailed associated progress message. Contains
		complementary information to the `title`.

		Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
		If unset, the previous progress message (if any) is still valid.
	**/
	var ?message:String;

	/**
		Optional progress percentage to display (value 100 is considered 100%).
		If not provided infinite progress is assumed and clients are allowed
		to ignore the `percentage` value in subsequent in report notifications.

		The value should be steadily rising. Clients are free to ignore values
		that are not following this rule.
	**/
	var ?percentage:Float;
}

enum abstract WorkDoneProgressEndKind(String) {
	var End = 'end';
}

typedef WorkDoneProgressEnd = {
	var kind:WorkDoneProgressEndKind;

	/**
		Optional, a final message indicating to for example indicate the outcome
		of the operation.
	**/
	var ?message:String;
}

class WorkDoneProgress {
	public final type = new ProgressType<EitherType<WorkDoneProgressBegin, EitherType<WorkDoneProgressReport, WorkDoneProgressEnd>>>();
}

typedef WorkDoneProgressCreateParams = {
	/**
		The token to be used to report progress.
	**/
	var token:ProgressToken;
}

/**
	The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress
	reporting from the server.
**/
class WorkDoneProgressCreateRequest {
	public static inline var type = new ProtocolRequestType<WorkDoneProgressCreateParams, NoData, Never, NoData, NoData>("window/workDoneProgress/create");
}

typedef WorkDoneProgressCancelParams = {
	/**
		The token to be used to report progress.
	**/
	var token:ProgressToken;
}

/**
	The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress
	initiated on the server side.
**/
class WorkDoneProgressCancelNotification {
	public static inline var type = new ProtocolNotificationType<WorkDoneProgressCancelParams, NoData>("window/workDoneProgress/cancel");
}
