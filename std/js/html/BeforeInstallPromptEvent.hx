package js.html;

import js.lib.Promise;

/**
	The `BeforeInstallPromptEvent` is fired at the `Window.onbeforeinstallprompt` handler,
	before a user is prompted to install a web site to a home screen.

	@see https://developer.mozilla.org/en-US/docs/Web/API/BeforeInstallPromptEvent
**/
@:native("BeforeInstallPromptEvent")
extern class BeforeInstallPromptEvent extends Event {

	/** The platforms on which this event was dispatched. **/
	final platforms: Array<String>;

	/** The user's choice to the install prompt. **/
	final userChoice: Promise<BeforeInstallPromptUserChoice>;

	/** Creates a new `BeforeInstallPromptEvent`. **/
	function new();

	/** Shows the install prompt. **/
	function prompt(): Promise<Dynamic>;
}

typedef BeforeInstallPromptUserChoice = {
	final outcome: BeforeInstallPromptUserChoiceOutcome;
}

enum abstract BeforeInstallPromptUserChoiceOutcome(String) {
	var Accepted = "accepted";
	var Dismissed = "dismissed";
}
