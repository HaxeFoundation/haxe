package asyncio.system;

import haxe.Callback;
import haxe.errors.NotImplemented;

/**
	Represents a user registered in OS.

	TODO:
	Not sure what would be the best underlying type for cross-platform, hence `@:coreType`
**/
@:coreType abstract SystemUser {
	@:from static function fromUserId(userId:Int):SystemUser {
		throw new NotImplemented();
	}

	@:from static function fromUserName(userName:String):SystemUser {
		throw new NotImplemented();
	}

	/**
		Create a new system user.

		TODO: not sure if we need this in std.
	**/
	function create(name:String, callback:Callback<Null<SystemUser>>):Void {
		callback(new NotImplemented(), null);
	}
}