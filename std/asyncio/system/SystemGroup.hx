package asyncio.system;

import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;

/**
	Represents a users group registered in OS.

	TODO:
	Not sure what would be the best underlying type for cross-platform, hence `@:coreType`
**/
@:coreType abstract SystemGroup {
	@:from static function fromGroupId(groupId:Int):SystemGroup {
		throw new NotImplemented();
	}

	@:from static function fromGroupName(groupName:String):SystemGroup {
		throw new NotImplemented();
	}

	/**
		Create a new system group.

		TODO: not sure if we need this in std.
	**/
	function create(name:String, callback:Callback<NoData>):Void {
		callback(new NotImplemented(), NoData);
	}

	/**
		Add `user` to this group.

		TODO: not sure if we need this in std.
	**/
	function addUser(user:SystemUser, callback:Callback<Null<SystemGroup>>):Void {
		callback(new NotImplemented(), null);
	}
}