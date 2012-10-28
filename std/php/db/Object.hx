/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package php.db;

/**
	SPOD Object : the persistent object base type. See the tutorial on haXe
	website to learn how to use SPOD.
**/
class Object #if spod_rtti implements haxe.rtti.Infos #end {

/*
	(optional)
	static var TABLE_NAME = "TableName";
	static var TABLE_IDS = ["id"];
	static var PRIVATE_FIELDS = ["my_priv_field"];
	static function RELATIONS() {
		return [{ key : "uid", prop : "user", manager : User.manager }];
	}

	static var manager = new php.db.Manager();
*/

	var __cache__ : Object;
	var __noupdate__ : Bool;
	var __manager__ : {
		private function doUpdate( o : Object ) : Void;
		private function doInsert( o : Object ) : Void;
		private function doSync( o : Object ) : Void;
		private function doDelete( o : Object ) : Void;
		private function objectToString( o : Object ) : String;
	};

	public function new() {
		__init_object();
	}

	private function __init_object() {
		__noupdate__ = false;
		__manager__ = Manager.managers.get(Type.getClassName(Type.getClass(this)));
		var rl : Array<Dynamic>;
		try {
			rl = untyped __manager__.cls.RELATIONS();
		} catch(e : Dynamic) { return; }
		for(r in rl)
			untyped __manager__.initRelation(this, r);
	}

	public function insert() {
		__manager__.doInsert(this);
	}

	public function update() {
		if( __noupdate__ ) throw "Cannot update not locked object";
		__manager__.doUpdate(this);
	}

	public function sync() {
		__manager__.doSync(this);
	}

	public function delete() {
		__manager__.doDelete(this);
	}

	public function toString() {
		return __manager__.objectToString(this);
	}

}
