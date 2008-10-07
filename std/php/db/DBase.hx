/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package php.db;

private class DBaseConnection {

	var c : Void;
	var fields : Array<Field>;

	public function new( file : String, mode : Int) {
		c = untyped __call__("dbase_open", file, mode);
		if(c == null) throw "Invalid dBase file: " + file;
		var infos : ArrayAccess<Dynamic> = untyped __call__("dbase_get_header_info", c);
		fields = [];
		for(info in infos) {
			fields.push({
				name : info['name'],
				type : getType(info)
			});
		}
	}

	public function close() {
		untyped __call__("dbase_close", c);
		untyped __call__("unset", c);
	}

	public function count() {
		return untyped __call__("dbase_numrecords", c);
	}

	public function insert(values : Array<Dynamic>) : Bool {
		return untyped __call__("dbase_add_record", c, values);
	}

	public function replace(index : Int, values : Array<Dynamic>) {
		return untyped __call__("dbase_replace_record", c, values, index);
	}

	public function delete(index : Int) : Bool {
		return untyped __call__("dbase_delete_record", c, index);
	}

	public function records() : Array<Dynamic> {
		var arr = [];
		for(i in 1...count()+1)
			arr.push(record(i));
		return arr;
	}

	public function rows() : Array<Array<Dynamic>> {
		var arr = [];
		for(i in 1...count()+1)
			arr.push(row(i));
		return arr;
	}

	public function row(index : Int) : Array<Dynamic> {
		var r = untyped __call__("dbase_get_record", c, index);
		if(untyped __php__("isset($r['deleted'])")) {
			untyped __php__("unset($r['deleted'])");
		}
		return untyped __call__("new _hx_array", r);
	}

	public function record(index : Int) : Dynamic {
		var row = row(index);
		var record = {};
		for(j in 0...fields.length)
			Reflect.setField(record, fields[j].name, row[j]);
		return record;
	}

	private function getType(info : Array<Dynamic>) {
		switch(info[untyped 'type']) {
			case 'D': return DateField;
			case 'N': return NumberField(info[untyped 'length'], info[untyped 'precision']);
			case 'C': return StringField(info[untyped 'length']);
			case 'L': return BooleanField;
			case 'F': return FloatField;
		}
		return null;
	}
}

class DBase {

	public static function open( file : String ) : DBaseConnection {
		return new DBaseConnection(file, 2);
	}

	public static function openReadOnly( file : String ) : DBaseConnection {
		return new DBaseConnection(file, 0);
	}

	public static function create(file : String, fields : Array<Field>) : Void {
		var flds : Array<Array<Dynamic>> = [];
		for(field in fields) {
			var fld : Array<Dynamic>= [];
			fld.push(field.name);
			switch(field.type) {
				case DateField:
					fld.push('D');
				case NumberField(len, precision):
					fld.push('N');
					fld.push(len);
					fld.push(precision);
				case StringField(len):
					fld.push('C');
					fld.push(len);
				case BooleanField:
					fld.push('L');
				case FloatField:
					fld.push('F');
			}
			flds.push(fld);
		}
		var h = untyped __call__("dbase_create", file, flds);
		if(h == false) throw "Error creating: " + file;
		untyped __call__("dbase_close", h);
	}
}

typedef Field = {
	name : String, // max 10 chars
	type : FieldType
}

enum FieldType {
//	MemoField;
	DateField;
	NumberField(len : Int, precision : Int);
	StringField(len : Int);
	BooleanField;
	FloatField;
}
