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
package haxe;

class JsonHelper<T> {
    
    private var _data:String;
    private var _cls:Class<T>;
    
	/**
		Creates a new JsonHelper with the given class type and
		the data.
	**/
    public function new(cls:Class<T>, data:String)
    {
        _data = data;
        _cls = cls;
	}
	
	/**
		Decodes the data into the class type. Only works on fields.
		
		Throws an error if the data is not valid JSON.
	**/
	public function decode():T
	{
        var casted = Type.createEmptyInstance(_cls);
        var obj = Json.parse(_data);
        var fields = Reflect.fields(obj);
        for (item in fields)
        {
			trySetField(casted, item, Reflect.field(obj, item));
        }
        return casted;
	}
    
    private function trySetField(obj:T, field:String, value:Dynamic):Void
    {
        try
        {
            Reflect.setField(obj, field, value);
		}
        catch (msg:String)
        {
            trace(msg);
		}
	}
}