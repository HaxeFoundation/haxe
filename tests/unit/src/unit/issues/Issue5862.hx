package unit.issues;
import haxe.ds.*;
#if java
import java.NativeArray;
#elseif cs
import cs.NativeArray;
#end

class Issue5862 extends Test {
#if (java || cs)
  public function test() {
    var imap = new IntMap();
    imap.set(0, "val1");
    imap.set(1, "val2");
    imap.set(2, "val3");
    imap.set(2, "changed_val3");

    var v:Vector<Dynamic> = cast @:privateAccess imap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val3");
    }

    var smap = new StringMap();
    smap.set("v1", "val1");
    smap.set("v2", "val2");
    smap.set("v3", "val3");
    smap.set("v3", "changed_val3");

	#if !jvm
    var v:Vector<Dynamic> = cast @:privateAccess smap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val3");
	}
	#end

    var omap = new ObjectMap<{}, String>();
    omap.set(imap, "val1");
    omap.set(smap, "val2");
    omap.set(omap, "val3");
    omap.set(omap, "changed_val3");

    var v:Vector<Dynamic> = cast @:privateAccess omap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val3");
    }
#if java
    var wmap = new WeakMap<{}, String>();
    wmap.set(imap, "val1");
    wmap.set(smap, "val2");
    wmap.set(omap, "val3");
    wmap.set(omap, "changed_val3");

    var v = @:privateAccess wmap.entries;
    for (i in 0...v.length) {
      t(v[i] == null || v[i].value != "val3");
    }
#end

    var imap = new IntMap();
    imap.set(0, "val1");
    imap.set(1, "val2");
    imap.set(2, "val3");
    imap.set(2, "changed_val3");
    imap.set(1, "changed_val2");

    var v:Vector<Dynamic> = cast @:privateAccess imap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val2");
    }

    var smap = new StringMap();
    smap.set("v1", "val1");
    smap.set("v2", "val2");
    smap.set("v3", "val3");
    smap.set("v3", "changed_val3");
    smap.set("v2", "changed_val2");

	#if !jvm
    var v:Vector<Dynamic> = cast @:privateAccess smap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val2");
	}
	#end

    var omap = new ObjectMap<{}, String>();
    omap.set(imap, "val1");
    omap.set(smap, "val2");
    omap.set(omap, "val3");
    omap.set(omap, "changed_val3");
    omap.set(smap, "changed_val2");

    var v:Vector<Dynamic> = cast @:privateAccess omap.vals;
    for (i in 0...v.length) {
      t(v[i] != "val2");
    }
#if java
    var wmap = new WeakMap<{}, String>();
    wmap.set(imap, "val1");
    wmap.set(smap, "val2");
    wmap.set(omap, "val3");
    wmap.set(omap, "changed_val3");
    wmap.set(smap, "changed_val2");

    var v = @:privateAccess wmap.entries;
    for (i in 0...v.length) {
      t(v[i] == null || v[i].value != "val2");
    }
#end
  }
#end
}
