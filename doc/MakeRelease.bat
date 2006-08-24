@echo off
rm -rf haxe-release
mkdir haxe-release
mkdir haxe-release\doc

cp ../haxe.exe ../haxesetup.exe CHANGES.txt LICENSE.txt haxeserver.bat haxe-release
cp -R ../std haxe-release

cd haxe-release\std

haxe all.hxml
cd tools

rem ---------- BUILD TOOLS -----------

cd haxedoc
haxe haxedoc.hxml
haxedoc ../../flash.xml ../../neko.xml ../../js.xml
mv index.html content ../../../doc
mv haxedoc.exe ../../..
cd ..

cd haxelib
haxe haxelib.hxml
mv haxelib.exe ../../..
cd ..

rem ---------- DONE -----------

cd ..

rm -rf CVS .cvsignore */CVS */.cvsignore */*/CVS */*/.cvsignore */*/*/CVS */*/*/.cvsignore
rm -rf all.n all.js *.swf *.xml 
rm -rf tools/haxedoc/haxedoc.n tools/haxedoc/index.html tools/haxedoc/content tools/haxedoc/haxedoc.exe
rm -rf mt mtwin

cd ..\..\..\..\neko\bin
cp gc.dll neko.dll neko.exe nekoc.exe nekotools.exe *.ndll ../../haxe/doc/haxe-release

echo The magic script is done, it's release time !
pause
