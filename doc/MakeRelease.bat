@echo off
rm -rf haxe-release
mkdir haxe-release
cp ../haxe.exe ../haxesetup.exe CHANGES.txt LICENSE.txt haxeserver.bat haxe-release
cp -R ../std haxe-release

cd haxe-release\std
rm -rf CVS .cvsignore */CVS */.cvsignore */*/CVS */*/.cvsignore */*/*/CVS */*/*/.cvsignore
rm -rf all.n all.js all.swf *.xml tools/docview.n

cd ..\..\..\..\neko\bin
cp neko.dll neko.exe nekoc.exe nekotools.exe *.ndll ../../haxe/doc/haxe-release

echo The magic script is done, it's release time !
pause
