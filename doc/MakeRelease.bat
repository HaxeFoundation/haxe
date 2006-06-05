@echo off
rm -rf haxe-release
mkdir haxe-release
mkdir haxe-release\doc
cp ../haxe.exe ../haxesetup.exe CHANGES.txt LICENSE.txt haxeserver.bat haxe-release
cp -R ../std haxe-release

cd haxe-release\std

haxe all.hxml
cd tools
haxe docview.hxml
neko docview ../flash.xml ../neko.xml ../js.xml
mv index.html content ../../doc
cd ..

rm -rf CVS .cvsignore */CVS */.cvsignore */*/CVS */*/.cvsignore */*/*/CVS */*/*/.cvsignore
rm -rf all.n all.js all.swf *.xml tools/docview.n tools/index.html tools/content

cd ..\..\..\..\neko\bin
cp gc.dll neko.dll neko.exe nekoc.exe nekotools.exe *.ndll ../../haxe/doc/haxe-release

echo The magic script is done, it's release time !
pause
