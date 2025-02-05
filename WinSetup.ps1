function Cmd-Path($file) {
	try { Split-Path -Parent (Get-Command "$file.exe" -ErrorAction Stop).Source } catch { "" }
}

# resolve Opam binary and repo
# you can choose when opam is installed by setting OPAM_INSTALL_DIR (and OPAMROOT - optional)

$Opam = Cmd-Path "opam"
$OpamRepo = $env:OPAMROOT
$Git = Cmd-Path "git"

if( !$Opam ) { $Opam = $env:OPAM_INSTALL_DIR }
if( !$Opam ) { $Opam = (Get-Item .).FullName + "\opam" }
if( !$OpamRepo ) { $OpamRepo = "$Opam\repo" }

$CygRoot = "$OpamRepo\.cygwin\root"
$WinSysPath = "$env:SystemRoot\System32"
$Neko = Cmd-Path "neko"
$RegPath = "HKCU:\Environment"
$MbedVer = "2.16.3"
$MbedTLS = "https://github.com/Simn/mingw64-mbedtls/releases/download/$MbedVer/mingw64-x86_64-mbedtls-$MbedVer-1.tar.xz"

function Install-Init {

	if( !$Git ) {
		echo "**ERROR** git.exe could not be found in PATH"
		Exit
	}

	if( !$Neko ) {
		echo "**ERROR** Neko.exe could not be found in PATH"
		Exit
	}

	# reset PATH to prevent conflicting cygwin or existing install
	Set-Item -Path env:PATH -Value "$CygRoot\usr\x86_64-w64-mingw32\bin;$CygRoot\bin;$Opam;$Neko;$Git;$WinSysPath"

	# set OPAM root dir
	Set-Item -Path env:OPAMROOT -Value "$OpamRepo"
}

function Install-Opam {
	# download opam binary
	Invoke-Expression "& { $(Invoke-RestMethod https://opam.ocaml.org/install.ps1)} -OpamBinDir $Opam"

	# init opam, assume that we have windows GIT installed
	Invoke-Expression "opam init --cygwin-internal-install --no-git-location --shell=powershell --shell-setup"
}

function Install-Haxe-Deps {
	Invoke-Expression "opam install . --deps-only --confirm-level=yes"

	# install mbedtls mingw package
	$tmpFile = "./mbed.tgz"
	Invoke-Expression "curl $MbedTLS -o $tmpFile"
	Invoke-Expression "tar -C / -xvf $tmpFile"
	Remove-Item "$tmpFile"

	# install lsp server
	Invoke-Expression "opam install ocaml-lsp-server --confirm-level=yes"
}

function Add-Path($NewPath) {
	$CurrentPath = (Get-ItemProperty -Path $RegPath -Name Path).Path
	if ($CurrentPath -notlike "*$NewPath*") {
		$CurrentPath = "$NewPath;$CurrentPath"
		Set-ItemProperty -Path $RegPath -Name Path -Value $CurrentPath
	}
}

function Setup-Paths {
	Add-Path "$OpamRepo\default\bin"
	Add-Path "$CygRoot\bin"
	Add-Path "$CygRoot\usr\x86_64-w64-mingw32\bin"
	Add-Path "$CygRoot\usr\x86_64-w64-mingw32\sys-root\mingw\bin"
	Set-ItemProperty -Path $RegPath -Name OPAMROOT -Value $OpamRepo

	# refresh for all processes (no need to restart)
	$signature = @"
[DllImport("user32.dll", CharSet = CharSet.Auto)]
public static extern int SendMessageTimeout(IntPtr hWnd, int Msg, IntPtr wParam, string lParam, int fuFlags, int uTimeout, out IntPtr lpdwResult);
"@
	$SendMessageTimeout = Add-Type -MemberDefinition $signature -Name "Win32SendMessageTimeout" -Namespace Win32Functions -PassThru
	$SendMessageTimeout::SendMessageTimeout([IntPtr]0xFFFF, 0x1A, [IntPtr]::Zero, "Environment", 2, 5000, [ref][IntPtr]::Zero)
}

Install-Init
Install-Opam
Install-Haxe-Deps
Setup-Paths
