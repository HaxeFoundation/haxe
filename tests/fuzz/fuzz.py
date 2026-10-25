#!/usr/bin/env python3
"""
Warm/cold fuzzing of the compilation server.

Random edits are applied to the sources of a project; after each edit, a
compilation through the server (warm) is compared with a compilation from
scratch (cold): exit code, compiler messages and output file must be the same.
The cold compilations vary the typing order (-D typer-shuffle) and the number
of domains typing in parallel (-D parallel-typing), so that a dependence on
the scheduling also shows up as a difference. The edits accumulate; an edit
that does not compile is undone.

    python3 tests/fuzz/fuzz.py --dir <project> --hxml build.hxml --out out/main.js \
        --src src,lib --edits 50 --seed 1 --port 7000

The project directory must be a git checkout (it is reset to HEAD at the start
and at the end). Differences are printed as DIFF lines, with the warm and cold
compiler outputs saved next to the project (<dir>.fuzz-warm.txt, <dir>.fuzz-cold.txt).
"""
import argparse, json, os, random, re, subprocess, sys, time

ap = argparse.ArgumentParser()
ap.add_argument("--dir", required=True, help="project directory (a git checkout)")
ap.add_argument("--hxml", default="build.hxml")
ap.add_argument("--out", required=True, help="output file, relative to the project directory")
ap.add_argument("--src", required=True, help="comma separated source directories to edit")
ap.add_argument("--exclude", default="Macro", help="comma separated substrings of files not to edit")
ap.add_argument("--edits", type=int, default=50)
ap.add_argument("--seed", type=int, default=1)
ap.add_argument("--port", default="7000")
ap.add_argument("--haxe", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "haxe"))
args = ap.parse_args()

H = os.path.abspath(args.haxe)
D = os.path.abspath(args.dir)
random.seed(args.seed)
DOMAINS = ["0", "1", "2", "4", "8", "16"]

subprocess.run(["git", "-C", D, "checkout", "-q", "--force", "HEAD"], check=True)
# the server keeps one configuration (a define is part of the cache signature)
warm_domains = random.choice(DOMAINS)
warm_args = ["-D", "parallel-typing=" + warm_domains]
server = subprocess.Popen([H, "--wait", args.port], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
# wait until the server answers (the port of a previous run may still be bound)
for _ in range(100):
	if subprocess.run([H, "--connect", args.port, "--version"], capture_output=True).returncode == 0: break
	if server.poll() is not None:
		time.sleep(0.5)
		server = subprocess.Popen([H, "--wait", args.port], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
	time.sleep(0.1)
else:
	server.kill()
	sys.exit("the compilation server does not answer on port " + args.port)
excluded = [e for e in args.exclude.split(",") if e]
files = [os.path.join(r, f) for d in args.src.split(",") for r, _, fs in os.walk(os.path.join(D, d)) for f in fs
	if f.endswith(".hx") and not any(e in os.path.join(r, f) for e in excluded)]

def run(cmd):
	t0 = time.time()
	p = subprocess.run([H] + cmd, cwd=D, capture_output=True, text=True)
	return p.returncode, p.stdout + p.stderr, time.time() - t0

def read_out():
	try:
		return open(os.path.join(D, args.out), "rb").read()
	except OSError:
		return None

def retyped():
	request = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "server/retyped", "params": {}})
	p = subprocess.run([H, "--connect", args.port, "--display", request], capture_output=True, text=True)
	try: return len(json.loads(p.stderr)["result"]["result"])
	except Exception: return -1

last = None

def mutate():
	global last
	f = random.choice(files)
	src = open(f).read()
	lines = src.split("\n")
	kind = random.choice(["comment", "literal", "field", "infer", "literal", "comment"])
	i = 0
	if kind == "comment":
		i = random.randrange(len(lines))
		lines.insert(i, "// fuzz")
	elif kind == "literal":
		idx = [i for i, l in enumerate(lines) if re.search(r"[^\w.]\d+[^\w.x]", l) and "case" not in l and "@" not in l and "var" not in l]
		if not idx: return None
		i = random.choice(idx)
		lines[i] = re.sub(r"([^\w.])(\d+)([^\w.x])", lambda m: m.group(1) + str(int(m.group(2)) + 1) + m.group(3), lines[i], count=1)
	elif kind == "field":
		idx = [i for i, l in enumerate(lines) if re.match(r"\s*(public |private |static |override |inline )*function ", l)]
		if not idx: return None
		i = random.choice(idx)
		lines.insert(i, "\tpublic var fuzzField%d : Int = 0;" % random.randrange(1000))
	elif kind == "infer":
		# a function returning a literal without a declared type: change the literal
		idx = [i for i, l in enumerate(lines) if re.search(r"function \w+\([^)]*\)\s*\{?\s*return \d+", l)]
		if not idx: return None
		i = random.choice(idx)
		lines[i] = re.sub(r"return (\d+)", lambda m: "return " + str(int(m.group(1)) + 1), lines[i], count=1)
	open(f, "w").write("\n".join(lines))
	last = (f, src)
	return kind + " " + os.path.relpath(f, D) + ":" + str(i + 1)

def compare(label):
	rc, out, t = run(["--connect", args.port, "--cwd", D, args.hxml] + warm_args)
	warm_file = read_out()
	nr = retyped()
	cold_args = ["-D", "parallel-typing=" + random.choice(DOMAINS)]
	if random.random() < 0.5:
		cold_args += ["-D", "typer-shuffle=%d" % random.randrange(1 << 30)]
	crc, cout, _ = run([args.hxml] + cold_args)
	cold_file = read_out()
	same = rc == crc and out == cout and (rc != 0 or warm_file == cold_file)
	# the server does not write an output that it believes unchanged: put its
	# own output back
	if warm_file is not None:
		open(os.path.join(D, args.out), "wb").write(warm_file)
	print("%-5s rc=%d %.2fs retyped=%-4d %-60s cold: %s" % ("SAME" if same else "DIFF", rc, t, nr, label, " ".join(cold_args)), flush=True)
	if not same:
		open(D + ".fuzz-warm.txt", "w").write(out)
		open(D + ".fuzz-cold.txt", "w").write(cout)
	return same, rc

print("server: -D parallel-typing=%s" % warm_domains, flush=True)
fails = 0
try:
	compare("initial")
	k = 0
	while k < args.edits:
		# modification times have a precision of one second
		time.sleep(1.05)
		m = mutate()
		if m is None: continue
		k += 1
		same, rc = compare(m)
		if not same: fails += 1
		if rc != 0:
			# back to a compiling state
			time.sleep(1.05)
			open(last[0], "w").write(last[1])
			same, rc = compare("undo")
			if not same: fails += 1
finally:
	server.kill()
	server.wait()
	subprocess.run(["git", "-C", D, "checkout", "-q", "--force", "HEAD"])
print("fails", fails)
sys.exit(1 if fails else 0)
