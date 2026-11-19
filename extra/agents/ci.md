# Investigating CI Failures

CI runs on GitHub Actions (see `.github/workflows/`). All targets are tested, on Windows, Linux and macOS, on both x86_64 and ARM64.

The steps below use the `gh` CLI. The checkout may have many remotes (contributors' forks); if `gh` does not resolve the upstream repository, pass `--repo HaxeFoundation/haxe`. If you have GitHub MCP tools instead, the equivalents are `pull_request_read`, `list_workflow_runs`, `get_workflow_run`, `list_workflow_jobs` and `get_job_logs` (with `failed_only: true`).

1. **Find the correct workflow run for the current PR commit**:
   - Get the PR's head commit: `gh pr view <pr> --json headRefOid,headRefName`
   - List recent runs for the branch: `gh run list --branch <branch>`
   - A run belongs to the right commit when its head SHA matches the PR's head commit
   - **Never assume a run's status from memory or a previous check - always query its live status** (`gh run view <run-id>`)

2. **Understand `status` vs `conclusion` - these are different fields**:
   - `status: in_progress` → the run is **actively running right now**
   - `status: completed` → the run is **finished**; check `conclusion` for the outcome
   - `conclusion: failure` → the run **failed** (it is NOT in progress, even if you previously saw it running)
   - `conclusion: success` → the run passed
   - `conclusion: cancelled` → the run was cancelled (e.g., a newer push superseded it)
   - **Do not describe a `completed` run as "in progress"**

3. Identify which jobs failed: `gh run view <run-id>` (look for jobs whose conclusion is not success)

4. Get the failing logs per job: `gh run view --job <job-id> --log-failed` (the job IDs are listed by `gh run view <run-id>`)
   - Avoid `gh run view <run-id> --log-failed` for whole runs: it downloads the logs of all jobs and is very slow
   - Logs end with post-step cleanup output; look at roughly the last 100-200 lines of the failing step rather than the very end
   - Look for keywords like `FAILED`, `error`, `exit code`, `assert`, `exception` to locate the relevant failure
   - Logs can be too large to read in one go: fetch them with a bounded tail (e.g. `tail_lines` for `get_job_logs`) or save them to a file, then search them instead of reading them in full

5. The job name contains the target and platform, e.g. `linux-test (macro, x86)` means the macro target on Linux x86

To reproduce a failure locally, `tests/runci/targets/*.hx` has the exact commands CI runs for each target and suite. For intermittent native crashes, see `extra/debugging-native-crashes.md`.
