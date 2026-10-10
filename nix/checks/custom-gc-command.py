import json
import os
import sys

state_file = os.environ["GC_TEST_STATE"]
with open(state_file) as source:
    state = json.load(source)
command = os.path.basename(sys.argv[0])
step = state["steps"][min(state["collections"], len(state["steps"]) - 1)]
status = 0
if command == "df":
    field = next(arg.split("=", 1)[1] for arg in sys.argv if arg.startswith("--output="))
    print(field)
    print(step[field])
elif command == "mount":
    if state.get("zfs"):
        print("pool on /nix type zfs (rw)")
elif command == "pgrep":
    status = 1
elif command == "nix-env":
    state["generation_deletions"] += 1
elif command in ("nix-collect-garbage", "nix-heuristic-gc"):
    state["collections"] += 1
    state["collectors"].append(command)
    status = state.get("collector_status", 0)
else:
    raise AssertionError(command)
with open(state_file, "w") as destination:
    json.dump(state, destination)
sys.exit(status)
