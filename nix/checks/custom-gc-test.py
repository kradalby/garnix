import json
import os
import subprocess
import sys
import tempfile
import unittest

scripts = sys.argv[1:]
sys.argv = sys.argv[:1]


def usage(percent=91, used=91000, inode_percent=30, inodes=300):
    return {"pcent": percent, "used": used, "avail": 100000-used,
            "ipcent": inode_percent, "iused": inodes}


class GarbageCollectionTest(unittest.TestCase):
    def run_gc(self, script, steps, expected_status, expected_collections, **extra):
        with tempfile.TemporaryDirectory() as directory:
            path = os.path.join(directory, "state.json")
            state = {"steps": steps, "collections": 0, "generation_deletions": 0, "collectors": [], **extra}
            with open(path, "w") as destination:
                json.dump(state, destination)
            result = subprocess.run([script], env={**os.environ, "GC_TEST_STATE": path},
                                    capture_output=True, text=True, timeout=5)
            self.assertEqual(result.returncode, expected_status, result.stdout + result.stderr)
            with open(path) as source:
                state = json.load(source)
            self.assertEqual(state["collections"], expected_collections)
            self.assertEqual(state["generation_deletions"], int(expected_collections > 0))
            return result, state

    def test_rooted_store_stops_after_one_collection(self):
        for script in scripts:
            with self.subTest(script=script):
                result, _ = self.run_gc(script, [usage()], 1, 1)
                self.assertIn("no progress", result.stderr)

    def test_small_progress_is_not_hidden_by_rounded_percentages(self):
        for script in scripts:
            with self.subTest(script=script):
                self.run_gc(script, [usage(), usage(used=90500), usage(percent=89, used=89000)], 0, 2)

    def test_inode_only_pressure_collects(self):
        for script in scripts:
            with self.subTest(script=script):
                self.run_gc(script, [usage(70, 70000, 91, 910), usage(70, 70000, 89, 890)], 0, 1)

    def test_continuous_pressure_has_an_attempt_limit(self):
        for script in scripts:
            with self.subTest(script=script):
                result, _ = self.run_gc(script, [usage(used=91000 - i * 100) for i in range(5)], 1, 3)
                self.assertIn("after 3 attempts", result.stderr)

    def test_collector_failure_is_propagated(self):
        for script in scripts:
            with self.subTest(script=script):
                self.run_gc(script, [usage()], 42, 1, collector_status=42)

    def test_under_threshold_leaves_generations_untouched(self):
        for script in scripts:
            with self.subTest(script=script):
                self.run_gc(script, [usage(70, 70000)], 0, 0)

    def test_zfs_inode_counts_do_not_trigger_collection(self):
        for script in scripts:
            with self.subTest(script=script):
                self.run_gc(script, [usage(70, 70000, 100, 999)], 0, 0, zfs=True)


unittest.main()
