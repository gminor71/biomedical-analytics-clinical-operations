from __future__ import annotations

import sys
from pathlib import Path

# Add clinical-trials root to Python path
sys.path.append(str(Path(__file__).resolve().parents[2]))

from pathlib import Path

from src.agent.controller import run_agent
from src.io.load_inputs import load_case
from src.io.save_outputs import save_json_log, save_markdown_report
from src.reporting.batch_summary import build_batch_summary
from src.reporting.report_builder import build_markdown_report


def main() -> None:
    """
    Main entry point for batch execution of the CT06 SAP Outline Agent.
    """

    # --- Step 1: Define input/output locations ---
    input_dir = Path("data/processed")
    report_dir = Path("results/reports")
    log_dir = Path("results/logs")
    summary_path = Path("results/summary/batch_summary.md")

    # --- Step 2: Find YAML files ---
    input_files = sorted(input_dir.glob("*.yaml"))

    if not input_files:
        print("\nNo YAML input files found in data/processed/\n")
        return

    results: list[dict] = []

    # --- Step 3: Process each case ---
    for input_path in input_files:
        case = load_case(input_path)
        result = run_agent(case)
        results.append(result)

        analysis_id = result.get("analysis_id", input_path.stem)

        report_path = report_dir / f"{analysis_id}_report.md"
        log_path = log_dir / f"{analysis_id}_run.json"

        report_text = build_markdown_report(result)

        save_markdown_report(report_text, report_path)
        save_json_log(result, log_path)

    # --- Step 4: Build and save batch summary ---
    summary_text = build_batch_summary(results)
    save_markdown_report(summary_text, summary_path)

    # --- Step 5: Print confirmation ---
    print("\n=== CT06 SAP Outline Agent Batch Run ===")
    print(f"Processed cases: {len(results)}")
    print(f"Reports saved to: {report_dir}")
    print(f"Logs saved to: {log_dir}")
    print(f"Batch summary saved to: {summary_path}")
    print("========================================\n")


if __name__ == "__main__":
    main()