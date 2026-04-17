from __future__ import annotations

import sys
from pathlib import Path

# Add clinical-trials root to Python path
sys.path.append(str(Path(__file__).resolve().parents[2]))

from pathlib import Path

from src.io.load_inputs import load_case
from src.agent.controller import run_agent
from src.reporting.report_builder import build_markdown_report
from src.io.save_outputs import save_markdown_report, save_json_log


def main() -> None:
    """
    Main entry point for the CT05 TTE Method Agent.
    """

    # --- Step 1: Define input path ---
    input_path = Path("data/processed/tte_case_002.yaml")

    # --- Step 2: Load case ---
    case = load_case(input_path)

    # --- Step 3: Run agent ---
    result = run_agent(case)

    # --- Step 4: Build report ---
    report_text = build_markdown_report(result)

    # --- Step 5: Define output paths ---
    analysis_id = result.get("analysis_id", "unknown")

    report_path = Path(f"results/reports/{analysis_id}_report.md")
    log_path = Path(f"results/logs/{analysis_id}_run.json")

    # --- Step 6: Save outputs ---
    save_markdown_report(report_text, report_path)
    save_json_log(result, log_path)

    # --- Step 7: Print confirmation ---
    print("\n=== CT05 TTE Method Agent ===")
    print(f"Input file: {input_path}")
    print(f"Report saved to: {report_path}")
    print(f"Run log saved to: {log_path}")
    print("Status:", result.get("status"))
    print("================================\n")


if __name__ == "__main__":
    main()