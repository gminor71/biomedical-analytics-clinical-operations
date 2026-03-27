from __future__ import annotations

from pathlib import Path

from src.io.load_inputs import load_case
from src.agent.controller import run_agent
from src.reporting.report_builder import build_markdown_report
from src.reporting.batch_summary import build_batch_summary
from src.io.save_outputs import save_markdown_report, save_json_log


def main() -> None:
    input_dir = Path("data/processed")
    yaml_files = list(input_dir.glob("*.yaml"))

    if not yaml_files:
        print("No YAML files found.")
        return

    all_results = []

    print(f"\nFound {len(yaml_files)} case(s)\n")

    for file_path in yaml_files:
        print(f"Processing: {file_path.name}")

        try:
            case = load_case(file_path)
            result = run_agent(case)
            all_results.append(result)

            report_text = build_markdown_report(result)

            analysis_id = result.get("analysis_id", "unknown")
            report_path = Path(f"results/reports/{analysis_id}_report.md")
            log_path = Path(f"results/logs/{analysis_id}_run.json")

            save_markdown_report(report_text, report_path)
            save_json_log(result, log_path)

        except Exception as e:
            print(f"Error processing {file_path.name}: {e}")

    summary_text = build_batch_summary(all_results)
    summary_path = Path("results/summary/batch_summary.md")
    save_markdown_report(summary_text, summary_path)

    print("\nBatch processing complete.")
    print(f"Batch summary saved to: {summary_path}\n")


if __name__ == "__main__":
    main()