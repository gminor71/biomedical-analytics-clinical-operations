from __future__ import annotations

import argparse
import re
import yaml
from pathlib import Path


def load_text(file_path: Path) -> str:
    with open(file_path, "r", encoding="utf-8") as f:
        return f.read()


def save_yaml(data: dict, file_path: Path) -> None:
    file_path.parent.mkdir(parents=True, exist_ok=True)
    with open(file_path, "w", encoding="utf-8") as f:
        yaml.dump(data, f, sort_keys=False)


def resolve_input_path(input_value: str, base_dir: Path) -> Path:
    candidate = Path(input_value)

    if candidate.is_absolute():
        return candidate

    if candidate.exists():
        return candidate.resolve()

    return (base_dir / input_value).resolve()


def slugify(value: str) -> str:
    value = value.strip().lower()
    value = re.sub(r"[^a-z0-9]+", "_", value)
    value = re.sub(r"_+", "_", value).strip("_")
    return value or "item"


def extract_line_value(text: str, label: str) -> str:
    pattern = rf"^{re.escape(label)}:\s*(.+)$"
    match = re.search(pattern, text, flags=re.MULTILINE)
    return match.group(1).strip() if match else ""


def extract_section(text: str, section_name: str) -> str:
    pattern = rf"^##\s+{re.escape(section_name)}\s*$([\s\S]*?)(?=^##\s+|\Z)"
    match = re.search(pattern, text, flags=re.MULTILINE)
    return match.group(1).strip() if match else ""


def extract_numbered_list(section_text: str) -> list[str]:
    findings: list[str] = []
    for line in section_text.splitlines():
        match = re.match(r"^\s*\d+\.\s+(.+)$", line.strip())
        if match:
            findings.append(match.group(1).strip())
    return findings


def normalize_timeline_phase(raw_phase: str) -> str:
    raw = raw_phase.lower()
    if "active monitoring" in raw:
        return "execution"
    if "startup" in raw:
        return "planning"
    if "closeout" in raw:
        return "closeout"
    return "execution"


def map_risk_level_to_impact(risk_text: str) -> str:
    raw = risk_text.lower()
    if "high" in raw:
        return "high"
    if "medium" in raw or "moderate" in raw:
        return "medium"
    return "low"


def infer_primary_function(finding: str) -> str:
    finding_l = finding.lower()

    if "query" in finding_l or "edc" in finding_l or "data entry" in finding_l:
        return "data_management"

    if "adverse event" in finding_l or "visit window" in finding_l or "treatment delay" in finding_l:
        return "clinical_operations"

    return "cross_functional"


def infer_impacts(finding: str, risk_text: str) -> tuple[str, str]:
    finding_l = finding.lower()
    base_impact = map_risk_level_to_impact(risk_text)

    if "aging open queries" in finding_l:
        return "high", "high"
    if "data entry" in finding_l:
        return "medium", "medium"
    if "adverse event narrative" in finding_l:
        return "medium", "medium"
    if "visit window" in finding_l:
        return "medium", "low"
    if "treatment delay rationale" in finding_l:
        return "medium", "medium"

    return base_impact, base_impact


def infer_description(finding: str, full_text: str) -> str:
    finding_l = finding.lower()

    if "data entry" in finding_l:
        return (
            "Data entry into the EDC system continues to occur several days after visit "
            "completion, with delays of approximately 4–6 days observed for recent visits."
        )

    if "adverse event narrative" in finding_l:
        return (
            "Adverse event documentation has improved, but recurring clarification is still "
            "needed to ensure complete and clinically relevant narrative detail."
        )

    if "aging open queries" in finding_l:
        return (
            "Several queries from the previous monitoring cycle remain unresolved beyond "
            "expected turnaround timelines, indicating a developing pattern in query resolution timeliness."
        )

    if "visit window" in finding_l:
        return (
            "An additional visit outside the protocol-defined window was identified. "
            "The deviation remains minor but recurrence warrants continued monitoring."
        )

    if "treatment delay rationale" in finding_l:
        return (
            "A treatment delay associated with laboratory review was documented, but the "
            "rationale recorded in the EDC requires clarification for consistency with source documentation."
        )

    return finding


def build_item_title(site_id: str, visit_id: str, finding: str) -> str:
    return f"{site_id} / {visit_id} / {finding}"


def transform_mvr_to_pm02_inputs(text: str, project_name: str) -> list[dict]:
    site_id = extract_line_value(text, "Site")
    visit_id = extract_line_value(text, "Visit")
    timeline_phase_raw = extract_line_value(text, "Timeline Phase")

    findings_section = extract_section(text, "Findings")
    risk_section = extract_section(text, "Risk Level")

    findings = extract_numbered_list(findings_section)
    project_phase = normalize_timeline_phase(timeline_phase_raw)

    pm02_inputs: list[dict] = []

    for finding in findings:
        timeline_impact, deliverable_impact = infer_impacts(finding, risk_section)

        pm02_inputs.append(
            {
                "project_name": project_name,
                "item_title": build_item_title(site_id, visit_id, finding),
                "item_description": infer_description(finding, text),
                "project_phase": project_phase,
                "functional_area": "cross_functional",
                "regulatory_environment": "high",
                "timeline_impact": timeline_impact,
                "deliverable_impact": deliverable_impact,
                "issue_is_current": True,
                "decision_needed": False,
                "vendor_related": False,
                "primary_function": infer_primary_function(finding),
            }
        )

    return pm02_inputs


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate PM02 input YAML files from a Monitoring Visit Report markdown/text file."
    )
    parser.add_argument(
        "--input",
        required=True,
        help="Path to the MVR markdown/text file.",
    )
    parser.add_argument(
        "--project-name",
        default="Clinical Operations Simulation",
        help="Project name to apply to generated PM02 inputs.",
    )
    parser.add_argument(
        "--output-dir",
        default="data/processed/generated_from_mvr",
        help="Output folder for generated PM02 input YAML files.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    base_dir = Path(__file__).resolve().parent
    input_path = resolve_input_path(args.input, base_dir)

    if not input_path.exists():
        raise FileNotFoundError(f"MVR input file not found: {input_path}")

    output_dir_candidate = Path(args.output_dir)
    if output_dir_candidate.is_absolute():
        output_dir = output_dir_candidate
    else:
        output_dir = (base_dir / output_dir_candidate).resolve()

    text = load_text(input_path)
    pm02_inputs = transform_mvr_to_pm02_inputs(text, args.project_name)

    if not pm02_inputs:
        raise ValueError("No findings were extracted from the MVR.")

    for item in pm02_inputs:
        file_name = f"{slugify(item['item_title'])}.yaml"
        save_yaml(item, output_dir / file_name)

    print(f"Generated {len(pm02_inputs)} PM02 input files in: {output_dir}")


if __name__ == "__main__":
    main()