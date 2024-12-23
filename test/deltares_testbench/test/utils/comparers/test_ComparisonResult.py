import pytest

from src.utils.comparers.comparison_result import ComparisonResult


@pytest.mark.parametrize(("error", "expected_result"), [(True, "ERROR"), (False, "NOK")])
def test_comparison_result_init(error: bool, expected_result: str) -> None:
    result = ComparisonResult(error=error)
    assert result.result == expected_result


@pytest.mark.parametrize(
    (
        "max_abs_diff",
        "max_rel_diff",
        "max_abs_diff_tolerance",
        "max_rel_diff_tolerance",
        "expected_passed",
        "expected_result",
    ),
    [(0.5, 0.5, 1.0, 0.5, True, "OK"), (1.5, 1.5, 1.0, 1.0, False, "NOK"), (0.0, 0.0, 1.0, 1.0, True, "OK")],
)
def test_is_tolerance_exceeded(
    max_abs_diff: float,
    max_rel_diff: float,
    max_abs_diff_tolerance: float,
    max_rel_diff_tolerance: float,
    expected_passed: bool,
    expected_result: str,
) -> None:
    result = ComparisonResult()
    result.max_abs_diff = max_abs_diff
    result.max_rel_diff = max_rel_diff
    result.is_tolerance_exceeded(maxAbsDiffTolerance=max_abs_diff_tolerance, maxRelDiffTolerance=max_rel_diff_tolerance)
    assert result.passed == expected_passed
    assert result.result == expected_result
